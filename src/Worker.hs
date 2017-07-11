{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Worker where

import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.Aeson (eitherDecodeStrict)
import Data.ElmPackage
import Data.Map (traverseWithKey)
import Data.OfficialPackage
import Data.SemVer (Version, fromText)
import Data.Yaml.Config (loadYamlSettings, useEnv)
import Database.Esqueleto
import Database.Migrate
import Database.Persist.Postgresql
       (createPostgresqlPool, pgConnStr, pgPoolSize)
import GHC.IO.Exception (ExitCode(..))
import Import.Worker hiding ((<>))
import LoadEnv (loadEnv, loadEnvFrom)
import Options.Applicative
import System.Directory (doesFileExist)
import System.Exit (die)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Text.Parsec as Parsec

type WorkerT = ReaderT Worker (ResourceT (LoggingT IO))

type WorkerDB = ReaderT SqlBackend WorkerT

-- | The Foundation datatype for the worker.
data Worker = Worker
    { workerSettings :: WorkerSettings
    , workerConnPool :: ConnectionPool
    , workerCommand :: Command
    }

data Command
    = AddRepo String
    | Crawl
    | RunMigrations
    | Scrape
    deriving (Show)

parseArgs :: ParserInfo Command
parseArgs = info sub fullDesc
  where
    sub =
        subparser $
        command
            "add-repo"
            (info addRepoOptions $
             fullDesc <> progDesc "Add a new repository to the database.") <>
        command
            "migrate"
            (info migrateOptions $
             fullDesc <> progDesc "Run database migrations") <>
        command "crawl" (info crawlOptions $ fullDesc <> progDesc "Crawl") <>
        command
            "scrape"
            (info scrapeOptions $ fullDesc <> progDesc "Scrape packages")
    addRepoOptions = fmap AddRepo $ argument str $ metavar "REPOSITORY"
    migrateOptions = pure RunMigrations
    crawlOptions = pure Crawl
    scrapeOptions = pure Scrape

runWorkerDB :: WorkerDB a -> WorkerT a
runWorkerDB workerDB = asks workerConnPool >>= runSqlPool workerDB

runWorker :: WorkerT ()
runWorker = do
    todo <- asks workerCommand
    case todo of
        AddRepo repo ->
            runWorkerDB $
            insert_ Repo {repoGitUrl = pack repo, repoSubmittedBy = Nothing}
        RunMigrations -> do
            dbconf <- asks (workerDatabaseConf . workerSettings)
            result <- liftIO $ migrateSchema $ pgConnStr dbconf
            case result of
                MigrationSuccess -> pure ()
                MigrationError err -> liftIO $ die err
        Crawl -> crawl
        Scrape -> scrape

-- | The main function for the worker.
workerMain :: IO ()
workerMain = do
    loadEnv
    loadEnvFrom "./.env-worker"
    workerSettings <- loadYamlSettings [] [configSettingsYmlValue] useEnv
    workerCommand <- execParser parseArgs
    -- We short-circuit the validation if we're running the migrations
    schemaValidation <-
        case workerCommand of
            RunMigrations -> pure MigrationSuccess
            _ -> validateSchema $ pgConnStr $ workerDatabaseConf workerSettings
    case schemaValidation of
        MigrationError err -> die err
        MigrationSuccess ->
            runStdoutLoggingT $ do
                workerConnPool <-
                    createPostgresqlPool
                        (pgConnStr $ workerDatabaseConf workerSettings)
                        (pgPoolSize $ workerDatabaseConf workerSettings)
                let worker = Worker {..}
                runResourceT $ runReaderT runWorker worker

crawl :: WorkerT ()
crawl =
    forever $ do
        checkTags
        liftIO $ threadDelay $ 10 * 1000000
    -- Should probably have a "big" exception handler here that
    -- logs unexpected exceptions ... or something ...

checkTags :: WorkerT ()
checkTags = do
    runWorkerDB $ neverCheckedForTags >>= traverse checkRepoTags
    pure ()

neverCheckedForTags :: WorkerDB [Entity Repo]
neverCheckedForTags =
    select $
    from $ \repo -> do
        where_ $
            notExists $
            from $ \check -> where_ (check ^. TagCheckRepo ==. repo ^. RepoId)
        pure repo

checkRepoTags :: Entity Repo -> WorkerDB ()
checkRepoTags repo = do
    knownVersions <-
        fmap (repoVersionVersion . entityVal) <$>
        fetchKnownVersions (entityKey repo)
    fetchedVersions <- fetchGitTags repo
    let newVersions =
            filter (\a -> fst a `notElem` knownVersions) fetchedVersions
    unless (null newVersions) $
        withSystemTempDirectory "git-clone" $ \basedir -> do
            gitdir <- cloneGitRepo repo basedir
            forM_ gitdir $ \dir ->
                forM_ newVersions $ uncurry $ checkNewTag (entityKey repo) dir
    transactionSave

fetchKnownVersions :: RepoId -> WorkerDB [Entity RepoVersion]
fetchKnownVersions repoId =
    select $
    from $ \version -> do
        where_ (version ^. RepoVersionRepo ==. val repoId)
        pure version

fetchGitTags :: Entity Repo -> WorkerDB [(Version, String)]
fetchGitTags repo = do
    (exitCode, out, err) <-
        liftIO $
        readProcessWithExitCode
            "git"
            [ "ls-remote"
            , "--tags"
            , "--quiet"
            , "--refs"
            , unpack $ (repoGitUrl . entityVal) repo
            ]
            ""
    ran <- liftIO getCurrentTime
    insert_
        TagCheck
        { tagCheckStdout = pack out
        , tagCheckStderr = pack err
        , tagCheckExitCode = exitCode
        , tagCheckRan = ran
        , tagCheckRepo = entityKey repo
        }
    pure $ (rights . fmap (parse parseTagAndVersion "line") . lines) out

parseTagAndVersion :: Parsec String () (Version, String)
parseTagAndVersion = sha40 *> tab *> parseTag
  where
    sha40 = Parsec.count 40 Parsec.hexDigit
    parseTag = do
        tag <- string "refs/tags/" *> Parsec.many anyChar <* eof
        case fromText $ pack tag of
            Right version -> pure (version, tag)
            Left err -> unexpected err

checkNewTag :: Key Repo -> FilePath -> Version -> String -> WorkerDB ()
checkNewTag repoId gitDir version tag = do
    result <- checkoutGitRepo gitDir tag
    -- TODO: Track the error somehow? We don't really expect
    -- errors from checkoutGitRepo
    case result of
        Left _ -> pure ()
        Right _ -> do
            (contents, decodedPackage, decodeError) <-
                do let elmPackageJson = gitDir </> "elm-package.json"
                   hasElmPackage <- liftIO $ doesFileExist elmPackageJson
                   if hasElmPackage
                       then do
                           contents <- liftIO $ pack <$> readFile elmPackageJson
                           case eitherDecodeStrict (encodeUtf8 contents) of
                               Left err ->
                                   pure (Just contents, Nothing, Just err)
                               Right package ->
                                   pure (Just contents, Just package, Nothing)
                       else pure (Nothing, Nothing, Nothing)
            packageId <-
                forM decodedPackage $ \p -> do
                    libraryId <-
                        forM (elmPackageLibraryName p) $ \libraryName ->
                            either entityKey id <$> insertBy Library {..}
                    packageId <-
                        insert
                            Package
                            { packageVersion = elmPackageVersion p
                            , packageSummary = elmPackageSummary p
                            , packageRepository = elmPackageRepository p
                            , packageLibrary = libraryId
                            , packageLicense = elmPackageLicense p
                            , packageElmVersion = elmPackageElmVersion p
                            }
                    forM_ (elmPackageModules p) $ \moduleName -> do
                        moduleId <- either entityKey id <$> insertBy Module {..}
                        insert_
                            PackageModule
                            { packageModulePackageId = packageId
                            , packageModuleModuleId = moduleId
                            , packageModuleExposed = True
                            }
                    void $
                        flip traverseWithKey (elmPackageDependencies p) $ \libraryName dependencyVersion -> do
                            dependencyLibrary <-
                                either entityKey id <$> insertBy Library {..}
                            let dependencyPackage = packageId
                            let repoGitUrl =
                                    "https://github.com/" <> libraryName <>
                                    ".git"
                            let repoSubmittedBy = Nothing
                            dependencyRepo <-
                                either entityKey id <$> insertBy Repo {..}
                            insert_ Dependency {..}
                    pure packageId
            insert_
                RepoVersion
                { repoVersionRepo = repoId
                , repoVersionTag = pack tag
                , repoVersionVersion = version
                , repoVersionPackage = contents
                , repoVersionDecodeError = pack <$> decodeError
                , repoVersionDecoded = packageId
                }

checkoutGitRepo ::
       MonadIO m
    => FilePath
    -> String
    -> m (Either (ExitCode, String, String) ())
checkoutGitRepo gitDir tag = do
    (exitCode, out, err) <-
        liftIO $
        readProcessWithExitCode
            "git"
            ["-C", gitDir, "checkout", "--quiet", "--detach", tag]
            ""
    if exitCode == ExitSuccess
        then pure $ Right ()
        else pure $ Left (exitCode, out, err)

cloneGitRepo :: Entity Repo -> FilePath -> WorkerDB (Maybe FilePath)
cloneGitRepo repo toPath = do
    (exitCode, out, err) <-
        liftIO $
        readProcessWithExitCode
            "git"
            [ "-C"
            , toPath
            , "clone"
            , "--depth"
            , "1"
            , "--quiet"
            , "--no-single-branch"
            , unpack $ (repoGitUrl . entityVal) repo
            , "git-clone"
            ]
            ""
    if exitCode == ExitSuccess
        then (pure . Just) (toPath </> "git-clone")
        else do
            ran <- liftIO getCurrentTime
            insert_
                CloneError
                { cloneErrorExitCode = exitCode
                , cloneErrorStdout = pack out
                , cloneErrorStderr = pack err
                , cloneErrorRepo = entityKey repo
                , cloneErrorRan = ran
                }
            pure Nothing

scrape :: WorkerT ()
scrape = do
    result <- fetchOfficialPackages
    case result of
        Left err -> print err
        Right packages ->
            runWorkerDB $
            forM_ packages $ \package -> do
                let repoGitUrl =
                        "https://github.com/" <> officialPackageName package <>
                        ".git"
                let repoSubmittedBy = Nothing
                void $ insertBy Repo {..}
