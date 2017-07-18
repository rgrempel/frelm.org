{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Worker where

import Control.Monad.Logger (LoggingT, logError, runStdoutLoggingT)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.Aeson (eitherDecodeStrict)
import Data.ElmPackage
import Data.List (nub)
import Data.Map (traverseWithKey)
import Data.OfficialPackage
import Data.SemVer (Version, fromText)
import Data.Time.ISO8601
import Data.Yaml.Config (loadYamlSettings, useEnv)
import Database.Esqueleto
import Database.Migrate
import qualified Database.Persist as P
import Database.Persist.Postgresql
       (createPostgresqlPool, pgConnStr, pgPoolSize)
import GHC.IO.Exception (ExitCode(..))
import Import.Worker hiding ((<>), isNothing, on)
import LoadEnv (loadEnv, loadEnvFrom)
import Options.Applicative
import System.Directory (doesFileExist)
import System.Exit (die)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process
       (CreateProcess, env, proc, readCreateProcessWithExitCode,
        readProcessWithExitCode)
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
    | RecheckRepo Int64
    | RecheckTags
    | ReparsePackages
    | ReparseAllPackages
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
            "recheck-tags"
            (info recheckTagsOptions $ fullDesc <> progDesc "Recheck tags") <>
        command
            "recheck-repo"
            (info recheckRepoOptions $
             fullDesc <> progDesc "Recheck a repo with the specified ID.") <>
        command "reparse" (info reparseOptions $ fullDesc <> progDesc "Reparse") <>
        command
            "reparse-all"
            (info reparseAllOptions $ fullDesc <> progDesc "Reparse All") <>
        command
            "scrape"
            (info scrapeOptions $ fullDesc <> progDesc "Scrape packages")
    addRepoOptions = fmap AddRepo $ argument str $ metavar "REPOSITORY"
    recheckRepoOptions = fmap RecheckRepo $ argument auto $ metavar "REPO_ID"
    migrateOptions = pure RunMigrations
    crawlOptions = pure Crawl
    recheckTagsOptions = pure RecheckTags
    reparseOptions = pure ReparsePackages
    reparseAllOptions = pure ReparseAllPackages
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
        RecheckRepo repoId -> recheckRepo (toSqlKey repoId)
        RecheckTags -> recheckTags
        RunMigrations -> do
            dbconf <- asks (workerDatabaseConf . workerSettings)
            result <- liftIO $ migrateSchema $ pgConnStr dbconf
            case result of
                MigrationSuccess -> pure ()
                MigrationError err -> liftIO $ die err
        Crawl -> crawl
        ReparsePackages -> reparsePackages
        ReparseAllPackages -> reparseAllPackages
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

reparsePackages :: WorkerT ()
reparsePackages =
    void $ runWorkerDB $ decodedPackageIsNull >>= traverse decodePackageJSON

reparseAllPackages :: WorkerT ()
reparseAllPackages =
    void $ runWorkerDB $ allPackageChecks >>= traverse decodePackageJSON

checkTags :: WorkerT ()
checkTags = void $ runWorkerDB $ neverCheckedForTags >>= traverse checkRepoTags

allPackageChecks :: WorkerDB [Entity PackageCheck]
allPackageChecks = select $ from pure

decodedPackageIsNull :: WorkerDB [Entity PackageCheck]
decodedPackageIsNull =
    select $
    from $ \(repoVersion `InnerJoin` packageCheck `LeftOuterJoin` package) -> do
        on $
            just (repoVersion ^. RepoVersionId) ==. package ?.
            PackageRepoVersion
        on $
            repoVersion ^. RepoVersionId ==. packageCheck ^.
            PackageCheckRepoVersion
        where_ $ isNothing (package ?. PackageRepoVersion)
        pure packageCheck

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
    knownVersions <- fetchKnownVersions (entityKey repo)
    fetchedVersions <- fetchGitTags repo
    let newVersions = filter (`notElem` knownVersions) fetchedVersions
    unless (null newVersions) $
        withSystemTempDirectory "git-clone" $ \basedir -> do
            gitdir <- cloneGitRepo repo basedir
            forM_ gitdir $ \dir ->
                forM_ newVersions $ checkNewTag (entityKey repo) dir
    transactionSave

recheckRepo :: RepoId -> WorkerT ()
recheckRepo repoId =
    runWorkerDB $ do
        repo <- getJustEntity repoId
        fetchedVersions <- fetchGitTags repo
        unless (null fetchedVersions) $
            withSystemTempDirectory "git-clone" $ \basedir -> do
                gitdir <- cloneGitRepo repo basedir
                forM_ gitdir $ \dir ->
                    forM_ fetchedVersions $ checkNewTag (entityKey repo) dir

recheckTags :: WorkerT ()
recheckTags =
    runWorkerDB $ do
        repos <- select $ from pure
        forM_ repos $ \repo -> do
            fetchedVersions <- fetchGitTags repo
            unless (null fetchedVersions) $
                withSystemTempDirectory "git-clone" $ \basedir -> do
                    gitdir <- cloneGitRepo repo basedir
                    forM_ gitdir $ \dir ->
                        forM_ fetchedVersions $ checkNewTag (entityKey repo) dir
            transactionSave

fetchKnownVersions :: RepoId -> WorkerDB [GitTag]
fetchKnownVersions repoId =
    fmap (fmap (toGitTag . entityVal)) $
    select $
    from $ \version -> do
        where_ (version ^. RepoVersionRepo ==. val repoId)
        pure version

fetchTagsProcess :: Entity Repo -> CreateProcess
fetchTagsProcess repo = process {env = Just [("GIT_TERMINAL_PROMPT", "0")]}
  where
    process =
        proc
            "git"
            [ "ls-remote"
            , "--tags"
            , "--quiet"
            , "--refs"
            , unpack $ (repoGitUrl . entityVal) repo
            ]

fetchGitTags :: Entity Repo -> WorkerDB [GitTag]
fetchGitTags repo = do
    (exitCode, out, err) <-
        liftIO $ readCreateProcessWithExitCode (fetchTagsProcess repo) ""
    ran <- liftIO getCurrentTime
    insert_
        TagCheck
        { tagCheckStdout = pack out
        , tagCheckStderr = pack err
        , tagCheckExitCode = exitCode
        , tagCheckRan = ran
        , tagCheckRepo = entityKey repo
        }
    pure $ (rights . fmap (parse parseTag "line") . lines) out

data GitTag = GitTag
    { gitTagSha :: Text
    , gitTagTag :: Text
    , gitTagVersion :: Version
    } deriving (Eq)

toGitTag :: RepoVersion -> GitTag
toGitTag r =
    GitTag
    { gitTagSha = repoVersionSha r
    , gitTagTag = repoVersionTag r
    , gitTagVersion = repoVersionVersion r
    }

parseTag :: Parsec String () GitTag
parseTag = do
    gitTagSha <- pack <$> Parsec.many Parsec.hexDigit
    void tab
    gitTagTag <- fmap pack $ string "refs/tags/" *> Parsec.many anyChar <* eof
    case fromText gitTagTag of
        Right gitTagVersion -> pure GitTag {..}
        Left err -> unexpected err

checkNewTag :: Key Repo -> FilePath -> GitTag -> WorkerDB ()
checkNewTag repoId gitDir gitTag = do
    result <- checkoutGitRepo gitDir (unpack $ gitTagTag gitTag)
    case result of
        Left err -> void $ $(logError) (tshow err)
        Right _ -> do
            committedAt <- tagCommittedAt gitDir (unpack $ gitTagSha gitTag)
            case committedAt of
                Left err2 -> void $ $(logError) (tshow err2)
                Right committed -> do
                    repoVersion <-
                        upsert
                            RepoVersion
                            { repoVersionRepo = repoId
                            , repoVersionVersion = gitTagVersion gitTag
                            , repoVersionSha = gitTagSha gitTag
                            , repoVersionTag = gitTagTag gitTag
                            , repoVersionCommittedAt = committed
                            }
                            [ RepoVersionVersion P.=. gitTagVersion gitTag
                            , RepoVersionSha P.=. gitTagSha gitTag
                            , RepoVersionCommittedAt P.=. committed
                            ]
                    let packageCheckKey =
                            PackageCheckKey $ entityKey repoVersion
                    let elmPackageJson = gitDir </> "elm-package.json"
                    hasElmPackage <- liftIO $ doesFileExist elmPackageJson
                    if hasElmPackage
                        then do
                            contents <- liftIO $ readFile elmPackageJson
                            let packageCheck =
                                    Entity
                                        packageCheckKey
                                        PackageCheck
                                        { packageCheckPackage = Just contents
                                        , packageCheckDecodeError = Nothing
                                        , packageCheckRepoVersion =
                                              entityKey repoVersion
                                        }
                            repsert
                                (entityKey packageCheck)
                                (entityVal packageCheck)
                            decodePackageJSON packageCheck
                        else repsert
                                 packageCheckKey
                                 PackageCheck
                                 { packageCheckPackage = Nothing
                                 , packageCheckDecodeError = Nothing
                                 , packageCheckRepoVersion =
                                       entityKey repoVersion
                                 }

decodePackageJSON :: Entity PackageCheck -> WorkerDB ()
decodePackageJSON pc =
    forM_ ((packageCheckPackage . entityVal) pc) $ \contents ->
        case eitherDecodeStrict (encodeUtf8 contents) of
            Left err ->
                update $ \table -> do
                    set
                        table
                        [PackageCheckDecodeError =. (val . Just . pack) err]
                    where_ $ table ^. PackageCheckId ==. val (entityKey pc)
            Right p -> do
                update $ \table -> do
                    set table [PackageCheckDecodeError =. val Nothing]
                    where_ $ table ^. PackageCheckId ==. val (entityKey pc)
                libraryId <-
                    forM (elmPackageLibraryName p) $ \libraryName ->
                        either entityKey id <$> insertBy Library {..}
                package <-
                    repsert
                        (PackageKey $ packageCheckRepoVersion $ entityVal pc)
                        Package
                        { packageRepoVersion =
                              packageCheckRepoVersion $ entityVal pc
                        , packageVersion = elmPackageVersion p
                        , packageSummary = elmPackageSummary p
                        , packageRepository = elmPackageRepository p
                        , packageLibrary = libraryId
                        , packageLicense = elmPackageLicense p
                        , packageNativeModules = elmPackageNativeModules p
                        , packageElmVersion = elmPackageElmVersion p
                        }
                forM_ (nub $ elmPackageModules p) $ \moduleName -> do
                    moduleId <- either entityKey id <$> insertBy Module {..}
                    void $
                        insertUnique
                            PackageModule
                            { packageModuleRepoVersion =
                                  (packageCheckRepoVersion . entityVal) pc
                            , packageModuleModuleId = moduleId
                            , packageModuleExposed = True
                            }
                void $
                    flip traverseWithKey (elmPackageDependencies p) $ \libraryName dependencyVersion -> do
                        dependencyLibrary <-
                            either entityKey id <$> insertBy Library {..}
                        let dependencyRepoVersion =
                                (packageCheckRepoVersion . entityVal) pc
                        -- We insert the Repo that the dependency represents so that
                        -- we'll check it as well.
                        void $
                            insertBy
                                Repo
                                { repoGitUrl = libraryNameToGitUrl libraryName
                                , repoSubmittedBy = Nothing
                                }
                        void $ insertUnique Dependency {..}

tagCommittedAt ::
       MonadIO m
    => FilePath
    -> String
    -> m (Either (ExitCode, String, String) UTCTime)
tagCommittedAt gitDir sha = do
    (exitCode, out, err) <-
        liftIO $
        readProcessWithExitCode
            "git"
            ["-C", gitDir, "show", "-s", "--format=%cI", sha]
            ""
    pure $
        case exitCode of
            ExitSuccess ->
                case (parseISO8601 . lastString . lines) out of
                    Just utcTime -> Right utcTime
                    Nothing ->
                        Left
                            ( ExitFailure 1
                            , (lastString . lines) out
                            , "parseISO8601 failed")
            ExitFailure _ -> Left (exitCode, out, err)

lastString :: [String] -> String
lastString = go ""
  where
    go acc v =
        case v of
            a:b -> go a b
            [] -> acc

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
                let libraryName = officialPackageName package
                let repoGitUrl = libraryNameToGitUrl libraryName
                let repoSubmittedBy = Nothing
                void $ insertBy Repo {..}
                publishedVersionLibrary <-
                    either entityKey id <$> insertBy Library {..}
                forM_ (officialPackageVersions package) $ \publishedVersionVersion ->
                    void $ insertBy PublishedVersion {..}

libraryNameToGitUrl :: Text -> Text
libraryNameToGitUrl libraryName = "https://github.com/" <> libraryName <> ".git"
