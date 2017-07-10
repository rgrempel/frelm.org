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
import Data.SemVer (Version, fromText, toText)
import Data.Yaml.Config (loadYamlSettings, useEnv)
import Database.Migrate
import Database.Persist.Postgresql
import GHC.IO.Exception (ExitCode)
import Import.Worker hiding ((<>))
import LoadEnv (loadEnv, loadEnvFrom)
import Options.Applicative
import System.Exit (die)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Text.Parsec as Parsec

type WorkerT = ReaderT Worker (ResourceT (LoggingT IO))

-- | The Foundation datatype for the worker.
data Worker = Worker
    { workerSettings :: WorkerSettings
    , workerConnPool :: ConnectionPool
    , workerCommand :: Command
    }

data Command
    = AddRepo String
    | RunMigrations
    deriving (Show)

parseArgs :: ParserInfo Command
parseArgs = info sub $ fullDesc <> progDesc "Worker for frelm.org website."
  where
    sub =
        hsubparser $
        (command "add-repo" $
         info addRepoOptions $ progDesc "Add a new repository to the database.") <>
        (command "migrate" $
         info migrateOptions $ progDesc "Run database migrations")
    addRepoOptions = fmap AddRepo $ argument str $ metavar "REPOSITORY"
    migrateOptions = pure RunMigrations

runWorkerDB ::
       (MonadBaseControl IO m, MonadReader Worker m)
    => ReaderT SqlBackend m b
    -> m b
runWorkerDB action = do
    pool <- asks workerConnPool
    runSqlPool action pool

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

-- | The main function for the worker.
workerMain :: IO ()
workerMain = do
    loadEnv
    loadEnvFrom "./.env-worker"
    workerSettings <- loadYamlSettings [] [configSettingsYmlValue] useEnv
    workerCommand <- execParser parseArgs
    schemaValidation <-
        case workerCommand of
            RunMigrations
                -- Since we'll run the migrations anyway
             -> pure MigrationSuccess
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
{-
checkTags :: RepoId -> WorkerT ()
checkTags repoId = do
    mRepo <-
        get repoId

    forM_ mRepo $ \repo -> do
        knownVersions :: [Version] <-
            (fmap (repoVersionVersion . entityVal)) <$>
                selectList
                    [ RepoVersionRepo ==. repoId ]
                    [ Asc RepoVersionVersion ]

        fetchedVersions :: [(Version, String)] <-
            liftIO $
                fetchGitTags (repoGitUrl repo)

        let
            newVersions =
                filter (\a -> notElem (fst a) knownVersions) fetchedVersions

        unless (null newVersions) $
            withSystemTempDirectory "git-clone" $ \basedir -> do
                gitDir <-
                    cloneGitRepo (repoGitUrl repo) basedir

                forM_ newVersions $
                    \(version, tag) ->
                        void $ checkNewTag repoId gitDir version tag


checkNewTag :: RepoId -> FilePath -> Version -> String -> WorkerT RepoVersionId
checkNewTag repoId gitDir version tag = do
    -- TODO: Care about result
    void $
        checkoutGitRepo gitDir tag

    package <-
        liftIO $
            -- TODO: Here and elsewhere, consider exceptions!
            readFile $ gitDir </> "elm-package.json"

    insert $
        RepoVersion
            { repoVersionRepo = repoId
            , repoVersionTag = pack tag
            , repoVersionVersion = version
            , repoVersionPackage = pack package
            }


checkoutGitRepo :: (MonadIO m, MonadLogger m) => FilePath -> String -> m ExitCode
checkoutGitRepo gitDir tag = do
    -- TODO: Do something with exitCode etc.
    ( exitCode, _, _ ) <-
        liftIO $
            readProcessWithExitCode "git"
                [ "-C"
                , gitDir
                , "checkout"
                , "--quiet"
                , "--detach"
                , tag
                ]
                ""

    pure exitCode


cloneGitRepo :: (MonadLogger m, MonadIO m) => Text -> FilePath -> m FilePath
cloneGitRepo url toPath = do
    -- TODO: Do something with exitCode etc.
    _ <-
        liftIO $
            readProcessWithExitCode "git"
                [ "-C"
                , toPath
                , "clone"
                , "--depth"
                , "1"
                , "--quiet"
                , "--no-single-branch"
                , unpack url
                , "git-clone"
                ]
                ""

    pure $ toPath </> "git-clone"


fetchGitTags :: Text -> IO [(Version, String)]
fetchGitTags url = do
    -- TODO: Check exitCode etc.
    ( _, stdOut, _ ) <-
        readProcessWithExitCode "git"
            [ "ls-remote"
            , "--tags"
            , "--quiet"
            , "--refs"
            , unpack url
            ]
            ""

    pure $
        ( rights
        . fmap (parse parseTagAndVersion "line")
        . lines
        )
        stdOut


parseTagAndVersion :: Parsec String () (Version, String)
parseTagAndVersion =
    sha40 *> tab *> parseTag

    where
        sha40 =
            Parsec.count 40 Parsec.hexDigit

        parseTag = do
            tag <-
                string "refs/tags/"
                *> Parsec.many anyChar
                <* eof

            case fromText $ pack tag of
                Right version ->
                    pure (version, tag)

                Left err ->
                    unexpected err
-}
