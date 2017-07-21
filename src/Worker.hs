{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Worker where

import Control.Monad.Logger
import Control.Monad.Trans.Resource
       (ResourceT, resourceForkWith, runResourceT)
import Data.Aeson (eitherDecodeStrict)
import Data.ElmPackage
import Data.List (nub)
import Data.Map (traverseWithKey)
import Data.OfficialPackage
import Data.SemVer (Version, fromText)
import Data.Time.Clock
import Data.Time.Format
import Data.Yaml.Config (loadYamlSettings, useEnv)
import Database.Esqueleto
import Database.Migrate
import qualified Database.Persist as P
import Database.Persist.Postgresql
       (createPostgresqlPool, pgConnStr, pgPoolSize)
import GHC.IO.Exception (ExitCode(..))
import Import.Worker hiding ((<>), isNothing, on)
import LoadEnv (loadEnv, loadEnvFrom)
import Options.Applicative as OA
import System.Directory (doesFileExist)
import System.Exit (die)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess, env, proc)
import System.Process.ByteString
       (readCreateProcessWithExitCode, readProcessWithExitCode)
import Text.Parsec as Parsec

type WorkerT = ResourceT (ReaderT Worker (LoggingT IO))

type WorkerDB = ReaderT SqlBackend WorkerT

-- | The Foundation datatype for the worker.
data Worker = Worker
    { workerSettings :: WorkerSettings
    , workerConnPool :: ConnectionPool
    , workerArgs :: Args
    }

data Command
    = AddRepo String
    | Crawl
    | RecheckRepo Int64
    | RecheckTags
    | ReparseMissingPackages
    | ReparseAllPackages
    | RunMigrations
    | Scrape
    deriving (Show)

data Args = Args
    { argsVerbosity :: LogLevel
    , argsCommand :: Command
    }

parseArgs :: ParserInfo Args
parseArgs = info (mainParser <**> helper) description
  where
    description = fullDesc <> progDesc "Frelm worker"
    mainParser = Args <$> parseVerbosity <*> parseCommand
    parseVerbosity =
        (toLogLevel . length) <$>
        OA.many
            (flag' () $
             short 'v' <>
             help
                 "Set verbosity. Defaults to Error, then each -v increases to Warn, Info then Debug. So, '-vvv' for Debug.")
    parseCommand =
        hsubparser $
        command "add-repo" addRepoParser <> command "migrate" migrateParser <>
        command "crawl" crawlParser <>
        command "recheck-tags" recheckTagsParser <>
        command "recheck-repo" recheckRepoParser <>
        command "reparse-missing" reparseMissingParser <>
        command "reparse-all" reparseAllParser <>
        command "scrape" scrapeParser
    addRepoParser =
        info
            (fmap AddRepo $ argument str $ metavar "REPOSITORY")
            (fullDesc <> progDesc "Add a new repository to the database.")
    migrateParser =
        info
            (pure RunMigrations)
            (fullDesc <> progDesc "Run database migrations")
    crawlParser = info (pure Crawl) (fullDesc <> progDesc "Crawl")
    recheckTagsParser =
        info (pure RecheckTags) (fullDesc <> progDesc "Recheck tags")
    recheckRepoParser =
        info
            (fmap RecheckRepo $
             argument auto $ metavar "REPO_ID" <> help "repoId to recheck")
            (fullDesc <> progDesc "Recheck a repo")
    reparseMissingParser =
        info
            (pure ReparseMissingPackages)
            (fullDesc <> progDesc "Reparse missing")
    reparseAllParser =
        info (pure ReparseAllPackages) (fullDesc <> progDesc "Reparse All")
    scrapeParser = info (pure Scrape) (fullDesc <> progDesc "Scrape packages")

runWorkerDB :: WorkerDB a -> WorkerT a
runWorkerDB workerDB = asks workerConnPool >>= runSqlPool workerDB

runWorker :: WorkerT ()
runWorker = do
    todo <- asks (argsCommand . workerArgs)
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
        ReparseMissingPackages -> reparseMissingPackages
        ReparseAllPackages -> reparseAllPackages
        Scrape -> scrape

-- | The main function for the worker.
workerMain :: IO ()
workerMain = do
    loadEnv
    loadEnvFrom "./.env-worker"
    workerSettings <- loadYamlSettings [] [configSettingsYmlValue] useEnv
    workerArgs <- execParser parseArgs
    -- We short-circuit the validation if we're running the migrations
    schemaValidation <-
        case argsCommand workerArgs of
            RunMigrations -> pure MigrationSuccess
            _ -> validateSchema $ pgConnStr $ workerDatabaseConf workerSettings
    case schemaValidation of
        MigrationError err -> die err
        MigrationSuccess ->
            runStdoutLoggingT $
            filterLogger (logWhen $ argsVerbosity workerArgs) $ do
                workerConnPool <-
                    createPostgresqlPool
                        (pgConnStr $ workerDatabaseConf workerSettings)
                        (pgPoolSize $ workerDatabaseConf workerSettings)
                let worker = Worker {..}
                runReaderT (runResourceT runWorker) worker

toLogLevel :: Int -> LogLevel
toLogLevel 0 = LevelError
toLogLevel 1 = LevelWarn
toLogLevel 2 = LevelInfo
toLogLevel _ = LevelDebug

logWhen :: LogLevel -> LogSource -> LogLevel -> Bool
logWhen verbosity _ level = level >= verbosity

waitForChildren :: [MVar ()] -> WorkerT ()
waitForChildren children =
    case children of
        [] -> pure ()
        firstKid:restOfKids -> do
            takeMVar firstKid
            waitForChildren restOfKids

forkChild :: WorkerT () -> WorkerT (MVar ())
forkChild todo = do
    mvar <- liftIO newEmptyMVar
    let handleResult r =
            case r of
                Left e -> do
                    print e
                    throwIO e
                Right _ -> putMVar mvar ()
    void $ resourceForkWith (`forkFinally` handleResult) todo
    pure mvar

crawl :: WorkerT ()
crawl = do
    scrapeMVar <- forkChild scrapeThread
    checkTagsMVar <- forkChild checkTagsThread
    waitForChildren [scrapeMVar, checkTagsMVar]

waitInterval :: NominalDiffTime -> WorkerT ()
waitInterval diff = liftIO $ threadDelay $ round diff * 1000000

-- | We'll check the official Elm package site for totally new libraries no
-- more than 4 times per day.
scrapeInterval :: NominalDiffTime
scrapeInterval = 86400 / 4

scrapeThread :: WorkerT ()
scrapeThread =
    forever $ do
        now <- liftIO getCurrentTime
        fetchLastRan <-
            runWorkerDB $
            select $ from $ \sr -> pure (max_ (sr ^. ScrapeResultRan))
        let neverRan = (-scrapeInterval) `addUTCTime` now
        let lastRan =
                maybe
                    neverRan
                    (fromMaybe neverRan . unValue)
                    (listToMaybe fetchLastRan)
        let shouldRunIn = scrapeInterval `addUTCTime` lastRan `diffUTCTime` now
        $(logInfo) $ "Should scrape in: " <> tshow shouldRunIn
        if shouldRunIn <= 0
            then do
                $(logInfo) "Scraping"
                scrape
                waitInterval scrapeInterval
            else waitInterval shouldRunIn

-- | In addition to fetching tags for each repository no more than every so
-- often, we also ensure that we wait a bit between each attempt to check
-- **any** repo.
minimumCheckInterval :: NominalDiffTime
minimumCheckInterval = 10

-- | We'll wake up this often to see if we should do something, even if we had
-- nothing to do last time.
maximumCheckInterval :: NominalDiffTime
maximumCheckInterval = 10 * 60

-- | We'll check once per day for each repo.
repoCheckInterval :: NominalDiffTime
repoCheckInterval = 86400

checkTagsThread :: WorkerT ()
checkTagsThread =
    forever $ do
        numberChecked <-
            runWorkerDB $ do
                now <- liftIO getCurrentTime
                let checkIfOlderThan = (-repoCheckInterval) `addUTCTime` now
                needsChecking <-
                    select $
                    from $ \(r `LeftOuterJoin` tc) -> do
                        on $ just (r ^. RepoId) ==. tc ?. TagCheckRepo
                        where_ $
                            isNothing (tc ?. TagCheckRan) ||.
                            (tc ?. TagCheckRan <=. just (val checkIfOlderThan))
                        limit 1
                        pure r
                traverse_ checkRepoTags needsChecking
                pure $ length needsChecking
        if numberChecked == 0
            then waitInterval maximumCheckInterval
            else waitInterval minimumCheckInterval

reparseMissingPackages :: WorkerT ()
reparseMissingPackages =
    void $ runWorkerDB $ decodedPackageIsNull >>= traverse decodePackageJSON

reparseAllPackages :: WorkerT ()
reparseAllPackages =
    void $ runWorkerDB $ allPackageChecks >>= traverse decodePackageJSON

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

checkRepoTags :: Entity Repo -> WorkerDB ()
checkRepoTags repo = do
    $(logInfo) $ "Checking repo: " <> (repoGitUrl . entityVal) repo
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
    void $
        upsert
            TagCheck
            { tagCheckStdout = decodeUtf8 out
            , tagCheckStderr = decodeUtf8 err
            , tagCheckExitCode = exitCode
            , tagCheckRan = ran
            , tagCheckRepo = entityKey repo
            }
            [ TagCheckStdout P.=. decodeUtf8 out
            , TagCheckStderr P.=. decodeUtf8 err
            , TagCheckExitCode P.=. exitCode
            , TagCheckRan P.=. ran
            ]
    pure $
        (rights . fmap (parse parseTag "line") . lines)
            ((unpack . decodeUtf8) out)

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
                    pc <-
                        do let elmPackageJson = gitDir </> "elm-package.json"
                           hasElmPackage <-
                               liftIO $ doesFileExist elmPackageJson
                           if hasElmPackage
                               then do
                                   contents <- liftIO $ readFile elmPackageJson
                                   upsert
                                       PackageCheck
                                       { packageCheckPackage = Just contents
                                       , packageCheckDecodeError = Nothing
                                       , packageCheckRepoVersion =
                                             entityKey repoVersion
                                       }
                                       [ PackageCheckPackage P.=. Just contents
                                       , PackageCheckDecodeError P.=. Nothing
                                       ]
                               else upsert
                                        PackageCheck
                                        { packageCheckPackage = Nothing
                                        , packageCheckDecodeError = Nothing
                                        , packageCheckRepoVersion =
                                              entityKey repoVersion
                                        }
                                        [ PackageCheckPackage P.=. Nothing
                                        , PackageCheckDecodeError P.=. Nothing
                                        ]
                    decodePackageJSON pc

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
                void $
                    upsert
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
                        [ PackageVersion P.=. elmPackageVersion p
                        , PackageSummary P.=. elmPackageSummary p
                        , PackageRepository P.=. elmPackageRepository p
                        , PackageLibrary P.=. libraryId
                        , PackageLicense P.=. elmPackageLicense p
                        , PackageNativeModules P.=. elmPackageNativeModules p
                        , PackageElmVersion P.=. elmPackageElmVersion p
                        ]
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
    -> m (Either (ExitCode, Text, Text) UTCTime)
tagCommittedAt gitDir sha = do
    (exitCode, out, err) <-
        liftIO $
        readProcessWithExitCode
            "git"
            ["--git-dir", gitDir </> ".git", "show", "-s", "--format=%cD", sha]
            ""
    pure $
        case exitCode of
            ExitSuccess ->
                first
                    (\err2 ->
                         ( ExitFailure 1
                         , (pack . lastString . lines . unpack . decodeUtf8) out
                         , pack err2)) $
                parseTimeM False defaultTimeLocale rfc822DateFormat .
                lastString . lines . unpack . decodeUtf8 $
                out
            ExitFailure _ -> Left (exitCode, decodeUtf8 out, decodeUtf8 err)

lastString :: [String] -> String
lastString = go ""
  where
    go acc v =
        case v of
            a:b -> go a b
            [] -> acc

checkoutGitRepo ::
       MonadIO m => FilePath -> String -> m (Either (ExitCode, Text, Text) ())
checkoutGitRepo gitDir tag = do
    (exitCode, out, err) <-
        liftIO $
        readProcessWithExitCode
            "git"
            [ "--git-dir"
            , gitDir </> ".git"
            , "--work-tree"
            , gitDir
            , "checkout"
            , "--quiet"
            , "--detach"
            , tag
            ]
            ""
    if exitCode == ExitSuccess
        then pure $ Right ()
        else pure $ Left (exitCode, decodeUtf8 out, decodeUtf8 err)

cloneGitRepo :: Entity Repo -> FilePath -> WorkerDB (Maybe FilePath)
cloneGitRepo repo toPath = do
    (exitCode, out, err) <-
        liftIO $
        readProcessWithExitCode
            "git"
            [ "clone"
            , "--depth"
            , "1"
            , "--quiet"
            , "--no-single-branch"
            , unpack $ (repoGitUrl . entityVal) repo
            , toPath </> "git-clone"
            ]
            ""
    if exitCode == ExitSuccess
        then (pure . Just) (toPath </> "git-clone")
        else do
            ran <- liftIO getCurrentTime
            insert_
                CloneError
                { cloneErrorExitCode = exitCode
                , cloneErrorStdout = decodeUtf8 out
                , cloneErrorStderr = decodeUtf8 err
                , cloneErrorRepo = entityKey repo
                , cloneErrorRan = ran
                }
            pure Nothing

scrape :: WorkerT ()
scrape = do
    result <- fetchOfficialPackages
    ran <- liftIO getCurrentTime
    runWorkerDB $
        case result of
            Left err ->
                insert_
                    ScrapeResult
                    { scrapeResultRan = ran
                    , scrapeResultGot = Nothing
                    , scrapeResultError = Just $ tshow err
                    }
            Right packages -> do
                insert_
                    ScrapeResult
                    { scrapeResultRan = ran
                    , scrapeResultGot = Just $ length packages
                    , scrapeResultError = Nothing
                    }
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
