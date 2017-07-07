{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Worker where

import Control.Monad.Logger (LoggingT, runStdoutLoggingT)
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Data.Yaml.Config (loadYamlSettingsArgs, useEnv)
import Database.Migrate
import Database.Persist.Postgresql
import LoadEnv (loadEnv, loadEnvFrom)
import Import.Worker


type WorkerT =
    ReaderT Worker (ResourceT (LoggingT IO))


-- | The Foundation datatype for the worker.
data Worker = Worker
    { workerSettings :: WorkerSettings
    , workerConnPool :: ConnectionPool
    }


runWorkerDB :: (MonadBaseControl IO m, MonadReader Worker m) => ReaderT SqlBackend m b -> m b
runWorkerDB action = do
    pool <-
        asks workerConnPool

    runSqlPool action pool


runWorker :: WorkerT ()
runWorker = do
    repos :: [Entity Repo] <-
        runWorkerDB $
            selectList [] []

    liftIO $
        putStrLn (tshow repos)


-- | The main function for the worker.
workerMain :: IO ()
workerMain = do
    loadEnv
    loadEnvFrom "./.env-worker"

    workerSettings <-
        loadYamlSettingsArgs [configSettingsYmlValue] useEnv

    -- TODO: Care about result
    _ <-
        migrateSchema $
            pgConnStr $
                workerDatabaseConf workerSettings

    runStdoutLoggingT $ do
        workerConnPool <-
            createPostgresqlPool
                (pgConnStr  $ workerDatabaseConf workerSettings)
                (pgPoolSize $ workerDatabaseConf workerSettings)

        let worker = Worker {..}

        runResourceT $ flip runReaderT worker $ runWorker
