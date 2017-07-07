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

import Control.Monad.Logger (LoggingT, liftLoc, runLoggingT, runStdoutLoggingT, LogSource, LogLevel)
import Database.Persist.Postgresql (createPostgresqlPool, withPostgresqlPool, pgConnStr, pgPoolSize, runSqlPool)
import Import


type WorkerT =
    ReaderT Worker (ResourceT (LoggingT IO))


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
