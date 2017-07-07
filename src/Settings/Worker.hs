{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Settings.Worker where

import ClassyPrelude
import Data.Aeson
import Database.Persist.Postgresql (PostgresConf)


-- | Settings for the worker process.
data WorkerSettings = WorkerSettings
    { workerDatabaseConf :: PostgresConf
    }


instance FromJSON WorkerSettings where
    parseJSON =
        withObject "WorkerSettings" $ \o -> do
            workerDatabaseConf <- o .: "database"

            pure WorkerSettings {..}
