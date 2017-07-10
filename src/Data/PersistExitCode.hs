{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.PersistExitCode where

import ClassyPrelude.Yesod
import Database.Persist (PersistValue(..))
import Database.Persist.Sql (PersistFieldSql(..))
import GHC.IO.Exception (ExitCode(..))

instance PersistField ExitCode where
    toPersistValue code =
        case code of
            ExitSuccess -> PersistInt64 0
            ExitFailure err -> PersistInt64 (fromIntegral err)
    fromPersistValue v =
        case v of
            PersistInt64 err ->
                if err == 0
                    then Right ExitSuccess
                    else Right $ ExitFailure (fromIntegral err)
            _ -> Left $ "Got unexpected persist value: " ++ tshow v

instance PersistFieldSql ExitCode where
    sqlType _ = SqlInt32
