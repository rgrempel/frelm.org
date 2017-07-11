{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.PersistSemVer where

import ClassyPrelude
import Data.Aeson
import Data.SemVer (Version, fromText, toText)
import Database.Persist (PersistValue(..))
import Database.Persist.Sql
       (PersistField(..), PersistFieldSql(..), SqlType(..))

instance PersistField Version where
    toPersistValue = PersistDbSpecific . encodeUtf8 . toText
    fromPersistValue v =
        case v of
            PersistDbSpecific str -> first pack $ fromText $ decodeUtf8 str
            _ -> Left $ "Got unexpected persist value: " ++ pack (show v)

instance PersistFieldSql Version where
    sqlType _ = SqlOther "SEMVER"

instance FromJSON Version where
    parseJSON = withText "version" $ either fail pure . fromText
