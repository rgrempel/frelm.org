{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.PersistSemVer where

import ClassyPrelude hiding (many)
import Data.Aeson
import Data.Range
import Data.SemVer (Version, fromText, toText)
import Database.Persist (PersistValue(..))
import Database.Persist.Sql
       (PersistField(..), PersistFieldSql(..), SqlType(..))
import Text.Parsec

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

instance FromJSON (Range Version) where
    parseJSON =
        withText "version bounds" $ \v ->
            either (fail . show) pure $
            parse (elmPackageRange parseVersion) "" v

parseVersion :: Parsec Text () Version
parseVersion = do
    str <- many $ asum [alphaNum, char '.', char '-', char '+']
    either unexpected pure (fromText $ pack str)

instance PersistField (Range Version) where
    toPersistValue = makePersistValue (encodeUtf8 . toText)
    fromPersistValue = readPersistValue parseVersion

instance PersistFieldSql (Range Version) where
    sqlType _ = SqlOther "SEMVER_RANGE"
