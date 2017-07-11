{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.VersionBounds where

import ClassyPrelude
import Data.Aeson
import Data.PersistSemVer ()
import Data.SemVer (Version, fromText, toText)
import Database.Persist (PersistValue(..))
import Database.Persist.Sql
       (PersistField(..), PersistFieldSql(..), SqlType(..))
import Text.Parsec as Parsec
import Text.Parsec (char, spaces, string)

data VersionBounds = VersionBounds
    { atLeast :: Version
    , lessThan :: Version
    } deriving (Eq, Show)

instance FromJSON VersionBounds where
    parseJSON =
        withText "Version bounds" $ \v ->
            either (fail . show) pure $
            Parsec.parse parseBounds "version bounds" v

parseVersion :: Parsec Text () Version
parseVersion = do
    str <-
        Parsec.many $
        asum
            [Parsec.alphaNum, Parsec.char '.', Parsec.char '-', Parsec.char '+']
    either unexpected pure (fromText $ pack str)

parseBounds :: Parsec Text () VersionBounds
parseBounds = do
    spaces
    lowerBound <- parseVersion
    spaces >> string "<=" >> spaces >> char 'v' >> spaces >> char '<' >> spaces
    upperBound <- parseVersion
    spaces >> eof
    pure $ VersionBounds lowerBound upperBound

instance PersistField VersionBounds where
    toPersistValue v =
        PersistDbSpecific $
        encodeUtf8 $
        "[" <> (toText . atLeast) v <> "," <> (toText . lessThan) v <> ")"
    fromPersistValue v =
        case v of
            PersistDbSpecific str ->
                Left $ "Here's what I got" ++ decodeUtf8 str
            _ -> Left $ "Got unexpected persist value: " ++ tshow v

instance PersistFieldSql VersionBounds where
    sqlType _ = SqlOther "SEMVER_RANGE"
