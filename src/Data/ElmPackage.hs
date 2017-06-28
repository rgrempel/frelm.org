{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A module for decoding an elm-package.json structure.
module Data.ElmPackage where

import Data.Aeson (withObject, withText)
import Data.SemVer (Version, fromText)
import Import
import Text.Parsec as Parsec


data ElmPackage = ElmPackage
    { elmPackageVersion :: Version
    , elmPackageSummary :: Text
    , elmPackageRepository :: Text
    , elmPackageLicense :: Text
    , elmPackageModules :: [Text]
    , elmPackageDependencies :: Map Text VersionBounds
    , elmPackageElmVersion :: VersionBounds
    } deriving (Eq, Show)


instance FromJSON ElmPackage where
    parseJSON =
        withObject "Elm Package" $ \v -> ElmPackage
            <$> v .: "version"
            <*> v .: "summary"
            <*> v .: "repository"
            <*> v .: "license"
            <*> v .: "exposed-modules"
            <*> v .: "dependencies"
            <*> v .: "elm-version"


data VersionBounds = VersionBounds
    { atLeast :: Version
    , lessThan :: Version
    } deriving (Eq, Show)


instance FromJSON VersionBounds where
    parseJSON =
        withText "Version bounds" $ \v ->
            either (fail . show) pure $
                Parsec.parse parseBounds "version bounds" v


parseBounds :: Parsec Text () VersionBounds
parseBounds = do
    lowerBound <-
        Parsec.many $
            Parsec.alphaNum Parsec.<|> Parsec.char '.'

    Parsec.spaces
    Parsec.string "<= v <"
    Parsec.spaces

    upperBound <-
        Parsec.many $
            Parsec.alphaNum Parsec.<|> Parsec.char '.'

    Parsec.spaces
    Parsec.eof

    either unexpected pure $
        VersionBounds
            <$> (fromText $ pack lowerBound)
            <*> (fromText $ pack upperBound)
