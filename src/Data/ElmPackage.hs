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
import Text.Parsec (spaces, string, char)


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


parseVersion :: Parsec Text () Version
parseVersion = do
    str <-
        Parsec.many $
            asum
                [ Parsec.alphaNum
                , Parsec.char '.'
                , Parsec.char '-'
                , Parsec.char '+'
                ]

    either unexpected pure (fromText $ pack str)


parseBounds :: Parsec Text () VersionBounds
parseBounds = do
    spaces

    lowerBound <-
        parseVersion

    spaces >> string "<=" >> spaces >> char 'v' >> spaces >> char '<' >> spaces

    upperBound <-
        parseVersion

    spaces >> eof

    pure $
        VersionBounds lowerBound upperBound
