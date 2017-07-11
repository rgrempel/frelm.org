{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A module for decoding an elm-package.json structure.
module Data.ElmPackage where

import ClassyPrelude
import Data.Aeson
import Data.PersistSemVer ()
import Data.SemVer (Version, fromText)
import Data.VersionBounds
import Text.Parsec as Parsec
import Text.Parsec (char, spaces, string)

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
        withObject "Elm Package" $ \v ->
            ElmPackage <$> v .: "version" <*> v .: "summary" <*>
            v .: "repository" <*>
            v .: "license" <*>
            v .: "exposed-modules" <*>
            v .: "dependencies" <*>
            v .: "elm-version"
