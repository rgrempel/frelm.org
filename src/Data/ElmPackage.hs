{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A module for decoding an elm-package.json structure.
module Data.ElmPackage where

import ClassyPrelude
import Data.Aeson
import Data.PersistSemVer ()
import Data.Range
import Data.SemVer (Version)
import Network.URI

data ElmPackage = ElmPackage
    { elmPackageVersion :: Version
    , elmPackageSummary :: Text
    , elmPackageRepository :: Text
    , elmPackageLicense :: Text
    , elmPackageSourceDirectories :: [Text]
    , elmPackageModules :: [Text]
    , elmPackageNativeModules :: Bool
    , elmPackageDependencies :: Map Text (Range Version)
    -- Not all pacakges specify this ... perhaps it wasn't
    -- required at one point?
    , elmPackageElmVersion :: Maybe (Range Version)
    } deriving (Eq, Show)

instance FromJSON ElmPackage where
    parseJSON =
        withObject "Elm Package" $ \v -> do
            elmPackageVersion <- v .: "version"
            elmPackageSummary <- v .: "summary"
            elmPackageRepository <- v .: "repository"
            elmPackageLicense <- v .: "license"
            elmPackageModules <- v .: "exposed-modules"
            elmPackageDependencies <- v .: "dependencies"
            elmPackageElmVersion <- v .:? "elm-version"
            elmPackageSourceDirectories <- v .: "source-directories"
            elmPackageNativeModules <- v .:? "native-modules" .!= False
            pure ElmPackage {..}

elmPackageLibraryName :: ElmPackage -> Maybe Text
elmPackageLibraryName package = do
    uri <- (parseAbsoluteURI . unpack) $ elmPackageRepository package
    stripSuffix ".git" (pack $ uriPath uri) >>= stripPrefix "/"
