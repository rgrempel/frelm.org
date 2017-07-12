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
import Data.Range
import Data.SemVer (Version)
import Network.URI

data ElmPackage = ElmPackage
    { elmPackageVersion :: Version
    , elmPackageSummary :: Text
    , elmPackageRepository :: Text
    , elmPackageLicense :: Text
    , elmPackageModules :: [Text]
    , elmPackageDependencies :: Map Text (Range Version)
    , elmPackageElmVersion :: Range Version
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

elmPackageLibraryName :: ElmPackage -> Maybe Text
elmPackageLibraryName package = do
    uri <- (parseAbsoluteURI . unpack) $ elmPackageRepository package
    stripSuffix ".git" (pack $ uriPath uri) >>= stripPrefix "/"
