{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A module for decoding an elm-package.json structure.
module Data.ElmPackage where

import Data.SemVer (Version)
import Import


data ElmPackage = ElmPackage
    { elmPackageVersion :: Version
    , elmPackageSummary :: Text
    , elmPackageRepository :: Text
    , elmPackageLicense :: Text
    , elmPackageModules :: [Text]
    , elmPackageDependencies :: [Dependency]
    , elmPackageElmVersion :: VersionBounds
    } deriving (Eq, Show)


data VersionBounds = VersionBounds
    { atLeast :: Version
    , lessThan :: Version
    } deriving (Eq, Show)


data Dependency = Dependency
    { label :: Text
    , bounds :: VersionBounds
    } deriving (Eq, Show)
