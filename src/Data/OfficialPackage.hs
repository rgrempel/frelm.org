{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.OfficialPackage
    ( OfficialPackage(..)
    , fetchOfficialPackages
    ) where

import ClassyPrelude
import Data.Aeson
import Data.PersistSemVer ()
import Data.SemVer
import Network.HTTP.Simple

data OfficialPackage = OfficialPackage
    { officialPackageName :: Text
    , officialPackageSummary :: Text
    , officialPackageVersions :: [Version]
    } deriving (Show)

instance FromJSON OfficialPackage where
    parseJSON =
        withObject "Official Package" $ \v ->
            OfficialPackage <$> v .: "name" <*> v .: "summary" <*>
            v .: "versions"

fetchOfficialPackages ::
       (MonadIO m, MonadThrow m) => m (Either JSONException [OfficialPackage])
fetchOfficialPackages =
    fmap getResponseBody $
    parseRequest "http://package.elm-lang.org/all-packages" >>= httpJSONEither
