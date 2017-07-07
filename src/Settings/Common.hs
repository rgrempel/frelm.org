{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Settings.Common where

import ClassyPrelude
import qualified Control.Exception as Exception
import Data.Aeson
import Data.FileEmbed (embedFile)
import Data.Yaml (decodeEither')
import Settings.Yml


-- | Raw bytes at compile time of @config/settings.yml@
configSettingsYmlBS :: ByteString
configSettingsYmlBS =
    $(embedFile configSettingsYml)

-- | @config/settings.yml@, parsed to a @Value@.
configSettingsYmlValue :: Value
configSettingsYmlValue =
    either Exception.throw id $
        decodeEither' configSettingsYmlBS
