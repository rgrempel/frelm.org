{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.PersistSemVer where

import ClassyPrelude.Yesod
import Database.Persist (PersistValue(..))
import Database.Persist.Sql (PersistFieldSql(..))
import Data.SemVer (Version, fromText, toText)


instance PersistField Version where
    toPersistValue =
        PersistDbSpecific . encodeUtf8 . toText

    fromPersistValue v =
        case v of
            PersistDbSpecific str ->
                first pack $ fromText $ decodeUtf8 str

            _ ->
                Left $ "Got unexpected persist value: " ++ pack (show v)


instance PersistFieldSql Version where
    sqlType _ = SqlOther "SEMVER"
