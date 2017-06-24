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
        PersistText . toText

    fromPersistValue v =
        case v of
            PersistText str ->
                first pack $ fromText str

            PersistByteString str ->
                first pack $ fromText $ decodeUtf8 str

            _ ->
                Left $ "Got unexpected persist value"


instance PersistFieldSql Version where
    sqlType _ = SqlString
