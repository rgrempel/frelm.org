{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Model where

import ClassyPrelude
import Data.PersistExitCode ()
import Data.PersistSemVer ()
import Data.SemVer (Version)
import Database.Persist.TH
import GHC.IO.Exception (ExitCode)

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share
    [mkPersist sqlSettings]
    [persistLowerCase|
        User sql=users
            name Text
            email Text
            avatarUrl Text Maybe
            plugin Text
            ident Text

            UniqueUser plugin ident
            deriving Eq Show Typeable

        Repo sql=repos
            gitUrl Text
            submittedBy UserId Maybe

            UniqueRepo gitUrl
            deriving Eq Show

        TagCheck
            repo RepoId
            ran UTCTime
            exitCode ExitCode
            stdout Text
            stderr Text

        CloneError
            repo RepoId
            ran UTCTime
            exitCode ExitCode
            stdout Text
            stderr Text

        RepoVersion
            repo RepoId
            tag Text
            version Version
            -- If there is no elm-package.json, will be empty
            package Text Maybe

            UniqueRepoVersion repo version
            deriving Eq Show
    |]
