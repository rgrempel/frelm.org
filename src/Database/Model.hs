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
import Data.VersionBounds
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
            package Text Maybe
            decodeError Text Maybe
            decoded PackageId Maybe

            UniqueRepoVersion repo version
            deriving Eq Show

        Package
            version Version
            summary Text
            repository Text
            library LibraryId Maybe
            license Text
            elmVersion VersionBounds

        Dependency
            package PackageId
            library LibraryId
            repo RepoId
            version VersionBounds
            UniqueDepdenency package library

        Library
            name Text
            UniqueLibrary name

        Module sql=modules
            name Text
            UniqueModule name

        PackageModule
            packageId PackageId
            moduleId ModuleId
            exposed Bool
            UniquePackageModule packageId moduleId
    |]
