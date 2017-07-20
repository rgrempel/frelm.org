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
{-# OPTIONS_GHC -ddump-splices -ddump-to-file #-}

module Database.Model where

import ClassyPrelude
import Data.PersistExitCode ()
import Data.PersistSemVer ()
import Data.Range
import Data.SemVer (Version)
import Database.Persist.TH
import GHC.IO.Exception (ExitCode)

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
mkPersist
    sqlSettings
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

            UniqueTagCheck repo

        CloneError
            repo RepoId
            ran UTCTime
            exitCode ExitCode
            stdout Text
            stderr Text

        RepoVersion
            repo RepoId
            tag Text
            sha Text
            committedAt UTCTime
            -- version is calculated from the tag ... it's nice for comparisons
            -- etc. Note that it's not necessarily the same as the version
            -- declared in the decoded package ... that is, this is an
            -- interpretation of the tag, rather than the decoded version. Of
            -- course, normally those ought to be the same.
            version Version

            UniqueRepoVersion repo tag
            deriving Eq Show

        -- This is a one-to-one with RepoVersion .. basically, we track the
        -- result of the latest attempt to decode the package. If we got the
        -- package at all, the package field will be non-null. If we had an
        -- error decoding it, the decodeError will be non-null. We keep this
        -- around even if we succeed, so that we can re-decode the package if
        -- necessary without re-cloning. (That's nice for development ... might
        -- not be necessary once stable).
        --
        -- This is distinct from `Package` so that we only have to join the
        -- textual contents of the package when that's relevant ... we can
        -- often skip joining this table.
        PackageCheck
            repoVersion RepoVersionId

            package Text Maybe
            decodeError Text Maybe

            UniquePackageCheck repoVersion

        -- Another one-to-one with RepoVersion ... this is the decoded contents
        -- of the elm-package.json, if we succeeded in decoding them. In a
        -- seperate table from `RepoVersion` since otherwise all of these would
        -- need to be nullable.
        Package
            repoVersion RepoVersionId

            version Version
            summary Text
            repository Text
            library LibraryId Maybe
            license Text
            nativeModules Bool
            elmVersion (Range Version) Maybe

            UniquePackage repoVersion

        Dependency
            repoVersion RepoVersionId
            library LibraryId
            version (Range Version)
            UniqueDepdenency repoVersion library

        Library
            name Text
            UniqueLibrary name

        PublishedVersion
            library LibraryId
            version Version
            UniquePublishedVersion library version

        Module sql=modules
            name Text
            UniqueModule name

        PackageModule
            repoVersion RepoVersionId
            moduleId ModuleId
            exposed Bool
            UniquePackageModule repoVersion moduleId

        ScrapeResult
            ran UTCTime
            got Int Maybe
            error Text Maybe
    |]
