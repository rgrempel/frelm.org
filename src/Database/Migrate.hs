{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Database.Migrate
    ( migrateSchema
    , validateSchema
    , module Import
    ) where

import ClassyPrelude
import Database.PostgreSQL.Simple
       (close, connectPostgreSQL, withTransaction)
import Database.PostgreSQL.Simple.Migration as Import
       (MigrationResult(..))
import Database.PostgreSQL.Simple.Migration
import NeatInterpolation (text)

migrateSchema :: ByteString -> IO (MigrationResult String)
migrateSchema url = do
    con <- connectPostgreSQL url
    result <- withTransaction con $ runMigrations False con migrations
    close con
    pure result

validateSchema :: ByteString -> IO (MigrationResult String)
validateSchema url = do
    con <- connectPostgreSQL url
    result <-
        withTransaction con $
        runMigrations False con (map MigrationValidation migrations)
    close con
    pure result

migrations :: [MigrationCommand]
migrations =
    [ MigrationInitialization
    , migrateInitial
    , createUsers
    , createRepos
    , createRepoVersions
    , addTagToRepoVersions
    , addPackageToRepoVersions
    ]

migrateInitial :: MigrationCommand
migrateInitial =
    MigrationScript "migrate-initial" $
    encodeUtf8
        [text|
            CREATE EXTENSION IF NOT EXISTS plpgsql
                WITH SCHEMA pg_catalog;
        |]

createUsers :: MigrationCommand
createUsers =
    MigrationScript "create-users" $
    encodeUtf8
        [text|
            CREATE TABLE users
                ( id
                    BIGSERIAL
                    PRIMARY KEY

                , name
                    VARCHAR
                    NOT NULL

                , email
                    VARCHAR
                    NOT NULL

                , plugin
                    VARCHAR
                    NOT NULL

                , ident
                    VARCHAR
                    NOT NULL

                , avatar_url
                    VARCHAR

                , CONSTRAINT users_unique
                    UNIQUE (plugin, ident)
                );
        |]

createRepos :: MigrationCommand
createRepos =
    MigrationScript "create-repos" $
    encodeUtf8
        [text|
            CREATE TABLE repos
                ( id
                    BIGSERIAL
                    PRIMARY KEY

                , git_url
                    VARCHAR
                    NOT NULL
                    CONSTRAINT repos_unique_git_url UNIQUE

                , submitted_by
                    BIGINT
                    CONSTRAINT repos_users_fk REFERENCES users
                );
        |]

createRepoVersions :: MigrationCommand
createRepoVersions =
    MigrationScript "create-repo-version" $
    encodeUtf8
        [text|
            CREATE TABLE repo_version
                ( id
                    BIGSERIAL
                    PRIMARY KEY

                , repo
                    BIGINT
                    NOT NULL
                    CONSTRAINT repo_version_repo_fk REFERENCES repos

                , version
                    SEMVER
                    NOT NULL

                , CONSTRAINT repo_version_unique
                    UNIQUE (repo, version)
                );
        |]

addPackageToRepoVersions :: MigrationCommand
addPackageToRepoVersions =
    MigrationScript "repo-version-add-package" $
    encodeUtf8
        [text|
            ALTER TABLE repo_version
                ADD COLUMN package
                    VARCHAR
                    NOT NULL;
        |]

addTagToRepoVersions :: MigrationCommand
addTagToRepoVersions =
    MigrationScript "repo-version-add-tag" $
    encodeUtf8
        [text|
            ALTER TABLE repo_version
                ADD COLUMN tag
                    VARCHAR
                    NOT NULL;
        |]
