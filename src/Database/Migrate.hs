{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Database.Migrate where

import ClassyPrelude
import Database.PostgreSQL.Simple (connectPostgreSQL, withTransaction, close)
import Database.PostgreSQL.Simple.Migration as SM (MigrationCommand(..), MigrationResult, runMigrations)
import NeatInterpolation (text)


migrateSchema :: ByteString -> IO (SM.MigrationResult String)
migrateSchema url = do
    con <-
        connectPostgreSQL url

    -- TODO: Do something with result ...
    result <-
        withTransaction con $
            SM.runMigrations True con $
                [ MigrationInitialization
                , migrateInitial
                , createUsers
                , createRepos
                , createRepoVersions
                , addTagToRepoVersions
                , addPackageToRepoVersions
                ]

    close con

    pure result


migrateInitial :: MigrationCommand
migrateInitial =
    MigrationScript "migrate-initial" $ encodeUtf8
        [text|
            CREATE EXTENSION IF NOT EXISTS plpgsql
                WITH SCHEMA pg_catalog;
        |]


createUsers :: MigrationCommand
createUsers =
    MigrationScript "create-users" $ encodeUtf8
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
    MigrationScript "create-repos" $ encodeUtf8
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
    MigrationScript "create-repo-version" $ encodeUtf8
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
    MigrationScript "repo-version-add-package" $ encodeUtf8
        [text|
            ALTER TABLE repo_version
                ADD COLUMN package
                    VARCHAR
                    NOT NULL;
        |]


addTagToRepoVersions :: MigrationCommand
addTagToRepoVersions =
    MigrationScript "repo-version-add-tag" $ encodeUtf8
        [text|
            ALTER TABLE repo_version
                ADD COLUMN tag
                    VARCHAR
                    NOT NULL;
        |]
