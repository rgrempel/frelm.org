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
    , createTagCheck
    , createCloneError
    , createPackage
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

createTagCheck :: MigrationCommand
createTagCheck =
    MigrationScript "create-tag-check" $
    encodeUtf8
        [text|
            CREATE TABLE tag_check
                ( id
                    BIGSERIAL
                    PRIMARY KEY

                , repo
                    BIGINT
                    NOT NULL
                    CONSTRAINT tag_check_repo
                        REFERENCES repos
                        ON DELETE CASCADE

                , exit_code
                    INT
                    NOT NULL

                , stdout
                    TEXT
                    NOT NULL

                , stderr
                    TEXT
                    NOT NULL

                , ran
                    TIMESTAMPTZ
                    NOT NULL
                    DEFAULT CURRENT_TIMESTAMP
                );

            CREATE INDEX tag_check_ran_idx
                ON tag_check (ran);

            CREATE INDEX tag_check_repo_idx
                ON tag_check (repo);

            CREATE INDEX tag_check_exit_code_idx
                ON tag_check (exit_code);
        |]

createCloneError :: MigrationCommand
createCloneError =
    MigrationScript "create-clone-error" $
    encodeUtf8
        [text|
            CREATE TABLE clone_error
                ( id
                    BIGSERIAL
                    PRIMARY KEY

                , repo
                    BIGINT
                    NOT NULL
                    CONSTRAINT clone_error_repo
                        REFERENCES repos
                        ON DELETE CASCADE

                , exit_code
                    INT
                    NOT NULL

                , stdout
                    TEXT
                    NOT NULL

                , stderr
                    TEXT
                    NOT NULL

                , ran
                    TIMESTAMPTZ
                    NOT NULL
                    DEFAULT CURRENT_TIMESTAMP
                );

            CREATE INDEX clone_error_ran_idx
                ON clone_error (ran);

            CREATE INDEX clone_error_repo_idx
                ON clone_error (repo);
        |]

createPackage :: MigrationCommand
createPackage =
    MigrationScript "create-package" $
    encodeUtf8
        [text|
            CREATE TYPE SEMVER_RANGE
                AS RANGE ( subtype = SEMVER );

            CREATE TABLE package
                ( id
                    BIGSERIAL
                    PRIMARY KEY

                , version
                    SEMVER
                    NOT NULL

                , summary
                    VARCHAR
                    NOT NULL

                , repository
                    VARCHAR
                    NOT NULL

                , license
                    VARCHAR
                    NOT NULL

                , elm_version
                    SEMVER_RANGE
                    NOT NULL
                );

            ALTER TABLE repo_version
                ADD COLUMN decoded BIGINT,
                ADD COLUMN decode_error VARCHAR,
                ALTER COLUMN package DROP NOT NULL,
                ADD CONSTRAINT repo_version_package_fk
                    FOREIGN KEY (decoded)
                    REFERENCES package;

            CREATE INDEX repo_version_decoded_idx
                ON repo_version (decoded);
        |]
