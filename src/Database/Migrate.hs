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
    , createModules
    , createLibrary
    , createDependency
    , createPublishedVersion
    , allowNullElmVersion
    , addCommittedAtToRepoVersion
    , addDefaultForSha
    , addDefaultForCommittedAt
    , addNativeModulesToPackage
    , uniqueRepoVersionOnTag
    , indexRepoAndVersion
    , rejigPackages
    , dropIdFieldFromPackage
    , dropRepoFromDependency
    , rejigPackagesAgain
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

createModules :: MigrationCommand
createModules =
    MigrationScript "create-modules" $
    encodeUtf8
        [text|
            CREATE TABLE modules
                ( id
                    BIGSERIAL
                    PRIMARY KEY

                , name
                    VARCHAR
                    NOT NULL

                , CONSTRAINT name_unique
                    UNIQUE (name)
                );

            CREATE TABLE package_module
                ( id
                    BIGSERIAL
                    PRIMARY KEY

                , package_id
                    BIGINT
                    NOT NULL
                    CONSTRAINT package_module_package_fk
                        REFERENCES package
                        ON DELETE CASCADE

                , module_id
                    BIGINT
                    NOT NULL
                    CONSTRAINT package_module_module_fk
                        REFERENCES modules

                , exposed
                    BOOL
                    NOT NULL

                , CONSTRAINT package_module_unique
                    UNIQUE (package_id, module_id)
                );

            CREATE INDEX package_module_package_idx
                ON package_module (package_id);

            CREATE INDEX package_module_module_idx
                ON package_module (module_id);
        |]

createLibrary :: MigrationCommand
createLibrary =
    MigrationScript "create-library" $
    encodeUtf8
        [text|
            CREATE TABLE library
                ( id
                    BIGSERIAL
                    PRIMARY KEY

                , name
                    VARCHAR
                    NOT NULL

                , CONSTRAINT library_name_unique
                    UNIQUE (name)
                );

            ALTER TABLE package
                ADD COLUMN library
                    BIGINT,
                ADD CONSTRAINT package_library_fk
                    FOREIGN KEY (library)
                    REFERENCES library;

            CREATE INDEX package_library_idx
                ON package (library);
        |]

createDependency :: MigrationCommand
createDependency =
    MigrationScript "create-dependency" $
    encodeUtf8
        [text|
            CREATE TABLE dependency
                ( id
                    BIGSERIAL
                    PRIMARY KEY

                , package
                    BIGINT
                    NOT NULL
                    CONSTRAINT dependency_package_fk
                        REFERENCES package
                        ON DELETE CASCADE

                , library
                    BIGINT
                    NOT NULL
                    CONSTRAINT dependency_library_fk
                        REFERENCES library

                , repo
                    BIGINT
                    NOT NULL
                    CONSTRAINT dependency_repo_fk
                        REFERENCES repos

                , version
                    SEMVER_RANGE
                    NOT NULL

                , CONSTRAINT depdenency_unique
                    UNIQUE (package, library)
                );

            CREATE INDEX dependency_package_idx
                ON dependency (package);

            CREATE INDEX dependency_library_idx
                ON dependency (library);

            CREATE INDEX dependency_repo_idx
                ON dependency (repo);
        |]

createPublishedVersion :: MigrationCommand
createPublishedVersion =
    MigrationScript "create-published-version" $
    encodeUtf8
        [text|
            CREATE TABLE published_version
                ( id
                    BIGSERIAL
                    PRIMARY KEY

                , library
                    BIGINT
                    NOT NULL
                    CONSTRAINT published_version_library_fk
                        REFERENCES library

                , version
                    SEMVER
                    NOT NULL

                , CONSTRAINT published_version_unique
                    UNIQUE (library, version)
                );

            CREATE INDEX published_version_library_idx
                ON dependency (library);

            CREATE INDEX published_version_dependency_idx
                ON dependency (version);
        |]

allowNullElmVersion :: MigrationCommand
allowNullElmVersion =
    MigrationScript "allow-null-elm-version" $
    encodeUtf8
        [text|
            ALTER TABLE package
                ALTER COLUMN elm_version
                    DROP NOT NULL;
        |]

addCommittedAtToRepoVersion :: MigrationCommand
addCommittedAtToRepoVersion =
    MigrationScript "add-commmited_at-to-repo_version" $
    encodeUtf8
        [text|
            ALTER TABLE repo_version
                ADD COLUMN committed_at TIMESTAMPTZ,
                ADD COLUMN sha VARCHAR;

            CREATE INDEX repo_version_committed_at_idx
                ON repo_version (committed_at);
        |]

addDefaultForSha :: MigrationCommand
addDefaultForSha =
    MigrationScript "add-default-for-sha" $
    encodeUtf8
        [text|
            UPDATE repo_version
                SET sha = '';
            ALTER TABLE repo_version
                ALTER COLUMN sha SET NOT NULL;
        |]

addDefaultForCommittedAt :: MigrationCommand
addDefaultForCommittedAt =
    MigrationScript "add-default-for-committed-at" $
    encodeUtf8
        [text|
            UPDATE repo_version
                SET committed_at = NOW();
            ALTER TABLE repo_version
                ALTER COLUMN committed_at SET NOT NULL;
        |]

addNativeModulesToPackage :: MigrationCommand
addNativeModulesToPackage =
    MigrationScript "add-native-modules-to-package" $
    encodeUtf8
        [text|
            ALTER TABLE package
                ADD COLUMN native_modules
                    BOOL
                    DEFAULT FALSE
                    NOT NULL;
        |]

uniqueRepoVersionOnTag :: MigrationCommand
uniqueRepoVersionOnTag =
    MigrationScript "unique-repo-version-on-tag" $
    encodeUtf8
        [text|
            ALTER TABLE repo_version
                DROP CONSTRAINT repo_version_unique,
                ADD CONSTRAINT repo_version_unique
                    UNIQUE (repo, tag);

            CREATE INDEX repo_version_version_idx
                ON repo_version (version);

            CREATE INDEX repo_version_tag_idx
                ON repo_version (tag);

            CREATE INDEX repo_version_sha_idx
                ON repo_version (sha);
        |]

-- | This turns out to speed up getting the "highest version
-- for a specific repo" considerably, by about 4x.
indexRepoAndVersion :: MigrationCommand
indexRepoAndVersion =
    MigrationScript "unique-repo-and-version" $
    encodeUtf8
        [text|
            CREATE INDEX repo_version_repo_version_idx
                ON repo_version (repo, version);
        |]

-- | This is a substantial re-working of how repoVersions relate
-- to package. Basically, we re-use the repoVersionId as the primary
-- key for the other tables
rejigPackages :: MigrationCommand
rejigPackages =
    MigrationScript "rejig-packages" $
    encodeUtf8
        [text|
            CREATE TABLE package_check
                ( repo_version
                    BIGINT
                    PRIMARY KEY
                    CONSTRAINT package_check_repo_version_fk
                        REFERENCES repo_version

                , package
                    VARCHAR

                , decode_error
                    VARCHAR
                );

            INSERT INTO package_check
                (SELECT id as repo_version, package, decode_error FROM repo_version);

            ALTER TABLE repo_version
                DROP COLUMN package,
                DROP COLUMN decode_error;

            ALTER TABLE package_module
                ADD COLUMN repo_version
                    BIGINT
                    CONSTRAINT package_module_rv_fk
                        REFERENCES repo_version,
                ADD CONSTRAINT package_module_unique2
                    UNIQUE (repo_version, module_id);

            UPDATE package_module as pm
                SET repo_version = rv.id
                FROM package as p
                INNER JOIN repo_version as rv ON p.id = rv.decoded
                WHERE pm.package_id = p.id;

            ALTER TABLE package_module
                ALTER column repo_version
                    SET NOT NULL;

            CREATE INDEX package_module_rv_idx
                ON package_module (repo_version);

            ALTER TABLE package_module
                DROP COLUMN package_id;

            ALTER TABLE dependency
                ADD COLUMN repo_version
                    BIGINT
                    CONSTRAINT dependency_rv_fk
                        REFERENCES repo_version,
                ADD CONSTRAINT dependency_module_unique2
                    UNIQUE (repo_version, library);

            UPDATE dependency as d
                SET repo_version = rv.id
                FROM package as p
                INNER JOIN repo_version as rv ON p.id = rv.decoded
                WHERE d.package = p.id;

            ALTER TABLE dependency
                ALTER COLUMN repo_version
                    SET NOT NULL;

            CREATE INDEX dependency_rv_idx
                ON dependency (repo_version);

            ALTER TABLE dependency
                DROP COLUMN package;

            ALTER TABLE package
                ADD COLUMN repo_version
                    BIGINT
                    CONSTRAINT package_rv_fk
                        REFERENCES repo_version;

            UPDATE package as p
                SET repo_version = rv.id
                FROM repo_version as rv
                WHERE rv.decoded = p.id;

            ALTER TABLE repo_version
                DROP COLUMN decoded;

            ALTER TABLE package
                DROP CONSTRAINT package_pkey,
                ADD PRIMARY KEY (repo_version);
        |]

dropIdFieldFromPackage :: MigrationCommand
dropIdFieldFromPackage =
    MigrationScript "drop-id-field-from-package" $
    encodeUtf8
        [text|
            ALTER TABLE package
                DROP COLUMN id;
        |]

dropRepoFromDependency :: MigrationCommand
dropRepoFromDependency =
    MigrationScript "drop-repo-from-dependency" $
    encodeUtf8
        [text|
            ALTER TABLE dependency
                DROP COLUMN repo;
        |]

rejigPackagesAgain :: MigrationCommand
rejigPackagesAgain =
    MigrationScript "rejig-packages-again" $
    encodeUtf8
        [text|
            ALTER TABLE package_check
                DROP CONSTRAINT package_check_pkey,
                ADD CONSTRAINT package_check_unique
                    UNIQUE (repo_version),
                ADD COLUMN id
                    BIGSERIAL
                    PRIMARY KEY;

            ALTER TABLE package
                DROP CONSTRAINT package_pkey,
                ADD CONSTRAINT package_unique
                    UNIQUE (repo_version),
                ADD COLUMN id
                    BIGSERIAL
                    PRIMARY KEY;
        |]
