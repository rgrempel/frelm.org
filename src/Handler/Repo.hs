{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Repo where

import Data.SemVer (Version, fromText, toText)
import GHC.IO.Exception (ExitCode)
import Import.App
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Text.Parsec as Parsec


data SubmissionForm = SubmissionForm
    { gitUrl :: Text
    } deriving Show


-- Or `Form Submission`
submissionForm :: Html -> MForm Handler (FormResult SubmissionForm, Widget)
submissionForm =
    renderDivs $ SubmissionForm
        <$> areq textField "Git URL" Nothing


getRepoR :: RepoId -> Handler Html
getRepoR repoId = do
    (repo, versions) <-
        runDB $ do
            r <-
                get404 repoId

            v <-
                selectList
                    [ RepoVersionRepo ==. repoId ]
                    [ Asc RepoVersionVersion ]

            pure (r, v)

    defaultLayout
        [whamlet|
            <pre>#{show repo}
            <form method=post action=@{RepoVersionsR repoId}>
                <button>Check versions

            <h4>Versions
                $forall Entity versionId version <- versions
                    <div>
                        <a href=@{RepoVersionR versionId}>#{toText $ repoVersionVersion version}
        |]


getRepoVersionR :: RepoVersionId -> Handler Html
getRepoVersionR repoVersionId = do
    ( repoVersion, repo ) <-
        runDB $ do
            -- TODO: Should do this in one query
            v <-
                get404 repoVersionId

            r <-
                get404 (repoVersionRepo v)

            pure (v, r)

    defaultLayout
        [whamlet|
            <pre>#{show repo}

            <h4>Version #{toText $ repoVersionVersion repoVersion}
        |]


getReposR :: Handler Html
getReposR = do
    (widget, enctype) <-
        generateFormPost submissionForm

    repos <-
        runDB $ selectList [] []

    defaultLayout
        [whamlet|
            <p>Submit a GIT URL
                <form method=post action=@{ReposR} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit

            <h4>Repos
                $forall Entity repoId repo <- repos
                    <div>
                        <a href=@{RepoR repoId}>#{repoGitUrl repo}
        |]


postReposR :: Handler Html
postReposR = do
    ((result, _), _) <-
        runFormPost submissionForm

    case result of
        FormSuccess submittedForm -> do
            currentUser <-
                requireAuthId

            let
                repo = Repo
                    { repoGitUrl =
                        gitUrl submittedForm

                    , repoSubmittedBy =
                        Just currentUser
                    }

            repoId <-
                runDB (insert repo)

            setMessage $
                toHtml ("Repo saved" :: Text)

            redirect $
                RepoR repoId

        FormMissing -> do
            setMessage $ toHtml ("Form data was missing" :: Text )
            redirect ReposR

        FormFailure err -> do
            setMessage $ toHtml ("Invalid input, let's try again: " ++ tshow err)
            redirect ReposR


{- This is a Restful URL where we'd ordinarily post to create a
 - new version for the repo. But, we don't want to do that manually,
 - so we'll use it to ask the system to check for versions.
 -}
postRepoVersionsR :: RepoId -> Handler Html
postRepoVersionsR repoId = do
    runDB $
        checkTags repoId

    redirect $
        RepoR repoId


checkTags :: RepoId -> YesodDB App ()
checkTags repoId = do
    mRepo <-
        get repoId

    forM_ mRepo $ \repo -> do
        knownVersions :: [Version] <-
            (fmap (repoVersionVersion . entityVal)) <$>
                selectList
                    [ RepoVersionRepo ==. repoId ]
                    [ Asc RepoVersionVersion ]

        fetchedVersions :: [(Version, String)] <-
            liftIO $
                fetchGitTags (repoGitUrl repo)

        let
            newVersions =
                filter (\a -> notElem (fst a) knownVersions) fetchedVersions

        unless (null newVersions) $
            withSystemTempDirectory "git-clone" $ \basedir -> do
                gitDir <-
                    cloneGitRepo (repoGitUrl repo) basedir

                forM_ newVersions $
                    \(version, tag) ->
                        void $ checkNewTag repoId gitDir version tag


checkNewTag :: RepoId -> FilePath -> Version -> String -> YesodDB App RepoVersionId
checkNewTag repoId gitDir version tag = do
    -- TODO: Care about result
    void $
        checkoutGitRepo gitDir tag

    package <-
        liftIO $
            -- TODO: Here and elsewhere, consider exceptions!
            readFile $ gitDir </> "elm-package.json"

    insert $
        RepoVersion
            { repoVersionRepo = repoId
            , repoVersionTag = pack tag
            , repoVersionVersion = version
            , repoVersionPackage = pack package
            }


checkoutGitRepo :: (MonadIO m, MonadLogger m) => FilePath -> String -> m ExitCode
checkoutGitRepo gitDir tag = do
    -- TODO: Do something with exitCode etc.
    ( exitCode, _, _ ) <-
        liftIO $
            readProcessWithExitCode "git"
                [ "-C"
                , gitDir
                , "checkout"
                , "--quiet"
                , "--detach"
                , tag
                ]
                ""

    pure exitCode


cloneGitRepo :: (MonadLogger m, MonadIO m) => Text -> FilePath -> m FilePath
cloneGitRepo url toPath = do
    -- TODO: Do something with exitCode etc.
    _ <-
        liftIO $
            readProcessWithExitCode "git"
                [ "-C"
                , toPath
                , "clone"
                , "--depth"
                , "1"
                , "--quiet"
                , "--no-single-branch"
                , unpack url
                , "git-clone"
                ]
                ""

    pure $ toPath </> "git-clone"


fetchGitTags :: Text -> IO [(Version, String)]
fetchGitTags url = do
    -- TODO: Check exitCode etc.
    ( _, stdOut, _ ) <-
        readProcessWithExitCode "git"
            [ "ls-remote"
            , "--tags"
            , "--quiet"
            , "--refs"
            , unpack url
            ]
            ""

    pure $
        ( rights
        . fmap (parse parseTagAndVersion "line")
        . lines
        )
        stdOut


parseTagAndVersion :: Parsec String () (Version, String)
parseTagAndVersion =
    sha40 *> tab *> parseTag

    where
        sha40 =
            Parsec.count 40 Parsec.hexDigit

        parseTag = do
            tag <-
                string "refs/tags/"
                *> Parsec.many anyChar
                <* eof

            case fromText $ pack tag of
                Right version ->
                    pure (version, tag)

                Left err ->
                    unexpected err
