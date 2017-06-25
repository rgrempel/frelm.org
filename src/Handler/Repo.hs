{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Repo where

import Data.SemVer (Version, fromText, toText)
import Import
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
    repo <-
        runDB $ get404 repoId

    defaultLayout
        [whamlet|
            <pre>#{show repo}
            <form method=post action=@{RepoVersionsR repoId}>
                <button>Check versions
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
    ((result, widget), enctype) <-
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
            setMessage $ toHtml ("Invalid input, let's try again" :: Text)
            redirect ReposR


{- This is a Restful URL where we'd ordinarily post to create a
 - new version for the repo. But, we don't want to do that manually,
 - so we'll use it to ask the system to check for versions.
 -}
postRepoVersionsR :: RepoId -> Handler Html
postRepoVersionsR repoId = do
    repo <-
        runDB $ get404 repoId

    ( exitCode, stdOut, stdErr ) <-
        liftIO $
            readProcessWithExitCode "git"
                [ "ls-remote"
                , "--tags"
                , "--quiet"
                , "--refs"
                , unpack $ repoGitUrl repo
                ]
                ""

    let tagsAndVersions =
            ( rights
            . fmap (parse lsRemoteLine "line")
            . lines
            )
            stdOut

    -- TODO: This would probably be better as a single query,
    -- returning a list of existing entries.
    results <-
        forM tagsAndVersions $ \(tag, version) ->
            runDB $ do
                existing <-
                    getBy (UniqueRepoVersion repoId version)

                case existing of
                    Just _ ->
                        pure (Right version)

                    Nothing -> do
                        insert $ RepoVersion
                            { repoVersionRepo = repoId
                            , repoVersionTag = pack tag
                            , repoVersionVersion = version
                            }

                        pure (Left version)

    let
        newVersions =
            lefts results

        oldVersions =
            rights results

    setMessage $ toHtml $
        "Checked for versions.\n\n I found these new versions\n\n " ++
        show (toText <$> newVersions) ++
        "\n\nAnd these versions I already knew about\n\n" ++
        show (toText <$> oldVersions)

    redirect $
        RepoR repoId


sha40 :: Parsec String () String
sha40 =
    Parsec.count 40 Parsec.hexDigit


tag :: Parsec String () (String, Version)
tag = do
    string "refs/tags/"
    tag <- Parsec.many anyChar
    eof

    case fromText $ pack tag of
        Right version ->
            return (tag, version)

        Left err ->
            unexpected err


lsRemoteLine :: Parsec String () (String, Version)
lsRemoteLine = do
    sha40
    tab
    tag
