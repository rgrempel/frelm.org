{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Repo where

import Import
import System.Process (readProcessWithExitCode)


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
                , unpack $ repoGitUrl repo
                ]
                ""

    setMessage $ toHtml $
        "Checked versions: Here's what I got: <pre>" ++ stdOut ++ "</pre>"

    redirect $
        RepoR repoId
