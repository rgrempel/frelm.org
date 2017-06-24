{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Repo where

import Import


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
