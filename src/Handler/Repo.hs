{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Repo where

import Data.SemVer (toText)
import Import.App

data SubmissionForm = SubmissionForm
    { gitUrl :: Text
    } deriving (Show)

-- Or `Form Submission`
submissionForm :: Html -> MForm Handler (FormResult SubmissionForm, Widget)
submissionForm =
    renderDivs $ SubmissionForm <$> areq textField "Git URL" Nothing

getRepoR :: RepoId -> Handler Html
getRepoR repoId = do
    (repo, versions) <-
        runDB $ do
            r <- get404 repoId
            v <-
                selectList [RepoVersionRepo ==. repoId] [Asc RepoVersionVersion]
            pure (r, v)
    defaultLayout
        [whamlet|
            <pre>#{show repo}

            <h4>Versions
                $forall Entity versionId version <- versions
                    <div>
                        <a href=@{RepoVersionR versionId}>#{toText $ repoVersionVersion version}
        |]

getRepoVersionR :: RepoVersionId -> Handler Html
getRepoVersionR repoVersionId = do
    (repoVersion, repo) <-
        runDB $
            -- TODO: Should do this in one query
         do
            v <- get404 repoVersionId
            r <- get404 (repoVersionRepo v)
            pure (v, r)
    defaultLayout
        [whamlet|
            <pre>#{show repo}

            <h4>Version #{toText $ repoVersionVersion repoVersion}
        |]

getReposR :: Handler Html
getReposR = do
    (widget, enctype) <- generateFormPost submissionForm
    repos <- runDB $ selectList [] []
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
    ((result, _), _) <- runFormPost submissionForm
    case result of
        FormSuccess submittedForm -> do
            currentUser <- requireAuthId
            let repo =
                    Repo
                    { repoGitUrl = gitUrl submittedForm
                    , repoSubmittedBy = Just currentUser
                    }
            repoId <- runDB (insert repo)
            setMessage $ toHtml ("Repo saved" :: Text)
            redirect $ RepoR repoId
        FormMissing -> do
            setMessage $ toHtml ("Form data was missing" :: Text)
            redirect ReposR
        FormFailure err -> do
            setMessage $
                toHtml ("Invalid input, let's try again: " ++ tshow err)
            redirect ReposR
