{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Repo where

import Data.SemVer (toText)
import Import.App

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

data SubmissionForm = SubmissionForm
    { gitUrl :: Text
    } deriving (Show)

submissionForm :: Form SubmissionForm
submissionForm =
    renderBootstrap3 BootstrapBasicForm $
    SubmissionForm <$> areq textField (bfs ("Git URL" :: Text)) Nothing

getReposR :: Handler Html
getReposR = do
    (widget, enctype) <- generateFormPost submissionForm
    repos <- runDB $ selectList [] [Asc RepoGitUrl]
    defaultLayout
        [whamlet|
            <div .container>
                <div .row>
                    <div .col-sm-12>
                        <h3>Repositories
                <div .row>
                    <div .col-md-6>
                        <p>
                            This is a list of all the repositories which
                            we check for new package versions.
                        <p>
                            If you have published a package to the official Elm package
                            manager, then your repository should appear here automatically
                            (eventually -- we check about once per day).
                        <p>
                            If you'd like to add a repository here manually, you can
                            do so using the form below, by submitting a Git URL that
                            looks something like the examples. That is the URL
                            we will use to fetch your package via operations such
                            as `git ls-remote` and `git clone`.

                    <div .col-md-6>
                        <form method=post action=@{ReposR} enctype=#{enctype}>
                            ^{widget}
                            <button type="submit" .btn .btn-default>Submit Git URL
                        <p>
                <div .row>
                    <div .col-lg-12>
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
