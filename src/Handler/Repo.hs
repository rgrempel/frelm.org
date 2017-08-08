{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Repo where

import Data.ElmPackage
import Data.PersistSemVer
import Data.SemVer (toText)
import Database.Esqueleto
import Handler.Common
import Import.App hiding (Value, groupBy, on)
import qualified Import.App as Prelude
import Text.Blaze (toMarkup)

getRepoR :: RepoId -> Handler Html
getRepoR repoId = do
    (versions, repo) <-
        runDB $ do
            repo <- get404 repoId
            versions <-
                select $
                from $ \version -> do
                    where_ $ version ^. RepoVersionRepo ==. val repoId
                    orderBy [desc $ version ^. RepoVersionVersion]
                    pure version
            pure (versions, repo)
    defaultLayout $ do
        setTitle $
            toMarkup $
            fromMaybe (repoGitUrl repo) $ gitUrlToLibraryName $ repoGitUrl repo
        [whamlet|
            <div .container>
                <div .row>
                    <div .col-lg-4 .col-md-4 .col-sm-6 .col-xs-12>
                        <div .panel.panel-default>
                            <div .panel-heading>
                                <h3 .panel-title>Tags
                            <table .table .table-striped .table-responsive>
                                $forall Entity _ version <- versions
                                    <tr>
                                        <td align="right">
                                            <a href=@{RepoVersionR (repoVersionRepo version) (repoVersionTag version)}>
                                                <span .label.#{labelForVersion $ repoVersionVersion version}>
                                                    #{toText $ repoVersionVersion version}
                                        <td>
                                            <a href=@{RepoVersionR (repoVersionRepo version) (repoVersionTag version)}>
                                                #{tshow $ utctDay $ repoVersionCommittedAt version}
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
    elmVersion <- lookupRequestedElmVersion
    (widget, enctype) <- generateFormPost submissionForm
    repos <-
        fmap
            (Prelude.groupBy
                 (Prelude.on (==) (\(Entity repoId _, _, _) -> repoId))) $
        runDB $
        select $
        from $ \(r `LeftOuterJoin` rv `LeftOuterJoin` p) -> do
            on $ rv ?. RepoVersionId ==. p ?. PackageRepoVersion
            on $
                (just (r ^. RepoId) ==. rv ?. RepoVersionRepo) &&.
                (rv ?. RepoVersionVersion ==. maxRepoVersion elmVersion r)
            orderBy [asc $ r ^. RepoGitUrl]
            pure (r, rv, p ?. PackageSummary)
    isLoggedIn <- isJust <$> maybeAuth
    repoClass <- newIdent
    versionClass <- newIdent
    repoNameClass <- newIdent
    listClass <- newIdent
    defaultLayout $ do
        setTitle "Elm Repositories"
        [whamlet|
            <div .container>
                <div .row>
                    <div .col-md-6>
                        <p>
                            This is a list of all the repositories which we
                            check for new versions.
                        <p>
                            Note that we list the repositories below according to
                            where we actually found them, not necessarily what is
                            declared in the <code>repository</code> field of an
                            <code>elm-package.json</code> file. For a list that
                            is based on the <code>repository</code> field, see the
                            <a href="@{LibrariesR}">libraries page</a>.
                    <div .col-md-6>
                        <p>
                            If you have published a package to the
                            <a href="http://package.elm-lang.org">official Elm package site</a>,
                            then your repository should appear here automatically
                            (eventually -- we check about once per day).
                        <p>
                            If you'd like to add a repository here manually,
                            you can do so using the form below, by submitting a
                            Git URL that looks something like the examples.
                            That is the URL we will use to fetch your package
                            via operations such as `git ls-remote` and `git
                            clone`.
                        $if isLoggedIn
                            <p>
                                <form method=post action=@{ReposR} enctype=#{enctype}>
                                    ^{widget}
                                    <button type="submit" .btn .btn-default>Submit Git URL to monitor
                        $else
                            <p>
                                <a href="@{AuthR LoginR}">Login</a> to submit a Git URL for us to monitor.
                <div .row .text-center>
                    ^{elmVersionWidget}
                <div .row>
                    <div .col-lg-12 .#{listClass}>
                        $forall byRepo <- repos
                            $forall (Entity repoId repo, rv, _) <- listToMaybe byRepo
                                $if Prelude.isJust rv || Prelude.isNothing elmVersion
                                    <div .#{repoClass}>
                                        <a href=@{RepoR repoId} .#{repoNameClass}>
                                            #{repoGitUrl repo}
                                        $forall (_, version, Value summary) <- byRepo
                                            $forall Entity _ v <- version
                                                <div .#{versionClass}>
                                                    <a href="@{RepoVersionR (repoVersionRepo v) (repoVersionTag v)}">
                                                        <span .label.#{labelForVersion $ repoVersionVersion v}>
                                                            #{toText $ repoVersionVersion v}
                                                    $forall s <- summary
                                                        <a href="@{RepoVersionR (repoVersionRepo v) (repoVersionTag v)}">
                                                            #{s}
                                $else
                                    <span>
        |]
        toWidget
            [cassius|
                .#{listClass}
                    margin-top: 1em

                .#{repoClass}
                    margin-top: 0.5em

                    a:visited, a:link
                        color: black

                .#{repoNameClass}
                    font-weight: bold

                .#{versionClass}
                    margin-left: 2em

                    .label
                        position: relative
                        top: -1px
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
