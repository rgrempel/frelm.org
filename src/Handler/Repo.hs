{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Repo where

import Data.SemVer (toText)
import Database.Esqueleto
import Import.App hiding (groupBy, on)

getRepoR :: RepoId -> Handler Html
getRepoR repoId = do
    versions <-
        runDB $
        select $
        from $ \version -> do
            where_ $ version ^. RepoVersionRepo ==. val repoId
            orderBy [desc $ version ^. RepoVersionVersion]
            pure version
    defaultLayout
        [whamlet|
            <div .container>
                <div .row>
                    <div .col-lg-12>
                        <p>
                            Here's a list of the tags we know about in this repo.
                            We look for tags that are formatted as a Semantic Version ...
                            for instance, 1.0.0. We check for new tags about once a day.
                        <table .table .table-striped .table-responsive>
                            $forall Entity versionId version <- versions
                                <tr>
                                    <td>
                                        <a href=@{RepoVersionR versionId}>#{repoVersionTag version}
                                    <td>#{tshow $ repoVersionCommittedAt version}
        |]

handle404 :: MonadHandler m => m [a] -> m a
handle404 =
    (=<<) $ \b ->
        case b of
            initial:_ -> pure initial
            [] -> notFound

getRepoVersionR :: RepoVersionId -> Handler Html
getRepoVersionR repoVersionId = do
    (v, p) <-
        handle404 $
        runDB $
        select $
        from $ \(v `LeftOuterJoin` p) -> do
            on $ v ^. RepoVersionDecoded ==. p ?. PackageId
            where_ $ v ^. RepoVersionId ==. val repoVersionId
            pure (v, p)
    defaultLayout
        [whamlet|
            <div .container>
                <div .row>
                    <div .col-lg-12>
                        ^{viewRepoVersion v p}
        |]

viewRepoVersion :: Entity RepoVersion -> Maybe (Entity Package) -> Widget
viewRepoVersion (Entity _ v) decoded =
    [whamlet|
        <table .table .table-striped .table-responsive>
            <tr>
                <td>Tag
                <td>#{repoVersionTag v}
            <tr>
                <td>SHA
                <td>#{repoVersionSha v}
            <tr>
                <td>Commmitted At
                <td>#{(tshow . repoVersionCommittedAt) v}

        $case repoVersionPackage v
            $of Nothing
                <div .alert.alert-danger>
                    We were not able to find the expected
                    <code>elm-package.json
                    at this tag.
            $of Just package
                $case decoded
                    $of Just d
                        ^{viewDecodedPackage d}
                    $of Nothing
                        <div .alert.alert-danger>
                            <p>
                                We encountered the following errors
                                when trying to decode the
                                <code>elm-package.json
                                file.
                            $forall err <- repoVersionDecodeError v
                                <p>
                                    <pre>
                                        #{err}
                            $forall p <- repoVersionPackage v
                                <p>
                                    <pre>
                                        #{p}
    |]

viewDecodedPackage :: Entity Package -> Widget
viewDecodedPackage (Entity _ p) =
    [whamlet|
        <table .table .table-striped .table-responsive>
            <tr>
                <td>Version
                <td>#{(toText . packageVersion) p}
            <tr>
                <td>Summary
                <td>#{packageSummary p}
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
    repos <-
        runDB $
        select $
        from $ \(repo `LeftOuterJoin` version `LeftOuterJoin` package) -> do
            on $ version ?. RepoVersionDecoded ==. just (package ?. PackageId)
            on $
                (version ?. RepoVersionRepo ==. just (repo ^. RepoId)) &&.
                (version ?. RepoVersionVersion ==.
                 sub_select
                     (from $ \version2 -> do
                          where_ $
                              version2 ^. RepoVersionRepo ==. repo ^. RepoId
                          pure $ max_ $ version2 ^. RepoVersionVersion))
            orderBy [asc $ repo ^. RepoGitUrl]
            pure (repo, version, package)
    isLoggedIn <- isJust <$> maybeAuth
    wrapper <- newIdent
    defaultLayout $ do
        [whamlet|
            <div .container .#{wrapper}>
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
                            If you'd like to add a repository here manually,
                            you can do so using the form on the right, by
                            submitting a Git URL that looks something like the
                            examples. That is the URL we will use to fetch your
                            package via operations such as `git ls-remote` and
                            `git clone`.
                        <p>
                            <small>
                                One thing you might reasonably wonder is how the
                                list of "repositories" differs from the list of
                                "libraries." Tecnically, the difference is that the
                                reponsitories track the actual Git URLs we use in
                                order to fetch things. The libraries track the
                                repository <b>declared</b> in the
                                <code>elm-package.json</code> file. Normally, these
                                have a well-defined relationship, but we don't
                                entirely count on it. This is in part because I'd
                                eventually like to do something useful with the
                                <code>dependency-sources</code> used by
                                <code>elm-install</code>.
                    <div .col-md-6>
                        $if isLoggedIn
                            <p>
                                <form method=post action=@{ReposR} enctype=#{enctype}>
                                    ^{widget}
                                    <button type="submit" .btn .btn-default>Submit Git URL to monitor
                        $else
                            <p>
                                <a href="@{AuthR LoginR}">Login</a> to submit a Git URL for us to monitor.
                <div .row>
                    <div .col-lg-12>
                        <dl>
                            $forall (repo, version, package) <- repos
                                <dt>
                                    <a href=@{RepoR (entityKey repo)}>#{(repoGitUrl . entityVal) repo}
                                <dd>
                                    $forall p <- package
                                        #{(packageSummary . entityVal) p}
        |]
        toWidget
            [cassius|
                .#{wrapper}
                    dt
                        margin-top: 1em
                    dd
                        margin-left: 3em
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
