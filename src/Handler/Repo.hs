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
                            $forall Entity _ version <- versions
                                <tr>
                                    <td>
                                        <a href=@{RepoVersionR (repoVersionRepo version) (repoVersionTag version)}>#{repoVersionTag version}
                                    <td>#{tshow $ repoVersionCommittedAt version}
        |]

handle404 :: MonadHandler m => m [a] -> m a
handle404 =
    (=<<) $ \b ->
        case b of
            initial:_ -> pure initial
            [] -> notFound

getRepoVersionR :: RepoId -> Text -> Handler Html
getRepoVersionR repoId tag = do
    (v, pc, p) <-
        handle404 $
        runDB $
        select $
        from $ \(v `LeftOuterJoin` pc `LeftOuterJoin` p) -> do
            on $ pc ?. PackageCheckRepoVersion ==. p ?. PackageRepoVersion
            on $ just (v ^. RepoVersionId) ==. pc ?. PackageCheckRepoVersion
            where_ $
                (v ^. RepoVersionRepo ==. val repoId) &&.
                (v ^. RepoVersionTag ==. val tag)
            pure (v, pc, p)
    defaultLayout
        [whamlet|
            <div .container>
                <div .row>
                    <div .col-lg-12>
                        ^{viewRepoVersion v}
                        ^{viewPackageCheck pc p}
        |]

viewRepoVersion :: Entity RepoVersion -> Widget
viewRepoVersion (Entity _ v) =
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
    |]

viewPackageCheck ::
       Maybe (Entity PackageCheck) -> Maybe (Entity Package) -> Widget
viewPackageCheck pc p =
    [whamlet|
        $case pc
            $of Nothing
                <div .alert.alert-danger>
                    We have not yet checked for the
                    <code>elm-package.json
                    file.

            $of Just (Entity _ packageCheck)
                $case packageCheckPackage packageCheck
                    $of Nothing
                        <div .alert.alert-danger>
                            We were not able to find the expected
                            <code>elm-package.json
                            file at this tag.

                    $of Just _
                        $case p
                            $of Just decoded
                                ^{viewDecodedPackage decoded}

                            $of Nothing
                                <div .alert.alert-danger>
                                    <p>
                                        We encountered the following errors
                                        when trying to decode the
                                        <code>elm-package.json
                                        file.

                                    $forall err <- packageCheckDecodeError packageCheck
                                        <p>
                                            <pre>
                                                #{err}

                                    $forall contents <- packageCheckPackage packageCheck
                                        <p>
                                            <pre>
                                                #{contents}
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
            on $ version ?. RepoVersionId ==. package ?. PackageRepoVersion
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
        setTitle "Elm Repositories"
        [whamlet|
            <div .container .#{wrapper}>
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
                            is based on the <code>repository</code>, see the
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
                <div .row>
                    <div .col-lg-12>
                        <dl>
                            $forall (repo, version, package) <- repos
                                <dt>
                                    <a href=@{RepoR (entityKey repo)}>
                                        #{(repoGitUrl . entityVal) repo}
                                <dd>
                                    $forall v <- version
                                        $forall p <- package
                                            <a href="@{RepoVersionR (repoVersionRepo $ entityVal v) (repoVersionTag $ entityVal v)}">
                                                <span .label.label-primary>#{toText $ repoVersionVersion $ entityVal v}
                                            #{packageSummary $ entityVal p}
        |]
        toWidget
            [cassius|
                .#{wrapper}
                    dt
                        margin-top: 1em
                    dd
                        margin-left: 0em
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
