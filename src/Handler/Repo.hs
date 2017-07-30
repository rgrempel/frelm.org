{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Repo where

import Cheapskate hiding (Entity)
import Data.PersistSemVer
import Data.Range
import Data.SemVer (Version, toText)
import Database.Esqueleto
import Handler.Common
import Import.App hiding (Value, groupBy, on)
import qualified Import.App as Prelude

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

handle404 :: MonadHandler m => m [a] -> m a
handle404 =
    (=<<) $ \b ->
        case b of
            initial:_ -> pure initial
            [] -> notFound

getRepoVersionR :: RepoId -> Text -> Handler Html
getRepoVersionR repoId tag = do
    (v, pc, p, modules, dependencies) <-
        runDB $ do
            (v, pc, p) <-
                handle404 $
                select $
                from $ \(v `LeftOuterJoin` pc `LeftOuterJoin` p) -> do
                    on $
                        pc ?. PackageCheckRepoVersion ==. p ?.
                        PackageRepoVersion
                    on $
                        just (v ^. RepoVersionId) ==. pc ?.
                        PackageCheckRepoVersion
                    where_ $
                        (v ^. RepoVersionRepo ==. val repoId) &&.
                        (v ^. RepoVersionTag ==. val tag)
                    pure (v, pc, p)
            modules <-
                select $
                from $ \(pm `InnerJoin` m) -> do
                    on $ m ^. ModuleId ==. pm ^. PackageModuleModuleId
                    where_ $
                        pm ^. PackageModuleRepoVersion ==. val (entityKey v)
                    orderBy [asc $ m ^. ModuleName]
                    pure m
            dependencies <-
                select $
                from $ \(d `InnerJoin` l `LeftOuterJoin` depRepo `LeftOuterJoin` depVersion) -> do
                    on $
                        (depVersion ?. RepoVersionRepo ==. depRepo ?. RepoId) &&.
                        (depVersion ?. RepoVersionVersion ==.
                         sub_select
                             (from $ \depVersion2 -> do
                                  where_ $
                                      (just (depVersion2 ^. RepoVersionRepo) ==.
                                       (depRepo ?. RepoId)) &&.
                                      valueInRange
                                          (depVersion2 ^. RepoVersionVersion)
                                          (d ^. DependencyVersion)
                                  pure $
                                      max_ $ depVersion2 ^. RepoVersionVersion))
                    on $
                        just
                            (val "https://github.com/" ++. (l ^. LibraryName) ++.
                             val ".git") ==.
                        (depRepo ?. RepoGitUrl)
                    on $ d ^. DependencyLibrary ==. l ^. LibraryId
                    where_ $ d ^. DependencyRepoVersion ==. val (entityKey v)
                    pure (d, l, depRepo, depVersion)
            pure (v, pc, p, modules, dependencies)
    defaultLayout
        [whamlet|
            <div .container>
                <div .row>
                    ^{viewPackageCheck pc p}
                    ^{viewRepoVersion v}
                    ^{viewModules v modules}
                    ^{viewDependencies dependencies}
                <div .row>
                    ^{viewReadme pc}
        |]

viewDependencies ::
       [( Entity Dependency
        , Entity Library
        , Maybe (Entity Repo)
        , Maybe (Entity RepoVersion))]
    -> Widget
viewDependencies deps =
    [whamlet|
        <div .col-lg-6 .col-md-6 .col-sm-6 .col-xs-12>
            <div .panel.panel-default>
                <div .panel-heading>
                    <h3 .panel-title>Dependencies
                <table .table .table-striped>
                    $forall (Entity _ d, Entity _ library, _, depRepoVersion) <- deps
                        <tr>
                            <td .text-right>#{libraryName library}
                            <td>#{showElmPackageRange toText $ dependencyVersion d}
                            <td>
                                $forall Entity _ drv <- depRepoVersion
                                    <a href="@{RepoVersionR (repoVersionRepo drv) (repoVersionTag drv)}">
                                        <span .label.#{labelForVersion $ repoVersionVersion drv}>
                                            #{toText $ repoVersionVersion drv}
    |]

viewModules :: Entity RepoVersion -> [Entity Module] -> Widget
viewModules (Entity _ rv) modules =
    [whamlet|
        <div .col-lg-6 .col-md-6 .col-sm-6 .col-xs-12>
            <div .panel.panel-default>
                <div .panel-heading>
                    <h3 .panel-title>Modules
                <ul .list-group>
                    $forall (Entity _ m) <- modules
                        <li .list-group-item>
                            <a href="@{ModuleR (repoVersionRepo rv) (repoVersionTag rv) (moduleName m)}">
                                #{moduleName m}
    |]

viewRepoVersion :: Entity RepoVersion -> Widget
viewRepoVersion (Entity _ v) =
    [whamlet|
        <div .col-lg-6 .col-md-6 .col-sm-6 .col-xs-12>
            <div .panel.panel-default>
                <div .panel-heading>
                    <h3 .panel-title>Commit
                <table .table .table-striped>
                    <tr>
                        <td .text-right>Tag
                        <td>#{repoVersionTag v}
                    <tr>
                        <td .text-right>Committed At
                        <td>#{(tshow . repoVersionCommittedAt) v}
    |]

viewPackageCheck ::
       Maybe (Entity PackageCheck) -> Maybe (Entity Package) -> Widget
viewPackageCheck pc p =
    [whamlet|
        <div .col-lg-6 .col-md-6 .col-sm-6 .col-xs-12>
            <div .panel.panel-default>
                <div .panel-heading>
                    <h3 .panel-title>Package
                <div .panel-body>
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
                                $of Just contents
                                    $forall (Entity _ decoded) <- p
                                        <div>#{packageSummary decoded}
                                    $forall err <- packageCheckDecodeError packageCheck
                                        <div .alert.alert-danger>
                                            <p>
                                                We encountered the following errors
                                                when trying to decode the
                                                <code>elm-package.json
                                                file.
                                            <pre>
                                                #{err}
                                                --
                                                #{contents}
                $forall decoded <- p
                    ^{viewDecodedPackage decoded}
    |]

viewReadme :: Maybe (Entity PackageCheck) -> Widget
viewReadme pc =
    [whamlet|
        $forall Entity _ packageCheck <- pc
            $forall readme <- packageCheckReadme packageCheck
                <div .col-lg-12>
                    <div .panel.panel-default>
                        <div .panel-heading>
                            <h3 .panel-title>README
                        <div .panel-body>
                            ^{viewMarkdown readme}
    |]

viewMarkdown :: Text -> Widget
viewMarkdown text = do
    addStylesheet $ StaticR highlight_js_styles_tomorrow_css
    addStylesheet $ StaticR css_highlight_js_css
    addScript $ StaticR highlight_js_highlight_pack_js
    addScript $ StaticR scripts_init_highlight_js_js
    toWidget $ \_ -> (toHtml . markdown def) text

viewDecodedPackage :: Entity Package -> Widget
viewDecodedPackage (Entity _ p) =
    [whamlet|
        <table .table .table-striped .table-responsive>
            <tr>
                <td .text-right>version
                <td>#{(toText . packageVersion) p}
            <tr>
                <td .text-right>license
                <td>#{packageLicense p}
            <tr>
                <td .text-right>native-modules
                <td>#{packageNativeModules p}
            $forall elmVersion <- packageElmVersion p
                <tr>
                    <td .text-right>elm-version
                    <td>#{showElmPackageRange toText elmVersion}
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
        fmap
            (Prelude.groupBy
                 (Prelude.on (==) (\(Entity repoId _, _, _, _) -> repoId))) $
        runDB $
        select $
        from $ \((r `InnerJoin` rr) `LeftOuterJoin` rv `LeftOuterJoin` p) -> do
            on $ rv ?. RepoVersionId ==. p ?. PackageRepoVersion
            on $
                (just (r ^. RepoId) ==. rv ?. RepoVersionRepo) &&.
                (rr ^. RepoRangeRepoVersion ==. rv ?. RepoVersionVersion)
            on $ r ^. RepoId ==. rr ^. RepoRangeRepoId
            orderBy [asc $ r ^. RepoGitUrl]
            pure (r, rv, p ?. PackageSummary, rr ^. RepoRangeElmVersion)
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
                            $forall (Entity repoId repo, _, _, _) <- listToMaybe byRepo
                                <div .#{repoClass}.#{elmVersionsForRepo byRepo}>
                                    <a href=@{RepoR repoId} .#{repoNameClass}>
                                        #{repoGitUrl repo}
                                    $forall (_, version, Value summary, Value elmVersion) <- byRepo
                                        $forall Entity _ v <- version
                                            <div .#{versionClass} .#{displayIfElmVersion elmVersion}>
                                                <a href="@{RepoVersionR (repoVersionRepo v) (repoVersionTag v)}">
                                                    <span .label.#{labelForVersion $ repoVersionVersion v}>
                                                        #{toText $ repoVersionVersion v}
                                                $forall s <- summary
                                                    <a href="@{RepoVersionR (repoVersionRepo v) (repoVersionTag v)}">
                                                        #{s}
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

elmVersionsForRepo :: [(a, b, c, Value (Maybe Version))] -> Text
elmVersionsForRepo =
    unwords . fmap (\(_, _, _, Value version) -> displayIfElmVersion version)

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
