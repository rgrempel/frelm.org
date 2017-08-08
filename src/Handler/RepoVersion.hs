{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.RepoVersion where

import Cheapskate hiding (Entity)
import Data.ElmPackage
import Data.PersistSemVer
import Data.Range
import Data.SemVer (toText)
import Database.Esqueleto
import Handler.Common
import Import.App hiding (Value, groupBy, on)
import Text.Blaze (toMarkup)

getRepoVersionR :: RepoId -> Text -> Handler Html
getRepoVersionR repoId tag = do
    (r, v, pc, p, modules, dependencies) <-
        runDB $ do
            (r, v, pc, p) <-
                handle404 $
                select $
                from $ \(r `InnerJoin` v `LeftOuterJoin` pc `LeftOuterJoin` p) -> do
                    on $
                        pc ?. PackageCheckRepoVersion ==. p ?.
                        PackageRepoVersion
                    on $
                        just (v ^. RepoVersionId) ==. pc ?.
                        PackageCheckRepoVersion
                    on $ v ^. RepoVersionRepo ==. r ^. RepoId
                    where_ $
                        (v ^. RepoVersionRepo ==. val repoId) &&.
                        (v ^. RepoVersionTag ==. val tag)
                    pure (r, v, pc, p)
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
            pure (r, v, pc, p, modules, dependencies)
    accordionId <- newIdent
    defaultLayout $ do
        let repoName =
                fromMaybe (repoGitUrl $ entityVal r) $
                gitUrlToLibraryName $ repoGitUrl (entityVal r)
        setTitle $
            toMarkup $
            repoName <> " " <> (toText . repoVersionVersion . entityVal) v
        [whamlet|
            <div .container>
                <div .row>
                    <div .col-lg-6 .col-md-6 .col-sm-6 .col-xs-12>
                        <div ##{accordionId} .panel-group role="tablist" aria-multiselectable="true">
                            ^{viewPackageCheck accordionId pc p}
                            ^{viewRepoVersion accordionId v}
                            ^{viewDependencies accordionId dependencies}
                    <div .col-lg-6 .col-md-6 .col-sm-6 .col-xs-12>
                        ^{viewModules v modules}
                <div .row>
                    <div .col-lg-12>
                        ^{viewReadme pc}
        |]
        toWidget $
            [lucius|
                .panel-heading a[data-toggle=collapse]::after {
                    font-family: 'Glyphicons Halflings';
                    content: "\e114";
                    float: right;
                    color: grey;
                }

                .panel-heading a[data-toggle=collapse].collapsed::after {
                    content: "\e080";
                }
            |]

viewDependencies ::
       Text
    -> [(Entity Dependency, Entity Library, Maybe (Entity RepoVersion))]
    -> Widget
viewDependencies accordionId deps = do
    panelHeading <- newIdent
    panelBody <- newIdent
    [whamlet|
        <div .panel.panel-default>
            <div .panel-heading ##{panelHeading} role="tab">
                <h4 .panel-title>
                    <a .collapsed role="button" data-toggle="collapse" data-parent="##{accordionId}" href="##{panelBody}" aria-expanded="false" aria-controls="#{panelBody}">
                        Dependencies
            <div .panel-collapse.collapse ##{panelBody} role="tabpanel" aria-labelledby="#{panelHeading}">
                <table .table .table-striped>
                    $forall (Entity _ d, Entity _ library, depRepoVersion) <- deps
                        <tr>
                            <td .text-right>#{libraryName library}
                            <td>#{showElmPackageRange toText $ dependencyVersion d}
                            <td>
                                $forall Entity _ drv <- depRepoVersion
                                    <a href="@{RepoVersionR (repoVersionRepo drv) (repoVersionTag drv)}">
                                        <span .label.#{labelForVersion $ repoVersionVersion drv}>
                                            #{toText $ repoVersionVersion drv}
    |]

viewReverseDeps :: [(Entity Library, Entity RepoVersion)] -> Widget
viewReverseDeps deps =
    [whamlet|
        <div .panel.panel-default>
            <div .panel-heading>
                <h4 .panel-title>Reverse Dependencies
            <table .table .table-striped>
                $forall (Entity _ library, Entity _ drv) <- deps
                    <tr>
                        <td .text-right>#{libraryName library}
                        <td>
                            <a href="@{RepoVersionR (repoVersionRepo drv) (repoVersionTag drv)}">
                                <span .label.#{labelForVersion $ repoVersionVersion drv}>
                                    #{toText $ repoVersionVersion drv}
    |]

viewModules :: Entity RepoVersion -> [Entity Module] -> Widget
viewModules (Entity _ rv) modules =
    [whamlet|
        <div .panel.panel-default>
            <div .panel-heading>
                <h4 .panel-title>Modules
            <ul .list-group>
                $forall (Entity _ m) <- modules
                    <li .list-group-item>
                        <a href="@{ModuleR (repoVersionRepo rv) (repoVersionTag rv) (moduleName m)}">
                            #{moduleName m}
    |]

viewRepoVersion :: Text -> Entity RepoVersion -> Widget
viewRepoVersion accordionId (Entity _ v) = do
    panelHeading <- newIdent
    panelBody <- newIdent
    [whamlet|
        <div .panel.panel-default>
            <div .panel-heading ##{panelHeading} role="tab">
                <h4 .panel-title>
                    <a .collapsed role="button" data-toggle="collapse" data-parent="##{accordionId}" href="##{panelBody}" aria-expanded="false" aria-controls="#{panelBody}">
                        Commit
            <div .panel-collapse.collapse ##{panelBody} role="tabpanel" aria-labelledby="#{panelHeading}">
                <table .table .table-striped>
                    <tr>
                        <td .text-right>Tag
                        <td>#{repoVersionTag v}
                    <tr>
                        <td .text-right>Committed At
                        <td>#{(tshow . repoVersionCommittedAt) v}
    |]

viewPackageCheck ::
       Text -> Maybe (Entity PackageCheck) -> Maybe (Entity Package) -> Widget
viewPackageCheck accordionId pc p = do
    panelHeading <- newIdent
    panelBody <- newIdent
    [whamlet|
        <div .panel.panel-default>
            <div .panel-heading ##{panelHeading} role="tab">
                <h4 .panel-title>
                    <a role="button" data-toggle="collapse" data-parent="##{accordionId}" href="##{panelBody}" aria-expanded="true" aria-controls="#{panelBody}">
                        Package
            <div .panel-collapse.collapse.in ##{panelBody} role="tabpanel" aria-labelledby="#{panelHeading}">
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
                <div .panel.panel-default>
                    <div .panel-heading>
                        <h4 .panel-title>README
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
