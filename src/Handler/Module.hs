{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Module where

import AST.Declaration
import qualified AST.Module as ElmModule
import Cheapskate.Html
import qualified Cheapskate.Types as Markdown
import Data.ElmPackage
import Data.PersistSemVer
import Data.SemVer (toText)
import Database.Esqueleto
import Handler.Common
import Import.App hiding (Value, groupBy, isNothing, on)
import qualified Import.App as Prelude
import Parse.Helpers (iParse)
import Parse.Module (elmModule)
import Reporting.Annotation
import Text.Blaze (toMarkup)

handle404 :: MonadHandler m => m [a] -> m a
handle404 =
    (=<<) $ \b ->
        case b of
            initial:_ -> pure initial
            [] -> notFound

getModuleR :: RepoId -> Text -> Text -> Handler Html
getModuleR repoId tag module_ = do
    (Entity _ pm, Value license, Value gitUrl, Value modName, Value version) <-
        runDB $
        handle404 $
        select $
        from $ \(r `InnerJoin` rv `InnerJoin` pm `InnerJoin` m `InnerJoin` p) -> do
            on $ pm ^. PackageModuleRepoVersion ==. p ^. PackageRepoVersion
            on $ pm ^. PackageModuleModuleId ==. m ^. ModuleId
            on $ pm ^. PackageModuleRepoVersion ==. rv ^. RepoVersionId
            on $ rv ^. RepoVersionRepo ==. r ^. RepoId
            where_ $
                (rv ^. RepoVersionRepo ==. val repoId) &&.
                (rv ^. RepoVersionTag ==. val tag) &&.
                (m ^. ModuleName ==. val module_)
            pure
                ( pm
                , p ^. PackageLicense
                , r ^. RepoGitUrl
                , m ^. ModuleName
                , rv ^. RepoVersionVersion)
    docsTab <- newIdent
    sourceTab <- newIdent
    defaultLayout $ do
        addStylesheet $ StaticR highlight_js_styles_tomorrow_css
        addStylesheet $ StaticR css_highlight_js_css
        addScript $ StaticR highlight_js_highlight_pack_js
        addScript $ StaticR scripts_init_highlight_js_js
        let repoName = fromMaybe gitUrl $ gitUrlToLibraryName gitUrl
        setTitle $
            toMarkup $
            modName <> " (" <> repoName <> " " <> toText version <> ")"
        [whamlet|
            <div .container>
                $maybe source <- packageModuleSource pm
                    <div .row>
                        <div .col-lg-12>
                            <ul .nav.nav-tabs role="tablist">
                                <li role="presentation" class="active">
                                    <a href="##{docsTab}" aria-controls="#{docsTab}" role="tab" data-toggle="tab">
                                        Docs
                                <li role="presentation">
                                    <a href="##{sourceTab}" aria-controls="#{sourceTab}" role="tab" data-toggle="tab">
                                        Source
                                <li>
                                    <span .label.label-success>
                                        #{license}
                            <div .tab-content>
                                <div ##{docsTab} .tab-pane.active role="tabpanel">
                                    ^{viewDocs modName source}
                                <div ##{sourceTab} .tab-pane role="tabpanel">
                                    <pre .elm>
                                        <code>
                                            #{source}
        |]

viewDocs :: Text -> Text -> Widget
viewDocs modName source = do
    let parsed = iParse elmModule (unpack source)
    case parsed of
        Left err ->
            [whamlet|
                <h3>Error
                #{tshow err}
            |]
        Right (ElmModule.Module _ _ (A _ docs) _ body) -> do
            case docs of
                Nothing ->
                    [whamlet|
                        <h3>#{modName}
                        <div .alert.alert-danger>
                            We did not parse any module docs.
                    |]
                Just blocks -> do
                    [whamlet|
                        <h3>#{modName}
                    |]
                    toWidget $ renderBlocks markdownOptions blocks
            for_ body $ \decl ->
                case decl of
                    DocComment blocks ->
                        toWidget $ renderBlocks markdownOptions blocks
                    BodyComment _ -> pure ()
                    Decl (A _ _) -> pure ()

markdownOptions :: Markdown.Options
markdownOptions =
    Markdown.Options
    { Markdown.sanitize = True
    , Markdown.allowRawHtml = True
    , Markdown.preserveHardBreaks = False
    , Markdown.debug = False
    }

getModulesR :: Handler Html
getModulesR = do
    elmVersion <- lookupRequestedElmVersion
    result <-
        fmap
            (Prelude.groupBy
                 (Prelude.on (==) (\(moduleId, _, _, _) -> moduleId))) $
        runDB $
        select $
        from $ \(r `InnerJoin` rv `InnerJoin` p `InnerJoin` pm `InnerJoin` m) -> do
            on $ m ^. ModuleId ==. pm ^. PackageModuleModuleId
            on $ pm ^. PackageModuleRepoVersion ==. p ^. PackageRepoVersion
            on $ p ^. PackageRepoVersion ==. rv ^. RepoVersionId
            on $
                (r ^. RepoId ==. rv ^. RepoVersionRepo) &&.
                (just (rv ^. RepoVersionVersion) ==. maxRepoVersion elmVersion r)
            orderBy [asc $ m ^. ModuleName, asc $ r ^. RepoGitUrl]
            pure (m ^. ModuleId, m ^. ModuleName, r ^. RepoGitUrl, rv)
    moduleClass <- newIdent
    moduleNameClass <- newIdent
    packageClass <- newIdent
    defaultLayout $ do
        setTitle "Elm Modules"
        [whamlet|
            <div .container>
                <div .row>
                    <p>
                        This is a list of all the Elm modules we know about,
                        with links to packages that implement them. You'll see
                        that there is some apparent duplication in those links
                        -- this mainly has to do with repositories that were
                        re-named at some point. We'll work on filtering out old
                        names eventually. (In some cases, there really is more
                        than one package that implements a module).
                <div .row .text-center>
                    ^{elmVersionWidget}
                <div .row>
                    <div .col-lg-12>
                        $forall byModule <- result
                            $forall (_, Value moduleName, _, _) <- listToMaybe byModule
                                <div .#{moduleClass}>
                                    <div .#{moduleNameClass}>#{moduleName}
                                    $forall (_, _, Value gitUrl, Entity _ rv) <- byModule
                                        <div .#{packageClass}>
                                            <a href="@{ModuleR (repoVersionRepo rv) (repoVersionTag rv) moduleName}">
                                                <span .label.#{labelForVersion $ repoVersionVersion rv}>
                                                    #{(toText . repoVersionVersion) rv}
                                                #{fromMaybe gitUrl $ gitUrlToLibraryName gitUrl}
        |]
        toWidget
            [cassius|
                .#{moduleClass}
                    margin-top: 0.5em

                .#{moduleNameClass}
                    font-weight: bold

                .#{packageClass}
                    margin-left: 2em
                    margin-bottom: 0.2em

                    a:link, a:visited
                        color: black

                    .label
                        position: relative
                        top: -1px
            |]
