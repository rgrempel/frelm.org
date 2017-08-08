{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Module where

import Cheapskate.Html
import Cheapskate.Types hiding (Entity)
import Control.Monad.Trans.State as ST
import Data.ElmModule
import Data.ElmPackage
import Data.PersistSemVer
import Data.SemVer (toText)
import Data.Sequence as S
import Database.Esqueleto
import Handler.Common
import Import.App hiding (Value, groupBy, isNothing, on)
import qualified Import.App as Prelude
import Text.Blaze (toMarkup)

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
                                    <pre>
                                        <code .elm>
                                            #{source}
        |]

viewDocs :: Text -> Text -> Widget
viewDocs modName source =
    case parseModule source of
        Left err ->
            [whamlet|
                <h3>Error
                #{tshow err}
            |]
        Right elmModule ->
            case elmModuleDocs elmModule of
                Nothing ->
                    [whamlet|
                        <h3>#{modName}
                        <div .alert.alert-danger>
                            We did not parse any module docs.
                    |]
                Just blocks -> viewDocBlocks modName elmModule blocks

viewDocBlocks :: Text -> ElmModule -> Blocks -> Widget
viewDocBlocks modName elmModule blocks = do
    [whamlet|
        <h1>#{modName}
    |]
    for_ blocks $ \block ->
        case block of
            Para inlines
                -- We need to consider @docs, which won't necessasrily
                -- appear at the beginning ... could be after a SoftBreak
                -- or a LineBreak ... and then should be "active" until
                -- the next, well, we'll see (should figure out what
                -- SoftBreak and LineBreak actually mean).
             ->
                evalStateT
                    (viewPara (elmModuleDocumented elmModule) inlines)
                    (Inlines S.empty)
            _ -> toWidget $ renderBlocks markdownOptions $ pure block

data InlineState
    = AtDocs
    | Inlines (Seq Inline)

viewPara ::
       Map String (Blocks, Text)
    -> Seq Inline
    -> StateT InlineState (WidgetT App IO) ()
viewPara documented i = do
    for_ i $ \inline -> do
        current <- ST.get
        case current of
            AtDocs ->
                case inline of
                    Str ident ->
                        if ident == ","
                            then pure ()
                            else case lookup (unpack ident) documented of
                                     Just (docBlocks, source) ->
                                         lift $
                                         viewDeclarationDocs source docBlocks
                                     Nothing -> pure ()
                    Space -> pure ()
                    _ -> put $ Inlines S.empty
            Inlines inlines ->
                case viewr inlines of
                    a :> Str b
                        | b == "@" ->
                            case inline of
                                Str c
                                    | c == "docs" -> do
                                        unless (S.null a) $
                                            void $
                                            lift $
                                            toWidget $
                                            renderBlocks markdownOptions $
                                            pure $ Para a
                                        put AtDocs
                                _ -> put $ Inlines $ inlines |> inline
                    _ -> put $ Inlines $ inlines |> inline
    remaining <- ST.get
    case remaining of
        AtDocs -> pure ()
        Inlines inlines ->
            unless (S.null inlines) $
            toWidget $ renderBlocks markdownOptions $ pure $ Para inlines

-- lift $ toWidget $ renderBlocks markdownOptions $ pure $ Para inlines
viewDeclarationDocs :: Text -> Blocks -> Widget
viewDeclarationDocs source docBlocks =
    [whamlet|
        <div .panel.panel-default>
            <div .panel-heading>
                <div .declaration.elm>
                    #{source}
            <div .panel-body>
                ^{renderBlocks markdownOptions docBlocks}
    |]

markdownOptions :: Options
markdownOptions =
    Options
    { sanitize = True
    , allowRawHtml = True
    , preserveHardBreaks = False
    , debug = False
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
