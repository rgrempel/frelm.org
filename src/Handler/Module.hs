{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Module where

import Data.ElmPackage
import Data.PersistSemVer
import Data.SemVer (toText)
import Database.Esqueleto
import Import.App hiding (groupBy, on)
import qualified Import.App as Prelude

handle404 :: MonadHandler m => m [a] -> m a
handle404 =
    (=<<) $ \b ->
        case b of
            initial:_ -> pure initial
            [] -> notFound

getModuleR :: RepoId -> Text -> Text -> Handler Html
getModuleR repoId tag module_ = do
    (Entity _ pm, Value license) <-
        runDB $
        handle404 $
        select $
        from $ \(rv `InnerJoin` pm `InnerJoin` m `InnerJoin` p) -> do
            on $ pm ^. PackageModuleRepoVersion ==. p ^. PackageRepoVersion
            on $ pm ^. PackageModuleModuleId ==. m ^. ModuleId
            on $ pm ^. PackageModuleRepoVersion ==. rv ^. RepoVersionId
            where_ $
                (rv ^. RepoVersionRepo ==. val repoId) &&.
                (rv ^. RepoVersionTag ==. val tag) &&.
                (m ^. ModuleName ==. val module_)
            pure (pm, p ^. PackageLicense)
    defaultLayout $ do
        addStylesheet $ StaticR highlight_js_styles_tomorrow_css
        addStylesheet $ StaticR css_highlight_js_css
        addScript $ StaticR highlight_js_highlight_pack_js
        addScript $ StaticR scripts_init_highlight_js_js
        [whamlet|
            <div .container>
                <div .row>
                    <div .col-lg-12>
                        $forall source <- packageModuleSource pm
                            <div .alert.alert-success>
                                License: #{license}
                            <pre .elm>
                                <code>
                                    #{source}
        |]

getModulesR :: Handler Html
getModulesR = do
    result <-
        fmap
            (Prelude.groupBy
                 (Prelude.on (==) (\(moduleId, _, _, _) -> moduleId))) $
        runDB $
        select $
        from $ \(m `InnerJoin` pm `InnerJoin` p `InnerJoin` rv `InnerJoin` r) -> do
            on $
                (r ^. RepoId ==. rv ^. RepoVersionRepo) &&.
                (just (rv ^. RepoVersionVersion) ==.
                 sub_select
                     (from $ \rv2 -> do
                          where_ $ rv2 ^. RepoVersionRepo ==. r ^. RepoId
                          pure $ max_ $ rv2 ^. RepoVersionVersion))
            on $ p ^. PackageRepoVersion ==. rv ^. RepoVersionId
            on $ pm ^. PackageModuleRepoVersion ==. p ^. PackageRepoVersion
            on $ m ^. ModuleId ==. pm ^. PackageModuleModuleId
            orderBy [asc $ m ^. ModuleName, desc $ rv ^. RepoVersionCommittedAt]
            pure (m ^. ModuleId, m ^. ModuleName, r ^. RepoGitUrl, rv)
    wrapper <- newIdent
    packageClass <- newIdent
    defaultLayout $ do
        setTitle "Elm Modules"
        [whamlet|
            <div .container.#{wrapper}>
                <div .row>
                    <p>
                        This is a list of all the Elm modules we know about,
                        with links to packages that implement them. You'll see
                        that there is some apparent duplication in those links
                        -- this mainly has to do with repositories that were
                        re-named at some point. We'll work on filtering out old
                        names eventually. (In some cases, there really is more
                        than one package that implements a module).
                <div .row>
                    <div .col-lg-12>
                        <dl>
                            $forall byModule <- result
                                $forall (_, Value moduleName, _, _) <- listToMaybe byModule
                                    <dt>#{moduleName}
                                    <dd>
                                        $forall (_, _, Value gitUrl, Entity _ rv) <- byModule
                                            <div .#{packageClass}>
                                                <a href="@{ModuleR (repoVersionRepo rv) (repoVersionTag rv) moduleName}">
                                                    <span .label.#{labelForVersion $ repoVersionVersion rv}>
                                                        #{(toText . repoVersionVersion) rv}
                                                    #{fromMaybe gitUrl $ gitUrlToLibraryName gitUrl}
        |]
        toWidget
            [cassius|
                .#{wrapper}
                    dt
                        margin-top: 0.5em
                    dd
                        margin-left: 2em

                .#{packageClass}
                    margin-bottom: 0.2em

                    a:link, a:visited
                        color: black

                    .label
                        position: relative
                        top: -1px
            |]
