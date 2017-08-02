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
import Handler.Common
import Import.App hiding (Value, groupBy, isNothing, on)
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
