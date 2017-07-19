{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Module where

import Data.SemVer (toText)
import Database.Esqueleto
import Import.App hiding (groupBy, on)
import qualified Import.App as Prelude

getModulesR :: Handler Html
getModulesR = do
    result <-
        fmap
            (Prelude.groupBy
                 (Prelude.on
                      (==)
                      (\(Entity moduleId _, _, _, _, _, _) -> moduleId))) $
        runDB $
        select $
        from $ \(m `InnerJoin` pm `InnerJoin` p `InnerJoin` l `InnerJoin` rv `InnerJoin` r) -> do
            on $
                (r ^. RepoId ==. rv ^. RepoVersionRepo) &&.
                (just (rv ^. RepoVersionVersion) ==.
                 sub_select
                     (from $ \rv2 -> do
                          where_ $ rv2 ^. RepoVersionRepo ==. r ^. RepoId
                          pure $ max_ $ rv2 ^. RepoVersionVersion))
            on $ p ^. PackageRepoVersion ==. rv ^. RepoVersionId
            on $ p ^. PackageLibrary ==. just (l ^. LibraryId)
            on $ pm ^. PackageModuleRepoVersion ==. p ^. PackageRepoVersion
            on $ m ^. ModuleId ==. pm ^. PackageModuleModuleId
            orderBy [asc $ m ^. ModuleName, desc $ rv ^. RepoVersionCommittedAt]
            pure (m, pm, l, p, rv, r)
    wrapper <- newIdent
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
                                $forall (Entity _ moduleRec, _, _, _, _ ,_) <- safeHead byModule
                                    <dt>#{moduleName moduleRec}
                                    <dd>
                                        $forall (_, _, Entity _ library, _, Entity rvId rv, Entity repoId _) <- byModule
                                            <div>
                                                #{libraryName library} /
                                                <a href="@{RepoVersionR (repoVersionRepo rv) (repoVersionTag rv)}">#{(toText . repoVersionVersion) rv}
        |]
        toWidget
            [cassius|
                .#{wrapper}
                    dt
                        margin-top: 0.5em
                    dd
                        margin-left: 3em
            |]

safeHead :: [a] -> Maybe a
safeHead a =
    case a of
        x:xs -> Just x
        [] -> Nothing
