{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Sitemap where

import Data.Conduit
import Data.SemVer
import Database.Esqueleto
import Import.App hiding (on)
import Yesod.Sitemap

getRobotsR :: Handler TypedContent
getRobotsR = (TypedContent typePlain . toContent) <$> robots SitemapR

getSitemapR :: Handler TypedContent
getSitemapR =
    sitemap $
    runDBSource $ do
        yield $ SitemapUrl HomeR Nothing (Just Weekly) (Just 1.0)
        yield $ SitemapUrl RecentR Nothing (Just Daily) (Just 1.0)
        yield $ SitemapUrl LibrariesR Nothing (Just Daily) (Just 1.0)
        yield $ SitemapUrl ModulesR Nothing (Just Daily) (Just 1.0)
        yield $ SitemapUrl ReposR Nothing (Just Daily) (Just 1.0)
        -- It probably makes sense to index each repo just once
        -- ... it would seem cluttered to find all the old versions,
        -- I think. Soon, the repo page will include the info for
        -- the latest version, so this is basically equivalent to
        -- indexing the latest version of each repo.
        let repoIds = selectSource $ from $ \repo -> pure $ repo ^. RepoId
        mapOutput repoToSitemapUrl repoIds
        -- For modules, we'll index the modules of the highest
        -- version for each repo.
        let pmIds =
                selectSource $
                from $ \(pm `InnerJoin` rv `InnerJoin` r `InnerJoin` m) -> do
                    on $ pm ^. PackageModuleModuleId ==. m ^. ModuleId
                    on $
                        (rv ^. RepoVersionRepo ==. r ^. RepoId) &&.
                        (just (rv ^. RepoVersionVersion) ==.
                         sub_select
                             (from $ \rv2 -> do
                                  where_ $
                                      rv2 ^. RepoVersionRepo ==. r ^. RepoId
                                  pure $ max_ $ rv2 ^. RepoVersionVersion))
                    on $ pm ^. PackageModuleRepoVersion ==. rv ^. RepoVersionId
                    pure
                        ( rv ^. RepoVersionRepo
                        , rv ^. RepoVersionVersion
                        , m ^. ModuleName)
        mapOutput packageModuleToSitemapUrl pmIds
  where
    repoToSitemapUrl (Value repoId) =
        SitemapUrl (RepoR repoId) Nothing (Just Weekly) (Just 0.8)
    packageModuleToSitemapUrl (Value repoId, Value repoVersion, Value modName) =
        SitemapUrl
            (ModuleR repoId (toText repoVersion) modName)
            Nothing
            (Just Weekly)
            (Just 0.6)
