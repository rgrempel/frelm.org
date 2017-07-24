{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Sitemap where

import Data.Conduit
import Database.Esqueleto
import Import.App hiding (on)
import Yesod.Sitemap

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
        mapOutput toSitemapUrl repoIds
  where
    toSitemapUrl (Value repoId) =
        SitemapUrl (RepoR repoId) Nothing (Just Daily) (Just 0.5)
