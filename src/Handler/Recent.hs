{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Recent where

import Data.SemVer (toText)
import Database.Esqueleto
import Import.App hiding (on)

getRecentR :: Handler Html
getRecentR = do
    result <-
        runDB $
        select $
        distinct $
        from $ \(repoVersion `InnerJoin` package `InnerJoin` library) -> do
            on (package ^. PackageLibrary ==. just (library ^. LibraryId))
            on
                (repoVersion ^. RepoVersionDecoded ==.
                 just (package ^. PackageId))
            orderBy [desc $ repoVersion ^. RepoVersionCommittedAt]
            limit 1000
            pure (repoVersion, package, library)
    defaultLayout
        [whamlet|
            <div .container>
                <div .row>
                    <p>
                        This is a list of recently published versions.
                <div .row>
                    <div .col-lg-12>
                        <dl>
                            $forall (Entity _ rv, Entity _ p, Entity _ l) <- result 
                                <dt>
                                    #{libraryName l} - #{(toText . repoVersionVersion) rv} - #{(tshow . repoVersionCommittedAt) rv}
                                <dd>
                                    #{packageSummary p}
        |]
