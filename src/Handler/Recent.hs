{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Recent where

import Data.SemVer (toText)
import Database.Esqueleto as E
import Import.App

getRecentR :: Handler Html
getRecentR = do
    result <-
        runDB $
        select $
        distinct $
        from $ \(repoVersion `InnerJoin` package `InnerJoin` library) -> do
            E.on (package ^. PackageLibrary E.==. just (library ^. LibraryId))
            E.on
                (repoVersion ^. RepoVersionDecoded E.==.
                 just (package ^. PackageId))
            orderBy [desc $ repoVersion ^. RepoVersionCommittedAt]
            limit 1000
            pure (repoVersion, package, library)
    defaultLayout
        [whamlet|
            <div .container>
                <div .row>
                    <h3>Recently published
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
