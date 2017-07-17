{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Recent where

import Data.SemVer (toText)
import Data.Time.Calendar (showGregorian)
import Database.Esqueleto
import Import.App hiding (on)
import qualified Import.App as Prelude

getRecentR :: Handler Html
getRecentR = do
    result <-
        fmap
            (Prelude.groupBy
                 (Prelude.on
                      (==)
                      (\(Entity _ rv, _, _) ->
                           (utctDay . repoVersionCommittedAt) rv))) $
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
    wrapper <- newIdent
    defaultLayout $ do
        [whamlet|
            <div .container.#{wrapper}>
                <div .row>
                    <p>
                        This is a list of recently published versions.
                <div .row>
                    <div .col-lg-12>
                            $forall byDay <- result
                                $forall (Entity _ rv, Entity _ p, Entity _ l) <- safeHead byDay
                                    <h4>#{(showGregorian (utctDay (repoVersionCommittedAt rv)))}
                                    <dl>
                                        $forall (Entity rvId rv2, Entity _ p, Entity _ l) <- byDay 
                                            <dt>
                                                #{libraryName l} -
                                                <a href="@{RepoVersionR (repoVersionRepo rv) (repoVersionTag rv)}">#{(toText . repoVersionVersion) rv2}
                                            <dd>
                                                #{packageSummary p}
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
