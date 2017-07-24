{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Recent where

import Data.PersistSemVer
import Data.SemVer
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
            on $ package ^. PackageLibrary ==. just (library ^. LibraryId)
            on $ repoVersion ^. RepoVersionId ==. package ^. PackageRepoVersion
            orderBy [desc $ repoVersion ^. RepoVersionCommittedAt]
            limit 1000
            pure (repoVersion, package, library)
    wrapper <- newIdent
    defaultLayout $ do
        setTitle "Recent Elm Modules"
        [whamlet|
            <div .container.#{wrapper}>
                <div .row>
                    <p>
                        This is a list of recently published Elm packages. The
                        identifiers listed below are derived from the
                        <code>repository</code> field in the package's
                        <code>elm-package.json</code> file, rather than the
                        repository where we actually found the package. If you
                        want to know the actual location of the repository,
                        follow the package link.
                    <p>
                        We also have a
                        <a href="https://twitter.com/frelmorg">frelmorg
                        twitter page, where we tweet when we see a new version
                        of a package.
                <div .row>
                    <div .col-lg-12>
                        $forall byDay <- result
                            $forall (Entity _ rvHead, _, _) <- listToMaybe byDay
                                <h4>#{showGregorian $ utctDay $ repoVersionCommittedAt rvHead}
                                <dl>
                                    $forall (Entity _ rv, Entity _ p, Entity _ l) <- byDay
                                        <dt>
                                            #{libraryName l}
                                        <dd>
                                            <a href="@{RepoVersionR (repoVersionRepo rv) (repoVersionTag rv)}">
                                                <span .label.#{labelForVersion $ repoVersionVersion rv}>
                                                    #{toText $ repoVersionVersion rv}
                                            #{packageSummary p}
        |]
        toWidget
            [cassius|
                .#{wrapper}
                    dt
                        margin-top: 0.5em
                    dd
                        margin-left: 2em
            |]
