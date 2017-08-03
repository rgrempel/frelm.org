{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Recent where

import Data.ElmPackage
import Data.PersistSemVer
import Data.Range
import Data.SemVer
import Data.Time.Calendar (showGregorian)
import Database.Esqueleto
import Handler.Common
import Import.App hiding (Value, isNothing, on)
import qualified Import.App as Prelude

getRecentR :: Handler Html
getRecentR = do
    elmVersion <- lookupRequestedElmVersion
    result <-
        runDB $ do
            result <-
                fmap
                    (Prelude.groupBy
                         (Prelude.on
                              (==)
                              (\(Entity _ rv, _, _) ->
                                   (utctDay . repoVersionCommittedAt) rv))) $
                select $
                from $ \(r `InnerJoin` rv `InnerJoin` p) -> do
                    on $ rv ^. RepoVersionId ==. p ^. PackageRepoVersion
                    on $ r ^. RepoId ==. rv ^. RepoVersionRepo
                    where_ $
                        justValueInRange elmVersion (p ^. PackageElmVersion)
                    orderBy [desc $ rv ^. RepoVersionCommittedAt]
                    limit 1000
                    pure (rv, p ^. PackageSummary, r ^. RepoGitUrl)
            pure result
    packageClass <- newIdent
    packageSummaryClass <- newIdent
    libraryNameClass <- newIdent
    dayClass <- newIdent
    dayNameClass <- newIdent
    defaultLayout $ do
        setTitle "Recent Elm Packages"
        [whamlet|
            <div .container>
                <div .row>
                    <p>
                        This is a list of recently published Elm packages. The
                        identifiers listed below are derived from the repository
                        and tag where we actually found the new version, rather
                        than the contents of the <code>elm-package.json</code> file.
                        (You can follow the links to see what is in that file,
                        if you like).
                    <p>
                        We also have a
                        <a href="https://twitter.com/frelmorg">frelmorg
                        twitter page, where we tweet when we see a new version
                        of a package.
                <div .row .text-center>
                    ^{elmVersionWidget}
                <div .row>
                    <div .col-lg-12>
                        $forall byDay <- result
                            <div .ev-display.#{dayClass}>
                                $forall (Entity _ rvHead, _, _) <- listToMaybe byDay
                                    <span .#{dayNameClass}>#{showGregorian $ utctDay $ repoVersionCommittedAt rvHead}
                                    $forall (Entity _ rv, Value summary, Value gitUrl) <- byDay
                                        <div .#{packageClass}>
                                            <span .#{libraryNameClass}>
                                                <a href="@{RepoVersionR (repoVersionRepo rv) (repoVersionTag rv)}">
                                                    #{fromMaybe gitUrl (gitUrlToLibraryName gitUrl)}
                                            <div .#{packageSummaryClass}>
                                                <a href="@{RepoVersionR (repoVersionRepo rv) (repoVersionTag rv)}">
                                                    <span .label.#{labelForVersion $ repoVersionVersion rv}>
                                                        #{toText $ repoVersionVersion rv}
                                                    #{summary}
        |]
        toWidget
            [cassius|
                .#{packageClass}
                    margin-top: 0.5em

                    a:link, a:visited
                        color: black

                    .label
                        position: relative
                        top: -1px

                .#{libraryNameClass}
                    font-weight: bold

                .#{packageSummaryClass}
                    margin-left: 2em

                .#{dayClass}
                    margin-top: 1em

                .#{dayNameClass}
                    font-size: 110%
                    font-weight: bold
            |]
