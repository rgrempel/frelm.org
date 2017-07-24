{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Recent where

import Data.ElmPackage
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
        from $ \(repoVersion `InnerJoin` package `InnerJoin` repo) -> do
            on $ repoVersion ^. RepoVersionRepo ==. repo ^. RepoId
            on $ repoVersion ^. RepoVersionId ==. package ^. PackageRepoVersion
            orderBy [desc $ repoVersion ^. RepoVersionCommittedAt]
            limit 1000
            pure (repoVersion, package ^. PackageSummary, repo ^. RepoGitUrl)
    wrapper <- newIdent
    listWrapper <- newIdent
    defaultLayout $ do
        setTitle "Recent Elm Modules"
        [whamlet|
            <div .container.#{wrapper}>
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
                <div .row>
                    <div .col-lg-12 .#{listWrapper}>
                        $forall byDay <- result
                            $forall (Entity _ rvHead, _, _) <- listToMaybe byDay
                                <h4>#{showGregorian $ utctDay $ repoVersionCommittedAt rvHead}
                                <dl>
                                    $forall (Entity _ rv, summary, gitUrl) <- byDay
                                        <dt>
                                            <a href="@{RepoVersionR (repoVersionRepo rv) (repoVersionTag rv)}">
                                                #{fromMaybe (unValue gitUrl) (gitUrlToLibraryName $ unValue gitUrl)}
                                        <dd>
                                            <a href="@{RepoVersionR (repoVersionRepo rv) (repoVersionTag rv)}">
                                                <span .label.#{labelForVersion $ repoVersionVersion rv}>
                                                    #{toText $ repoVersionVersion rv}
                                                #{unValue summary}
        |]
        toWidget
            [cassius|
                .#{wrapper}
                    dt
                        margin-top: 0.5em
                    dd
                        margin-left: 2em

                .#{listWrapper}
                    a:link, a:visited
                        color: black

                    .label
                        position: relative
                        top: -1px
            |]
