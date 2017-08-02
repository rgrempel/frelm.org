{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Library where

import Data.PersistSemVer
import Data.SemVer (toText)
import Database.Esqueleto
import Handler.Common
import Import.App hiding (Value, isNothing, on)
import qualified Import.App as Prelude

-- | What we want to "get" here is every library, along with some sense
-- of the packages which implement the library. For that, we
-- consult the **declared** reppository in the elm-package.json,
-- ignoring where we **actually** obtained it.
--
-- This makes sense on this page, I think, because we also have the
-- repositories page where you can see a list of repositories.
getLibrariesR :: Handler Html
getLibrariesR = do
    elmVersion <- lookupRequestedElmVersion
    result <-
        fmap
            (Prelude.groupBy
                 (Prelude.on (==) (\(Entity libraryId _, _, _, _) -> libraryId))) $
        runDB $
        select $
        from $ \(r `InnerJoin` rv `InnerJoin` p `InnerJoin` l) -> do
            on $ p ^. PackageLibrary ==. just (l ^. LibraryId)
            on $ rv ^. RepoVersionId ==. (p ^. PackageRepoVersion)
            on $
                (r ^. RepoId ==. rv ^. RepoVersionRepo) &&.
                (just (rv ^. RepoVersionVersion) ==. maxRepoVersion elmVersion r)
            orderBy [asc $ l ^. LibraryName, asc $ r ^. RepoGitUrl]
            pure (l, r ^. RepoGitUrl, rv, p ^. PackageSummary)
    wrapper <- newIdent
    defaultLayout $ do
        setTitle "Elm Libraries"
        [whamlet|
            <div .container>
                <div .row>
                    <p>
                        This is a list of all the Elm libraries we know about.
                    <p>
                        A "library", as we are using the term here, refers to
                        something like <code>elm-lang/core</code>. So, the sort
                        of thing you use in an <code>elm-package.json</code>
                        file to specify a dependency.
                    <p>
                        Normally, there is a well-defined relationship between
                        a "library" and a "repository", in the sense that
                        <code>elm-lang/core</code> is to be found
                        at <code>https://github.com/elm-lang/core.git</code>.
                        And, one would expect the <code>repository</code> field of the
                        <code>elm-package.json</code> file to reflect that.
                        However, it doesn't always do so, for a variety of
                        reasons.
                    <p>
                        For now, at least, the list here is based on an
                        interpretation of the <code>repository</code> field in
                        the <code>elm-package.json</code> file, rather than the
                        actual location of the repository where we found it.
                        But, we do also show a link to our page for the actual
                        repository. And if you want to see a list of repositories,
                        you can look at the <a href="@{ReposR}">repositories page</a>.
                <div .row .text-center>
                    ^{elmVersionWidget}
                <div .row .#{wrapper}>
                    <div .col-lg-12>
                        $forall byLibrary <- result
                            ^{viewLibrary byLibrary}

        |]
        toWidget
            [cassius|
                .#{wrapper}
                    .library
                        margin-top: 0.6em

                    .label
                        position: relative
                        top: -1px

                    a:visited, a:link
                        color: black

                    .libraryName
                        font-weight: bold

                    .repo
                        margin-left: 2em
                        margin-bottom: 0.4em

                    .package
                        margin-left: 2em
            |]

viewLibrary ::
       [(Entity Library, Value Text, Entity RepoVersion, Value Text)] -> Widget
viewLibrary byLibrary = do
    let firstEntry = listToMaybe byLibrary
    [whamlet|
        $forall (Entity _ library, _, _, _) <- firstEntry
            <div .library>
                <span .libraryName>
                    #{libraryName library}
                $forall (_, Value gitUrl, Entity _ rv, Value summary) <- byLibrary
                    <div>
                        <div .package>
                            <a href="@{RepoVersionR (repoVersionRepo rv) (repoVersionTag rv)}">
                                <span .label.#{labelForVersion $ repoVersionVersion rv}>
                                    #{(toText . repoVersionVersion) rv}
                                #{summary}
                        <div .repo>
                            <a href="@{RepoR $ repoVersionRepo rv}">
                                #{gitUrl}
    |]
