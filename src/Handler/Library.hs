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
import Import.App hiding (on)
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
                (just (rv ^. RepoVersionVersion) ==.
                 sub_select
                     (from $ \rv2 -> do
                          where_ $ rv2 ^. RepoVersionRepo ==. r ^. RepoId
                          pure $ max_ $ rv2 ^. RepoVersionVersion))
            orderBy
                [asc $ l ^. LibraryName, desc $ rv ^. RepoVersionCommittedAt]
            pure (l, r ^. RepoGitUrl, rv, p ^. PackageSummary)
    wrapper <- newIdent
    repoClass <- newIdent
    packageClass <- newIdent
    defaultLayout $ do
        setTitle "Elm Libraries"
        [whamlet|
            <div .container.#{wrapper}>
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
                <div .row>
                    <div .col-lg-12>
                        <dl>
                            $forall byLibrary <- result
                                $forall (Entity _ library, _, _, _) <- listToMaybe byLibrary
                                    <dt>#{libraryName library}
                                    <dd>
                                        $forall (_, gitUrl, Entity _ rv, summary) <- byLibrary
                                            <div .#{repoClass}>
                                                <div .#{packageClass}>
                                                    <a href="@{RepoVersionR (repoVersionRepo rv) (repoVersionTag rv)}">
                                                        <span .label.#{labelForVersion $ repoVersionVersion rv}>
                                                            #{(toText . repoVersionVersion) rv}
                                                        #{unValue summary}
                                                <div>
                                                    <a href="@{RepoR $ repoVersionRepo rv}">
                                                        #{unValue gitUrl}
        |]
        toWidget
            [cassius|
                .#{wrapper}
                    dt
                        margin-top: 0.6em
                    dd
                        margin-left: 2em

                    .#{repoClass}
                        margin-bottom: 0.4em

                        a:visited
                            color: black

                        a:link
                            color: black

                        .label
                            position: relative
                            top: -2px
            |]
