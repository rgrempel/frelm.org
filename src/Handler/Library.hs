{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Library where

import Data.SemVer (toText)
import Database.Esqueleto
import Import.App hiding (on)
import qualified Import.App as Prelude

-- | What we want to "get" here is every library, along with some sense
-- of the packages which implement the library. For that, so far, we
-- consult the **declared** reppository in the elm-package.json,
-- ignoring where we **actually** obtained it. (This is in anticipation
-- of doing something interesting with the `dependency-sources` that
-- `elm-install` uses).
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
            pure (l, r, rv, p)
    wrapper <- newIdent
    defaultLayout $ do
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
                        <code>elm-lang/core</code> is necessarily to be found
                        at <code>https://github.com/elm-lang/core.git</code>.
                        However, I'm distinguishing between the two in the hope
                        of eventually doing something interesting with the
                        <code>dependency-sources</code> used by
                        <code>elm-install</code>, which would make it possible
                        to have alternative implementations of a "library".
                <div .row>
                    <div .col-lg-12>
                        <dl>
                            $forall byLibrary <- result
                                $forall (Entity _ library, _, _, _) <- safeHead byLibrary
                                    <dt>#{libraryName library}
                                    <dd>
                                        $forall (Entity _ library, Entity repoId repo, Entity repoVersionId rv, Entity packageId package) <- byLibrary
                                            <div>
                                                #{packageSummary package} /
                                                <a href="@{RepoVersionR (repoVersionRepo rv) (repoVersionTag rv)}">#{(toText . repoVersionVersion) rv}
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
