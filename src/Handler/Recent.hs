{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Recent where

import Data.ElmPackage
import Data.List (nub)
import Data.PersistSemVer
import Data.Range
import Data.SemVer
import Data.Time.Calendar (showGregorian)
import Database.Esqueleto
import Handler.Common
import Import.App hiding (Value, on)
import qualified Import.App as Prelude

getRecentR :: Handler Html
getRecentR = do
    (result, elmVersions) <-
        runDB $ do
            result <-
                fmap
                    (Prelude.groupBy
                         (Prelude.on
                              (==)
                              (\(Entity _ rv, _, _, _) ->
                                   (utctDay . repoVersionCommittedAt) rv))) $
                select $
                from $ \(repoVersion `InnerJoin` package `InnerJoin` repo) -> do
                    on $ repoVersion ^. RepoVersionRepo ==. repo ^. RepoId
                    on $
                        repoVersion ^. RepoVersionId ==. package ^.
                        PackageRepoVersion
                    orderBy [desc $ repoVersion ^. RepoVersionCommittedAt]
                    limit 1000
                    pure
                        ( repoVersion
                        , package ^. PackageSummary
                        , repo ^. RepoGitUrl
                        , package ^. PackageElmVersion)
            elmVersions <- select $ from $ \ev -> pure $ ev ^. ElmVersionVersion
            pure (result, elmVersions)
    packageClass <- newIdent
    packageSummaryClass <- newIdent
    libraryNameClass <- newIdent
    dayClass <- newIdent
    dayNameClass <- newIdent
    defaultLayout $ do
        setTitle "Recent Elm Modules"
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
                            <div .ev-display.#{dayClass}.#{considerDayElmVersions elmVersions byDay}>
                                $forall (Entity _ rvHead, _, _, _) <- listToMaybe byDay
                                    <span .#{dayNameClass}>#{showGregorian $ utctDay $ repoVersionCommittedAt rvHead}
                                    $forall (Entity _ rv, Value summary, Value gitUrl, Value elmVersionRange) <- byDay
                                        <div .#{packageClass} .ev-display.#{considerElmVersions elmVersions elmVersionRange}>
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

considerElmVersions :: [Value (Maybe Version)] -> Maybe (Range Version) -> Text
considerElmVersions elmVersions range = unwords $ catMaybes versionList
  where
    versionList =
        case range of
            Nothing -> [Just $ versionToClass Nothing]
            Just r ->
                fmap
                    (\(Value vers) ->
                         case vers of
                             Nothing -> Just $ versionToClass Nothing
                             Just v ->
                                 if contains r v
                                     then Just $ versionToClass (Just v)
                                     else Nothing)
                    elmVersions

considerDayElmVersions ::
       [Value (Maybe Version)]
    -> [(a, b, c, Value (Maybe (Range Version)))]
    -> Text
considerDayElmVersions elmVersions ranges =
    unwords $
    nub $
    fmap
        (considerElmVersions elmVersions . (\(_, _, _, Value vers) -> vers))
        ranges
