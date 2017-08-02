{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- | Common handler functions.
module Handler.Common where

import Data.FileEmbed (embedFile)
import Data.List (nub)
import Data.Range
import Data.SemVer (Version, fromText, toText)
import Database.Esqueleto
import Import.App hiding (Value, on)

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.
getFaviconR :: Handler TypedContent
getFaviconR = do
    cacheSeconds $ 60 * 60 * 24 -- cache for a day
    return $
        TypedContent "image/x-icon" $
        toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR =
    return $ TypedContent typePlain $ toContent $(embedFile "config/robots.txt")

toParams :: Maybe Version -> [(Text, Text)]
toParams Nothing = []
toParams (Just v) = [("ev", toText v)]

lookupRequestedElmVersion :: MonadHandler m => m (Maybe Version)
lookupRequestedElmVersion = do
    ev <- fmap fromText <$> lookupGetParam "ev"
    case ev of
        Just (Left _) -> notFound
        Just (Right v) -> pure (Just v)
        Nothing -> pure Nothing

elmVersionWidget :: Widget
elmVersionWidget = do
    requestedElmVersion <- lookupRequestedElmVersion
    currentRoute <- fromMaybe HomeR <$> getCurrentRoute
    renderUrl <- getUrlRenderParams
    standardVersions <-
        handlerToWidget $
        runDB $ select $ from (\v -> pure $ v ^. ElmVersionVersion)
    let versions = nub $ sort $ Value requestedElmVersion : standardVersions
    wrapper <- newIdent
    let buttonColorForVersion v =
            if v == requestedElmVersion
                then "btn-primary" :: Text
                else "btn-default"
    let hrefForVersion v =
            if v == requestedElmVersion
                then ""
                else renderUrl currentRoute $ toParams v
    [whamlet|
        <div class="btn-group" role="group" aria-label="Choose Elm Version" .#{wrapper}>
            <button type="button" .btn.btn-xs.btn-success>
                Elm Version
            $forall Value v <- versions
                <a type="button" href="#{hrefForVersion v}" .btn.btn-xs .#{buttonColorForVersion v}>
                    $case v
                        $of Just vers
                            #{toText vers}
                        $of Nothing
                            All
    |]
    toWidget
        [cassius|
            .#{wrapper}
                button
                    pointer-events: none
                a
                    min-width: 3.5em
        |]

maxRepoVersion ::
       Maybe Version -> SqlExpr (Entity Repo) -> SqlExpr (Value (Maybe Version))
maxRepoVersion elmVersion r =
    case elmVersion of
        Nothing ->
            sub_select $
            from $ \rv2 -> do
                where_ $ rv2 ^. RepoVersionRepo ==. r ^. RepoId
                pure $ max_ $ rv2 ^. RepoVersionVersion
        Just _ ->
            sub_select $
            from $ \(rv2 `InnerJoin` p2) -> do
                on $ p2 ^. PackageRepoVersion ==. rv2 ^. RepoVersionId
                where_ $
                    (rv2 ^. RepoVersionRepo ==. r ^. RepoId) &&.
                    justValueInRange elmVersion (p2 ^. PackageElmVersion)
                pure $ max_ $ rv2 ^. RepoVersionVersion
