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
import Data.Time.Clock
import Database.Esqueleto
import Import.App hiding (Value, on)
import Web.Cookie

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.
getFaviconR :: Handler TypedContent
getFaviconR = do
    cacheSeconds $ 60 * 60 * 24 -- cache for a day
    return $
        TypedContent "image/x-icon" $
        toContent $(embedFile "config/favicon.ico")

handle404 :: MonadHandler m => m [a] -> m a
handle404 =
    (=<<) $ \b ->
        case b of
            initial:_ -> pure initial
            [] -> notFound

toParams :: Maybe Version -> [(Text, Text)]
toParams Nothing = [("ev", "all")]
toParams (Just v) = [("ev", toText v)]

cookieLife :: DiffTime
cookieLife = 60 * 60 * 24 * 365

setElmVersionCookie :: MonadHandler m => Text -> m ()
setElmVersionCookie ev =
    setCookie $
    def
    { setCookieName = "ev"
    , setCookieValue = encodeUtf8 ev
    , setCookiePath = Just "/"
    , setCookieMaxAge = Just cookieLife
    }

{- | Looks up the "ev" param in the URL. If it is set, we also
set a cookie. We only use the cookie if you don't supply
the param. So, basically, we use the cookie to redirect if
you don't supply the param.
-}
lookupRequestedElmVersion :: Handler (Maybe Version)
lookupRequestedElmVersion = do
    evParam <- lookupGetParam "ev"
    currentRoute <- fromMaybe HomeR <$> getCurrentRoute
    if evParam == Just "all"
        then do
            setElmVersionCookie "all"
            pure Nothing
        else case fromText <$> evParam of
                 Just (Left err) ->
                     invalidArgs
                         ["The 'ev' param could not be interpreted.", pack err]
                 Just (Right v) -> do
                     setElmVersionCookie (toText v)
                     pure (Just v)
                 Nothing -> do
                     evCookie <- fmap fromText <$> lookupCookie "ev"
                     case evCookie of
                         Just (Right v) ->
                             redirect (currentRoute, [("ev", toText v)])
                         _ -> pure ()
                     pure Nothing

elmVersionWidget :: Widget
elmVersionWidget = do
    requestedElmVersion <- handlerToWidget lookupRequestedElmVersion
    currentRoute <- fromMaybe HomeR <$> getCurrentRoute
    renderUrl <- getUrlRenderParams
    standardVersions <-
        handlerToWidget $
        runDB $ select $ from (\v -> pure $ v ^. ElmVersionVersion)
    let versions = nub $ sort $ Value requestedElmVersion : standardVersions
    wrapper <- newIdent
    labelButton <- newIdent
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
            <button type="button" .btn.btn-xs.btn-success .#{labelButton}>
                Elm Version
            $forall Value v <- versions
                <button type="button" data-href="#{hrefForVersion v}" .btn.btn-xs .#{buttonColorForVersion v}>
                    $case v
                        $of Just vers
                            #{toText vers}
                        $of Nothing
                            All
    |]
    toWidget
        [julius|
            (function (wrapper) {
                $("." + wrapper + " button[data-href]").click(function () {
                    var href = $(this).attr('data-href');
                    if (href) window.location = href;
                });
            })(#{toJSON wrapper});
        |]
    toWidget
        [cassius|
            .#{wrapper}
                button
                    min-width: 3.5em

            .#{labelButton}
                pointer-events: none
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
