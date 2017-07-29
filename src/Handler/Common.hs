{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | Common handler functions.
module Handler.Common where

import Data.FileEmbed (embedFile)
import Data.SemVer (Version, toText)
import qualified Data.Text
import Database.Esqueleto
import Import.App

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.
getFaviconR :: Handler TypedContent
getFaviconR = do
    cacheSeconds $ 60 * 60 * 24 -- cache for a day
    return $
        TypedContent "image/x-icon" $
        toContent $(embedFile "config/favicon.ico")

versionToClass :: Maybe Version -> Text
versionToClass Nothing = "ev-any"
versionToClass (Just v) = "ev-" <> Data.Text.replace "." "-" (toText v)

displayIfElmVersion :: Maybe Version -> Text
displayIfElmVersion v = "ev-display " <> versionToClass v

containerElmVersion :: Maybe Version -> Text
containerElmVersion Nothing = "ev-container-any"
containerElmVersion (Just vers) = "ev-container-" <> Data.Text.replace "." "-" (toText vers)

elmVersionWidget :: Widget
elmVersionWidget = do
    versions <-
        handlerToWidget $
        runDB $ select $ from (\v -> pure $ v ^. ElmVersionVersion)
    wrapper <- newIdent
    [whamlet|
        <div class="btn-group" role="group" aria-label="Choose Elm Version" .#{wrapper}>
            <button type="button" .btn.btn-xs.btn-success>
                Elm Version
            $forall Value v <- versions
                <a type="button" data-set-version="#{containerElmVersion v}" .btn.btn-xs.btn-default.version-button>
                    $case v
                        $of Just vers
                            #{toText vers}
                        $of Nothing
                            All
    |]
    addScript $ StaticR scripts_version_button_js
    toWidget
        [cassius|
            .#{wrapper}
                button
                    pointer-events: none
                a
                    min-width: 3.5em

            .ev-display
                display: none
        |]
    for_ versions $ \(Value v) ->
        toWidget
            [cassius|
                .#{containerElmVersion v} .ev-display.#{versionToClass v}
                    display: inherit
            |]
