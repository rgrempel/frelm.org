{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Profile where

import Import.App

getProfileR :: Handler Html
getProfileR = do
    (_, user) <- requireAuthPair
    defaultLayout $ do
        setTitle . toHtml $ userIdent user <> "'s User page"
        [whamlet|
            <div .ui.container>
                <h1>Access granted!
                <p>
                    This page is protected and access is allowed only for
                    authenticated users.
                <p>
                    Your data is protected with us <strong><span
                    class="username">#{userIdent user}</span></strong>!
        |]
