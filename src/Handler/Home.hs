{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Home where

import Import.App hiding (div)
import Text.Blaze.Bootstrap as Html
import Text.Blaze.Html as Html
import Text.Blaze.Html5 as Html
import Text.Blaze.Html5.Attributes as Html

-- import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
-- import Text.Julius (RawJS (..))
-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR =
    defaultLayout $ do
        setTitle "Welcome to Frelm!"
        toWidget $ \_ ->
            div ! class_ "masthead" $
            container $
            row $ do
                h1 ! class_ "header" $ "frelm"
                h2 "Put slogan here"
        toWidget
            [lucius|
                li {
                    line-height: 2em;
                    font-size: 16px;
                }
            |]
