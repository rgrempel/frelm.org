{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Module where

import Data.SemVer (toText)
import Database.Esqueleto
import Import.App

getModulesR :: Handler Html
getModulesR = do
    modules <-
        runDB $
        select $
        from $ \m -> do
            orderBy [asc $ m ^. ModuleName]
            pure m
    defaultLayout
        [whamlet|
            <div .container>
                <div .row>
                    <p>
                        This is a list of all the Elm modules we know about.
                <div .row>
                    <div .col-lg-12>
                        $forall Entity moduleId moduleRec <- modules
                            <div>
                                #{moduleName moduleRec}
        |]
