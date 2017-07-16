{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Library where

import Data.SemVer (toText)
import Database.Persist.Sql
import Import.App

getLibrariesR :: Handler Html
getLibrariesR = do
    libraries <- runDB $ selectList [] [Asc LibraryName]
    defaultLayout
        [whamlet|
            <div .container>
                <div .row>
                    <p>
                        This is a list of all the Elm libraries we know about.
                <div .row>
                    <div .col-lg-12>
                        $forall Entity libraryId library <- libraries
                            <div>
                                #{libraryName library}
        |]
