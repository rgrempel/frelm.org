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
                        $forall Entity libraryId library <- libraries
                            <div>
                                #{libraryName library}
        |]
