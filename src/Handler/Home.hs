{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Home where

import Import.App hiding (div)

getHomeR :: Handler Html
getHomeR = do
    masthead <- newIdent
    docs <- newIdent
    defaultLayout $ do
        setTitle "Frelm: Alternative package discovery for Elm"
        toWidget
            [hamlet|
                <div .container .#{masthead}>
                    <div .row>
                        <h1>frelm
                        <h2>Another way of discovering Elm packages
                <div .container .#{docs}>
                    <div .row>
                        <p>
                            Elm has a very fine official website dedicated to
                            discovering Elm packages, at
                            <a href="http://package.elm-lang.org">http://package.elm-lang.org.
                            There is also a site at
                            <a href="https://libraries.io/elm">https://libraries.io/elm
                            that has some nice features (e.g. an analysis of dependencies).
                        <p>
                            So, what's different here?
                        <ul>
                            <li>
                                If you're a package author, you don't have to
                                tell us about new versions. We'll look
                                periodically (about once a day) and see if there
                                are any. (But you can add your package from our
                                <a href="@{ReposR}">repos page
                                if we've missed it.)
                            <li>
                                If you're looking for packages, we keep track
                                of a few things that the official Elm web-site
                                doesn't (at least, at time of writing). For
                                instance, we can show you the packages that
                                have been most
                                <a href="@{RecentR}">recently updated.
                                (The
                                <a href="https://libraries.io/elm">libraries.io
                                site does this too).

                                We also have a list of all the
                                <a href="@{ModulesR}">Elm modules
                                exposed by the packages we know about, which you
                                might find handy if want to avoid name clashes.
                        <p>
                            That being said, the
                            <a href="http://package.elm-lang.org">official package website
                            has a very well-considered design, and you may well prefer
                            to use it.
                    |]
        toWidget
            [cassius|
                .#{masthead}
                    font-family: Lato,'Helvetica Neue',Arial,Helvetica,sans-serif
                    text-align: center
                    margin-bottom: 1em

                    h1
                        margin-top: 0em
                        margin-bottom: 0em
                        font-size: 4.5em
                        line-height: 1.2em
                        font-weight: normal

                     h2
                        font-size: 1.7em
                        font-weight: normal

                .#{docs}
                    li
                        margin-top: 0.5em
            |]
