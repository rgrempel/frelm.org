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
        setTitle "Welcome to Frelm!"
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
                            You should definitely consider trying that website,
                            to see whether it fits your needs.
                        <p>
                            So, what's different here?

                        <ul>
                            <li>
                                If you're a package author, you don't have to
                                tell us about new versions. We'll look
                                periodically and see if there are any.
                            <li>
                                The package need not be hosted on Github -- any
                                git repository will do.
                            <li>
                                If you're looking for packages, we keep track
                                of a few things that the official Elm web-site
                                doesn't (at leat, at time of writing). For
                                instance, we can show you the packages that
                                have been most recently updated. We also have a
                                list of all the Elm modules exposed by the
                                packages we know about.

                        <h3>Terminology

                        <p>
                            Here are some notes about the terminology we're using
                            here.
                        <ul>
                            <li>
                                A "<b>repository</b>" refers to the Git
                                repository where we found something.
                            <li>
                                A "<b>library</b>" is something like
                                "elm-lang/core" ... that is, the "key" you use
                                to identifiy a dependency.
                            <li>
                                A "<b>package</b>" is a specific version of a
                                repository, implementing a library.
                            <li>
                                A "<b>module</b>" is something like `List` ...
                                that is, the name of an Elm module.
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
