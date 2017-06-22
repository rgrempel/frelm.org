{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Submission where

import Import


data SubmissionForm = SubmissionForm
    { source :: Text
    } deriving Show


-- Or `Form Submission`
submissionForm :: Html -> MForm Handler (FormResult SubmissionForm, Widget)
submissionForm =
    renderDivs $ SubmissionForm
        <$> areq urlField "Git URL" Nothing


getSubmissionR :: Handler Html
getSubmissionR = do
    (widget, enctype) <-
        generateFormPost submissionForm
    
    defaultLayout
        [whamlet|
            <p>Submit a GIT URL
                <form method=post action=@{SubmissionR} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
        |]


postSubmissionR :: Handler Html
postSubmissionR = do
    ((result, widget), enctype) <-
        runFormPost submissionForm
    
    case result of
        FormSuccess submission ->
            defaultLayout [whamlet|<p>#{show submission}|]

        FormMissing ->
            defaultLayout
                [whamlet|
                    <p>Missing input.
                    <form method=post action=@{SubmissionR} enctype=#{enctype}>
                        ^{widget}
                        <button>Submit
                |]
        
        FormFailure err ->
            defaultLayout
                [whamlet|
                    <p>Invalid input, let's try again.
                    <p>#{show err}
                    <form method=post action=@{SubmissionR} enctype=#{enctype}>
                        ^{widget}
                        <button>Submit
                |]
