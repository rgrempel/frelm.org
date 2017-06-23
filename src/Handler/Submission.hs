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
        <$> areq textField "Git URL" Nothing


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
        FormSuccess submission -> do
            setMessage $ toHtml ("Success" ++ show submission)
            redirect SubmissionR

        FormMissing -> do
            setMessage $ toHtml ("Form data was missing" :: Text )
            redirect SubmissionR
        
        FormFailure err -> do
            setMessage $ toHtml ("Invalid input, let's try again" :: Text)
            redirect SubmissionR
