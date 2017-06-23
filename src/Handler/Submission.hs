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


getSubmissionR :: SubmissionId -> Handler Html
getSubmissionR id =
    defaultLayout
        [whamlet|
            <p>A submission page
        |]


getSubmissionsR :: Handler Html
getSubmissionsR = do
    (widget, enctype) <-
        generateFormPost submissionForm

    defaultLayout
        [whamlet|
            <p>Submit a GIT URL
                <form method=post action=@{SubmissionsR} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
        |]


postSubmissionsR :: Handler Html
postSubmissionsR = do
    ((result, widget), enctype) <-
        runFormPost submissionForm

    case result of
        FormSuccess submittedForm -> do
            currentUser <- requireAuthId

            let submission =
                    Submission
                        { submissionSource = source submittedForm
                        , submissionSubmittedBy = currentUser
                        }

            submissionId <-
                runDB (insert submission)

            setMessage $
                toHtml ("Submission saved" :: Text)

            redirect $
                SubmissionR submissionId

        FormMissing -> do
            setMessage $ toHtml ("Form data was missing" :: Text )
            redirect SubmissionsR

        FormFailure err -> do
            setMessage $ toHtml ("Invalid input, let's try again" :: Text)
            redirect SubmissionsR
