{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
--
-- OAuth2 plugin for http://gitlab.com
--
-- * Authenticates against gitlab
-- * Uses gitlab user id as credentials identifier
--
module Yesod.Auth.OAuth2.Gitlab
    ( oauth2Gitlab
    , module Yesod.Auth.OAuth2
    ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif

import Control.Exception.Lifted
import Control.Monad (mzero)
import Data.Aeson
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.HTTP.Conduit (Manager)
import Yesod.Auth
import Yesod.Auth.OAuth2

import qualified Data.Text as T


data GitlabUser = GitlabUser
    { gitlabUserId :: Int
    , gitlabUserName :: Text
    , gitlabUserLogin :: Text
    , gitlabUserAvatarUrl :: Text
    , gitlabUserEmail :: Text
    }

instance FromJSON GitlabUser where
    parseJSON (Object o) =
        GitlabUser
            <$> o .: "id"
            <*> o .: "name"
            <*> o .: "username"
            <*> o .: "avatar_url"
            <*> o .: "email"

    parseJSON _ = mzero


oauth2Gitlab :: YesodAuth m
             => Text -- ^ Client ID
             -> Text -- ^ Client Secret
             -> AuthPlugin m
oauth2Gitlab clientId clientSecret =
    authOAuth2 "gitlab" oauth fetchGitlabProfile

    where
        -- Note that I'd rather use `scope=read_user`, but that isn't working ATM
        -- https://gitlab.com/gitlab-org/gitlab-ce/issues/33022
        oauth = OAuth2
            { oauthClientId = encodeUtf8 clientId
            , oauthClientSecret = encodeUtf8 clientSecret
            , oauthOAuthorizeEndpoint = encodeUtf8 $ "https://gitlab.com/oauth/authorize?scope=api"
            , oauthAccessTokenEndpoint = "https://gitlab.com/oauth/token"
            , oauthCallback = Nothing
            }


fetchGitlabProfile :: Manager -> AccessToken -> IO (Creds m)
fetchGitlabProfile manager token = do
    userResult <-
        authGetJSON manager token "https://gitlab.com/api/v3/user"

    case userResult of
        Left err ->
            throwIO $ InvalidProfileResponse "gitlab" err

        Right user ->
            pure $ Creds
                { credsPlugin = "gitlab"
                , credsIdent = T.pack $ show $ gitlabUserId user
                , credsExtra =
                    [ ("name", gitlabUserName user)
                    , ("login", gitlabUserLogin user)
                    , ("email", gitlabUserEmail user)
                    , ("avatar_url", gitlabUserAvatarUrl user)
                    , ("access_token", decodeUtf8 $ accessToken token)
                    ]
                }
