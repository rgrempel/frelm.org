{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Import.NoFoundation
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)

-- Used only when in "auth-dummy-login" setting is enabled.
import Yesod.Auth.Dummy
import Yesod.Auth.Message (AuthMessage(LoginTitle))
import Yesod.Auth.OAuth2.Github
import Yesod.Auth.OAuth2.Gitlab

import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import Yesod.Core.Types (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import Yesod.Default.Util (addStaticContentExternal)

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings :: AppSettings
    , appStatic :: Static -- ^ Settings for static file serving.
    , appConnPool :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger :: Logger
    , appGithubOAuthKeys :: OAuthKeys
    , appGitlabOAuthKeys :: OAuthKeys
    }

data MenuItem = MenuItem
    { menuItemLabel :: Text
    , menuItemRoute :: Route App
    , menuItemAccessCallback :: Bool
    }

data MenuTypes
    = NavbarLeft MenuItem
    | NavbarRight MenuItem

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData
    "App"
    [parseRoutes|
        /static StaticR Static appStatic
        /auth   AuthR   Auth   getAuth

        /favicon.ico FaviconR GET
        /robots.txt RobotsR GET

        / HomeR GET

        /library LibrariesR GET

        /module ModulesR GET

        /repo ReposR GET POST
        /repo/#RepoId RepoR GET

        /version/#RepoVersionId RepoVersionR GET

        /profile ProfileR GET
    |]

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Session timeout in minutes
sessionTimeout :: Int
sessionTimeout = 120

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
--
-- Controls the base of generated URLs. For more information on modifying,
-- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
instance Yesod App where
    approot =
        ApprootRequest $ \app req ->
            fromMaybe
                (getApprootText guessApproot app req)
                (appRoot $ appSettings app)
    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ =
        Just <$> -- TODO: add sssOnlySessions
        envClientSessionBackend
            sessionTimeout -- timeout in minutes
            "CLIENT_SESSION_KEY"
    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware = defaultYesodMiddleware
            -- TODO: Add these in
            -- . (sslOnlyMiddleware sessionTimeout)
            -- . defaultCsrfMiddleware
    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage
        muser <- maybeAuthPair
        mcurrentRoute <- getCurrentRoute
        -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
        (title, parents) <- breadcrumbs
        -- Define the menu items of the header.
        let menuItems =
                [ NavbarLeft
                      MenuItem
                      { menuItemLabel = "Home"
                      , menuItemRoute = HomeR
                      , menuItemAccessCallback = True
                      }
                , NavbarLeft
                      MenuItem
                      { menuItemLabel = "Repositories"
                      , menuItemRoute = ReposR
                      , menuItemAccessCallback = True
                      }
                , NavbarLeft
                      MenuItem
                      { menuItemLabel = "Libraries"
                      , menuItemRoute = LibrariesR
                      , menuItemAccessCallback = True
                      }
                , NavbarLeft
                      MenuItem
                      { menuItemLabel = "Modules"
                      , menuItemRoute = ModulesR
                      , menuItemAccessCallback = True
                      }
                , NavbarLeft
                      MenuItem
                      { menuItemLabel = "Profile"
                      , menuItemRoute = ProfileR
                      , menuItemAccessCallback = isJust muser
                      }
                , NavbarRight
                      MenuItem
                      { menuItemLabel = "Login"
                      , menuItemRoute = AuthR LoginR
                      , menuItemAccessCallback = isNothing muser
                      }
                , NavbarRight
                      MenuItem
                      { menuItemLabel = "Logout"
                      , menuItemRoute = AuthR LogoutR
                      , menuItemAccessCallback = isJust muser
                      }
                ]
        let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
        let navbarRightMenuItems = [x | NavbarRight x <- menuItems]
        let navbarLeftFilteredMenuItems =
                [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
        let navbarRightFilteredMenuItems =
                [x | x <- navbarRightMenuItems, menuItemAccessCallback x]
        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.
        pc <-
            widgetToPageContent $ do
                addStylesheet $ StaticR css_bootstrap_css
                $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")
    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR
    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized ProfileR _ = isAuthenticated
    isAuthorized LibrariesR _ = pure Authorized
    isAuthorized ModulesR _ = pure Authorized
    isAuthorized (RepoVersionR _) _ = return Authorized
    isAuthorized (RepoR _) write = authorizeWrite write
    isAuthorized ReposR write = authorizeWrite write
    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
        -- Generate a unique filename based on the content itself
      where
        genFileName lbs = "autogen-" ++ base64md5 lbs
    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app) ||
        level == LevelWarn || level == LevelError
    makeLogger = return . appLogger
    -- Provide proper Bootstrap styling for default displays, like
    -- error pages
    defaultMessageWidget title body = $(widgetFile "default-message-widget")

authorizeWrite :: Bool -> Handler AuthResult
authorizeWrite write =
    if write
        then isAuthenticated
        else return Authorized

-- Define breadcrumbs.
instance YesodBreadcrumbs App where
    breadcrumb (StaticR _) = pure ("Static", Nothing)
    breadcrumb FaviconR = pure ("Favicon", Nothing)
    breadcrumb RobotsR = pure ("Robots", Nothing)
    breadcrumb HomeR = pure ("Home", Nothing)
    breadcrumb (AuthR _) = pure ("Login", Nothing)
    breadcrumb ProfileR = pure ("Profile", Nothing)
    breadcrumb LibrariesR = pure ("Libraries", Nothing)
    breadcrumb ModulesR = pure ("Modules", Nothing)
    breadcrumb ReposR = pure ("Repositories", Nothing)
    breadcrumb (RepoR repoId) =
        runDB $ do
            repoUrl <- maybe "Unknown repository" repoGitUrl <$> get repoId
            pure (repoUrl, Just ReposR)
    breadcrumb (RepoVersionR versionId) =
        runDB $ do
            repoVersion <- get versionId
            pure
                ( maybe "Unknown Version" repoVersionTag repoVersion
                , (RepoR . repoVersionRepo) <$> repoVersion)

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId
    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True
    authenticate creds =
        runDB $ do
            x <- getBy $ UniqueUser (credsPlugin creds) (credsIdent creds)
            case x of
                Just (Entity uid _) -> do
                    update uid $ credsToUserUpdate creds
                    return $ Authenticated uid
                Nothing ->
                    case credsToUser creds of
                        Just user -> Authenticated <$> insert user
                        Nothing ->
                            return $
                            ServerError "Name or email was not provided."
    -- You can add other plugins like Google Email, email or OAuth here
    authPlugins app =
        [ oauth2Github
              (oauthKeysClientId $ appGithubOAuthKeys app)
              (oauthKeysClientSecret $ appGithubOAuthKeys app)
        , oauth2Gitlab
              (oauthKeysClientId $ appGitlabOAuthKeys app)
              (oauthKeysClientSecret $ appGitlabOAuthKeys app)
        ] ++
        extraAuthPlugins
        -- Enable authDummy login if enabled.
      where
        extraAuthPlugins = [authDummy | appAuthDummyLogin $ appSettings app]
    authHttpManager = getHttpManager
    loginHandler = do
        tp <- getRouteToParent
        lift $
            authLayout $ do
                setTitleI LoginTitle
                master <- getYesod
                pluginClass <- newIdent
                pluginNameBase <- newIdent
                [whamlet|
                    <div .container>
                        <div .row>
                            $forall plugin <- authPlugins master
                                <div class="col-md-4 #{pluginClass}">
                                    ^{apLogin plugin tp}
                |]
                toWidget
                    [lucius|
                        div.#{pluginClass} {
                            font-weight: bold;
                            margin-top: 12px;
                        }
                    |]

credsToUser :: Creds m -> Maybe User
credsToUser Creds {..} =
    if credsPlugin == "dummy"
        then Just
                 User
                 { userName = "Dummy"
                 , userEmail = "none@none.com"
                 , userPlugin = credsPlugin
                 , userIdent = credsIdent
                 , userAvatarUrl = Nothing
                 }
        -- For Github and others, we require the name and email
        else User <$> lookup "name" credsExtra <*> lookup "email" credsExtra <*>
             pure (lookup "avatar_url" credsExtra) <*>
             pure credsPlugin <*>
             pure credsIdent

credsToUserUpdate :: Creds m -> [Update User]
credsToUserUpdate creds = catMaybes $ considerExtra <$> credsExtra creds
  where
    considerExtra (key, value) =
        case key of
            "name" -> Just $ UserName =. value
            "email" -> Just $ UserEmail =. value
            "avatar_url" -> Just $ UserAvatarUrl =. Just value
            _ -> Nothing

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    return $
        case muid of
            Nothing -> Unauthorized "You must login to access this page"
            Just _ -> Authorized

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger
-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
