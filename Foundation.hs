module Foundation
    ( App (..)
    , Route (..)
    , AppMessage (..)
    , resourcesApp
    , Handler
    , Widget
    , Form
    , maybeAuth
    , requireAuth
    , module Settings
    , module Model
    , getExtra
    ) where

import Prelude
import Yesod hiding ((==.))
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.BrowserId
import Yesod.Auth.GoogleEmail
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Conduit (Manager)
import qualified Settings
import qualified Database.Persist.Store
import Settings.StaticFiles
import Database.Persist.GenericSql
import Settings (widgetFile, Extra (..))
import Model
import Text.Jasmine (minifym)
import Web.ClientSession (getKey)
import Text.Hamlet (hamletFile)
import Database.Esqueleto
import qualified Text.HyperEstraier.Database as Hs
import Data.Conduit.Pool
import Data.Maybe (listToMaybe)

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.Store.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
    , indexPool :: Pool Hs.Database
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConfig
    }

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype AppRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route App = AppRoute
-- * Creates the value resourcesApp which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- App. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the AppRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm App App (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = do
        key <- getKey "config/client_session_key.aes"
        return . Just $ clientSessionBackend key 1440

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        muser <- maybeAuth
        pc <- widgetToPageContent $ do
            $(widgetFile "normalize")
            menu <- $(widgetFile "menu")
            addStylesheet $ StaticR css_bootstrap_css
            addStylesheet $ StaticR css_bootstrap_responsive_css
            $(widgetFile "default-layout")
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent = addStaticContentExternal minifym base64md5 Settings.staticDir (StaticR . flip StaticRoute [])

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    isAuthorized route isWrite = do
      mauth <- maybeAuth
      runDB $ mauth `isAuthorizedTo` permissionsRequiredFor route isWrite

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersist
    runDB f = do
        master <- getYesod
        Database.Persist.Store.runPool
            (persistConfig master)
            f
            (connPool master)

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Just uid
            Nothing -> do
                fmap Just $ insert $ User (credsIdent creds) Nothing

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [authBrowserId, authGoogleEmail]

    authHttpManager = httpManager

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

data Permission = Post |
  Edit NoteId |
  View NoteId |
  List |
  PostLink |
  EditLink LinkId |
  ViewLink LinkId |
  EditJournal JournalId

permissionsRequiredFor :: Route App -> Bool -> [Permission]
permissionsRequiredFor AddR _ = [Post]
permissionsRequiredFor NewLinkR _ = [Post]
permissionsRequiredFor ListR _ = [List]
permissionsRequiredFor ListLinksR _ = [List]
permissionsRequiredFor (EditR noteid) _ = [Edit noteid]
permissionsRequiredFor (ViewR noteid) _ = [View noteid]
permissionsRequiredFor (EditLinkR link) _ = [EditLink link]
permissionsRequiredFor (ViewLinkR link) _ = [ViewLink link]
permissionsRequiredFor (WriteJournalR journal) _ = [EditJournal journal]
permissionsRequiredFor (JournalEditR journal) _ = [EditJournal journal]
permissionsRequiredFor (CreateJournalR) _ = [Post]
permissionsRequiredFor _ _ = []

hasPermissionTo :: Entity User -> Permission -> YesodDB sub App AuthResult
_ `hasPermissionTo` Post = return Authorized
_ `hasPermissionTo` PostLink = return Authorized
_ `hasPermissionTo` List = return Authorized
(Entity userId _) `hasPermissionTo` (EditLink link) = do
  link' <- select $ from $ \l -> do
    where_ (l ^. LinkOwner ==. val userId &&. l ^. LinkId ==. val link)
    return l
  return $ maybe
    (Unauthorized "You have no permission to access this link")
    (const Authorized)
    (listToMaybe link')
user `hasPermissionTo` (ViewLink link) = user `hasPermissionTo` EditLink link
(Entity userId _) `hasPermissionTo` (Edit note) = do
  note' <- select $ from $ \n -> do
    where_ (n ^. NoteOwner ==. val userId &&. n ^. NoteId ==. val note)
    return n
  case note' of
       [] -> return $ Unauthorized "You have no permission to edit this note"
       _ -> return Authorized
user `hasPermissionTo` (View note) = user `hasPermissionTo` (Edit note)
(Entity userId _) `hasPermissionTo` (EditJournal journal) = do
  journal <- select $ from $ \j -> do
    where_ (j ^. JournalOwner ==. val userId &&. j ^. JournalId ==. val journal)
    return j
  case journal of
       [] -> return $ Unauthorized "You have no permission to edit this journal"
       _ -> return Authorized

isAuthorizedTo :: Maybe (Entity User) -> [Permission] -> YesodDB sub App AuthResult
_ `isAuthorizedTo` [] = return Authorized
Nothing `isAuthorizedTo` (_:_) = return AuthenticationRequired
Just u `isAuthorizedTo` (p:ps) = do
  r <- u `hasPermissionTo` p
  case r of
       Authorized -> Just u `isAuthorizedTo` ps
       _ -> return r

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
