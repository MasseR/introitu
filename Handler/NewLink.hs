module Handler.NewLink where

import Import
import Network.URI (parseURI, URI(..))
import qualified Data.Text as T

data LinkForm = LinkForm Text Text Textarea

linkForm :: AForm App App LinkForm
linkForm = LinkForm <$> 
  areq uriField "URL" Nothing <*>
  (maybe "" id <$> aopt textField "Title" Nothing) <*>
  (maybe (Textarea "") id <$> aopt textareaField "Summary" Nothing)
  where
    uriField = check validateURI textField
    validateURI uri = maybe (Left ("Not a valid url" :: Text)) (const (Right uri)) (parseURI $ T.unpack uri)

getNewLinkR :: Handler RepHtml
getNewLinkR = do
  ((_, formWidget), enctype) <- runFormPost $ renderDivs linkForm
  defaultLayout $ do
    addScriptRemote "//cdnjs.cloudflare.com/ajax/libs/jquery/1.8.2/jquery.min.js"
    addScriptRemote "https://raw.github.com/raimohanska/bacon.js/master/lib/Bacon.min.js"
    $(widgetFile "newLink")

postNewLinkR :: Handler RepHtml
postNewLinkR = do
  ((form, _), _) <- runFormPost $ renderDivs linkForm
  (Entity user _) <- requireAuth
  now <- liftIO getCurrentTime
  case form of
       FormSuccess (LinkForm url title summary) -> do
         linkId <- runDB $ insert (Link url user title summary now)
         redirect $ ViewLinkR linkId
       _ -> redirect NewLinkR
