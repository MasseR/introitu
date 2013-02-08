module Handler.NewLink where

import Import
import Network.URI (parseURI)
import qualified Data.Text as T
import Data.Char (isSpace)
import Data.Foldable (msum)

data LinkForm = LinkForm (Maybe Text) Text Textarea [Text]

linkForm :: AForm App App LinkForm
linkForm = LinkForm <$> 
  ((fmap T.pack . msum . uris) <$> areq textField "URL" Nothing) <*>
  (maybe "" id <$> aopt textField "Title" Nothing) <*>
  (maybe (Textarea "") id <$> aopt textareaField "Summary" Nothing) <*>
  (maybe [] splitTags <$> aopt textField "Tags" Nothing)
  where
    splitTags = map trim . T.split (== ',')
    trim = T.dropWhileEnd isSpace . T.dropWhile isSpace
    uris uri = map (fmap show . parseURI . T.unpack) [uri, "http://" `T.append` uri]

getNewLinkR :: Handler RepHtml
getNewLinkR = do
  ((_, formWidget), enctype) <- runFormPost $ renderDivs linkForm
  defaultLayout $ do
    addScriptRemote "//cdnjs.cloudflare.com/ajax/libs/jquery/1.8.2/jquery.min.js"
    addScriptRemote "https://raw.github.com/raimohanska/bacon.js/master/dist/Bacon.min.js"
    addScriptRemote "https://raw.github.com/janl/mustache.js/master/mustache.js"
    $(widgetFile "newLink")

postNewLinkR :: Handler RepHtml
postNewLinkR = do
  ((form, _), _) <- runFormPost $ renderDivs linkForm
  (Entity user _) <- requireAuth
  now <- liftIO getCurrentTime
  case form of
       FormSuccess (LinkForm url title summary tags) ->
         case url of
              Just url' -> do
                linkId <- runDB $ insert (Link url' user title summary now)
                runDB $ mapM_ (insert . LinkTags linkId) tags
                redirect $ NewLinkR
              _ -> setMessage "Invalid url" >> redirect NewLinkR
       _ -> redirect NewLinkR
