module Handler.Add where

import Import
import Model.Note

data NoteForm = NoteForm {
    fnoteTitle :: Text
  , fnoteTopic :: Text
  , fnoteContent :: Textarea
  } deriving Show

noteForm :: Maybe NoteForm -> AForm App App NoteForm
noteForm note = NoteForm <$>
  areq textField "Title" (fnoteTitle <$> note) <*>
  areq textField "Topic" (fnoteTopic <$> note) <*>
  areq textareaField "Content" (fnoteContent <$> note)

getAddR :: Handler RepHtml
getAddR = do
  ((_, formWidget), enctype) <- runFormPost $ renderDivs (noteForm Nothing)
  renderer <- getUrlRender
  defaultLayout $ do
    let markdownUri = renderer $ MarkdownR
    setTitle "New note"
    addScriptRemote "//cdnjs.cloudflare.com/ajax/libs/jquery/1.8.2/jquery.min.js"
    addScriptRemote "//cdnjs.cloudflare.com/ajax/libs/underscore.js/1.3.3/underscore-min.js"
    addScript $ StaticR markitup_jquery_markitup_js
    addScript $ StaticR markitup_sets_markdown_set_js
    addStylesheet $ StaticR markitup_skins_markitup_style_css
    addStylesheet $ StaticR markitup_sets_markdown_style_css
    $(widgetFile "preview")
    $(widgetFile "add")

postAddR :: Handler RepHtml
postAddR = do
  (Entity userId _) <- requireAuth
  ((result, _), _) <- runFormPost $ renderDivs (noteForm Nothing)
  case result of
       FormSuccess note -> do
         time <- liftIO getCurrentTime
         noteId <- runDB $ insert $ Note userId (fnoteTitle note) (fnoteContent note) (fnoteTopic note)
         _ <- getUrlRender >>= indexNote noteId time
         redirect $ EditR noteId
       _ -> redirect AddR
