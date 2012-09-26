module Handler.Add where

import Import

data NoteForm = NoteForm {
    fnoteTitle :: Text
  , fnoteContent :: Textarea
  , fnoteTopic :: Text} deriving Show

noteForm :: Maybe NoteForm -> AForm App App NoteForm
noteForm note = NoteForm <$>
  areq textField "Title" (fnoteTitle <$> note) <*>
  areq textareaField "Content" (fnoteContent <$> note) <*>
  areq textField "Topic" (fnoteTopic <$> note)

getAddR :: Handler RepHtml
getAddR = do
  ((_, formWidget), enctype) <- runFormPost $ renderDivs (noteForm Nothing)
  renderer <- getUrlRender
  defaultLayout $ do
    let markdownUri = renderer $ MarkdownR
    setTitle "New note"
    addScriptRemote "//cdnjs.cloudflare.com/ajax/libs/jquery/1.8.2/jquery.min.js"
    addScriptRemote "//cdnjs.cloudflare.com/ajax/libs/underscore.js/1.3.3/underscore-min.js"
    $(widgetFile "preview")
    $(widgetFile "add")

postAddR :: Handler RepHtml
postAddR = do
  (Entity userId _) <- requireAuth
  ((result, _), _) <- runFormPost $ renderDivs (noteForm Nothing)
  case result of
       FormSuccess note -> do
         noteId <- runDB $ insert $ Note userId (fnoteTitle note) (fnoteContent note) (fnoteTopic note)
         redirect $ EditR noteId
       _ -> redirect AddR
