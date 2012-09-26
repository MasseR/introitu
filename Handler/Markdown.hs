module Handler.Markdown where

import Import
import Data.Text (pack)
import qualified Data.Text as T (filter)
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Pandoc (writeHtml, readMarkdown, defaultWriterOptions, defaultParserState)

renderMarkdown :: Text -> Html
renderMarkdown  = (renderToHtml . prepareText)
  where
        renderToHtml = (writeHtml defaultWriterOptions) . (readMarkdown defaultParserState)
        prepareText = renderHtml . toHtml . T.filter (/= '\r')

htmlToText :: Html -> Text
htmlToText = pack . renderHtml

postMarkdownR :: Handler RepHtml
postMarkdownR = do
  markdown <- runInputPost $ iopt textareaField "markdown"
  let rendered = maybe "" (renderMarkdown . unTextarea) markdown
  hamletToRepHtml $ [hamlet|#{rendered}|]
