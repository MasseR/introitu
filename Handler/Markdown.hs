module Handler.Markdown where

import Import
import qualified Data.Text as T
import Text.Pandoc (writeHtml, readMarkdown, defaultWriterOptions, defaultParserState)
import Text.HTML.SanitizeXSS

renderMarkdown :: Text -> Html
renderMarkdown  = (renderToHtml . prepareText)
  where
        renderToHtml = (writeHtml defaultWriterOptions) . (readMarkdown defaultParserState)
        prepareText = T.unpack . T.filter (/= '\r') . sanitize

postMarkdownR :: Handler RepHtml
postMarkdownR = do
  markdown <- runInputPost $ iopt textareaField "markdown"
  let rendered = maybe "" (renderMarkdown . unTextarea) markdown
  hamletToRepHtml $ [hamlet|#{rendered}|]
