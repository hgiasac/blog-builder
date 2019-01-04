module Page.Front (writeFrontPages) where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Effect (Effect)
import Node.FS.Sync (readdir)
import Node.Path as Path
import Template.Common (webTitle)
import Template.Default (defaultTemplate)
import Text.Markdown.SlamDown.Smolder (toMarkup)
import Text.Smolder.HTML as HTML
import Text.Smolder.HTML.Attributes as HA
import Text.Smolder.Markup ((!))
import Utils.Markdown (getFirstParagraph)
import Utils.String (fileNameToTitle, replaceExtension)
import Utils.Writer (distDir, pagesDir, toMarkDown, writeHtml)


frontDir :: Path.FilePath
frontDir = "front"

frontSrc :: Path.FilePath
frontSrc = Path.concat [pagesDir, frontDir]

writeHtmlFromMarkdown :: Path.FilePath -> Effect Unit 
writeHtmlFromMarkdown p = do 
  emd <- toMarkDown postPath 
  case emd of 
    Left _ -> pure unit 
    Right md -> do 
      markup <- defaultTemplate (webTitle <> " - " <> pageTitle) (getFirstParagraph md) $ do 
        HTML.div ! HA.className "wrapper" $ 
          HTML.div ! HA.className "container grid-lg" $ do
            HTML.hr ! HA.className "mt-2"  
            HTML.article ! HA.className "article" $ 
              toMarkup md
      writeHtml distPath markup
  where
    postPath = Path.concat [frontSrc, p]
    distPath = Path.concat [distDir, replaceExtension "html" p]
    pageTitle = fileNameToTitle p

writeFrontPages :: Effect Unit
writeFrontPages = do
  paths <- readdir frontSrc
  traverse_ writeHtmlFromMarkdown paths
  
 