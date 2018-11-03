module Page.Post (writePosts) where
  
import Prelude

import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Effect (Effect)
import Node.FS.Sync (readdir)
import Node.Path as Path
import Template.Default (defaultTemplate)
import Text.Markdown.SlamDown.Smolder (toMarkup)
import Text.Smolder.HTML as HTML
import Text.Smolder.HTML.Attributes as HA
import Text.Smolder.Markup (Markup, (!), text)
import Utils.Markdown (getFirstParagraph)
import Utils.String (fileNameToTitle, replaceExtension)
import Utils.Writer (distDir, postsDir, pagesSrc, toMarkDown, writeHtml)

postLink :: forall e. String -> Markup e 
postLink fName = HTML.li $ HTML.a ! HA.href url $ text titleName
  where
    titleName = fileNameToTitle fName
    htmlName = replaceExtension "html" fName
    url = Path.concat ["/", postsDir, htmlName]

postsList :: forall e. Array String -> Markup e 
postsList fileNames = 
  HTML.div ! HA.className "content" $
    HTML.ul do 
      traverse_ postLink fileNames

postsIndex :: forall e. Array String -> Markup e 
postsIndex fileNames = 
  defaultTemplate "Technical Blog and Notes" "The list of my technical blog posts and writings" do 
    HTML.div ! HA.className "wrapper" $ do
      HTML.div ! HA.className "container grid-lg" $ do
        HTML.hr ! HA.className "mt-2"
        HTML.article ! HA.className "article" $ do 
          HTML.h1 $ text "Blog"
          HTML.h3 ! HA.className "text-center" $ text "Simple Technical Blog and Notes"
          HTML.div ! HA.className "pt-4" $
            postsList fileNames

writeHtmlFromMarkdown :: Path.FilePath -> Effect Unit 
writeHtmlFromMarkdown p = do 
  emd <- toMarkDown postPath
  case emd of 
    Left _ -> pure unit  
    Right md -> writeHtml distPath $ defaultTemplate (fileNameToTitle p) (getFirstParagraph md) $ do 
      HTML.div ! HA.className "wrapper" $ 
        HTML.div ! HA.className "container grid-lg" $ do
          HTML.hr ! HA.className "mt-2"  
          HTML.article ! HA.className "article" $ 
            toMarkup md
  where
    postPath = Path.concat [pagesSrc, p]
    distPath = Path.concat [distDir, postsDir, replaceExtension "html" p]

writePostsIndex :: Array String -> Effect Unit 
writePostsIndex = writeHtml (Path.concat [distDir, postsDir, "index.html"]) <<< postsIndex 

writePosts :: Effect Unit
writePosts = do
  paths <- readdir pagesSrc
  writePostsIndex paths
  traverse_ writeHtmlFromMarkdown paths
  
