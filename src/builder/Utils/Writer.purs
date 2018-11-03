module Utils.Writer where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either)
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (mkdir, readTextFile, writeTextFile)
import Node.Path as Path
import Text.Markdown.SlamDown (SlamDownP)
import Text.Markdown.SlamDown.Parser (parseMd)
import Text.Smolder.Markup (Markup)
import Text.Smolder.Renderer.String (render)

distDir :: Path.FilePath
distDir = "dist"

postsDir :: Path.FilePath
postsDir = "posts"
  
pagesDir :: Path.FilePath
pagesDir = "pages"

assetsDir :: String 
assetsDir = "assets"
  
pagesSrc :: Path.FilePath
pagesSrc = Path.concat[pagesDir, postsDir]

prepareDirs :: Effect Unit 
prepareDirs = do 
  mkdir distDir
  mkdir $ Path.concat [distDir, assetsDir]
  mkdir $ Path.concat [distDir, postsDir]

toMarkDown :: Path.FilePath -> Effect (Either String (SlamDownP String))
toMarkDown p = do
  content <- readTextFile UTF8 p 
  pure $ lmap (\e -> e <> ". Path: " <> p) $ parseMd content

writeHtml :: forall e. Path.FilePath -> Markup e -> Effect Unit
writeHtml fp = writeTextFile UTF8 fp <<< render
