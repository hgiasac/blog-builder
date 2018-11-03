module Page.Home (writeHome) where

import Prelude

import Data.Array (sortBy, tail)
import Data.Maybe (fromMaybe, maybe)
import Data.String (length)
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (writeTextFile)
import Node.Path as Path
import Page.Home.CSS (homeStyle)
import Template.Common (navLinkData, navListMarkup, webTitle)
import Text.Smolder.HTML (canvas, script)
import Text.Smolder.HTML as HTML
import Text.Smolder.HTML.Attributes as HA
import Text.Smolder.Markup (Markup, empty, text, (!))
import Utils.Writer (distDir, writeHtml, assetsDir)

homeHead :: forall e. Markup e 
homeHead = HTML.head do 
  HTML.meta ! HA.charset "utf-8"
  HTML.meta ! HA.name "viewport" ! HA.content "width=device-width, initial-scale=1"
  HTML.link ! HA.rel "stylesheet" ! HA.href (Path.concat ["/", assetsDir, "home.css"])
  
  HTML.title $ text ("Toan Nguyen - " <> webTitle)
  HTML.meta 
    ! HA.name "description" 
    ! HA.content "I am Toan Nguyen, a simple developer who love to code. I currently focus on functional programming"

homeNav :: forall e. Markup e 
homeNav = navListMarkup "homenav" $ sortBy sortFn (fromMaybe [] (tail navLinkData))
  where
    sortFn [l1, s1] [l2, s2] = compare (length s1) (length s2) <> compare s1 s2 
    sortFn _ _ = EQ

homeMarkup :: forall e. Markup e
homeMarkup = do
  homeHead
  HTML.body do
    canvas ! HA.id "bg" $ text " "
    homeNav

    script ! HA.src (Path.concat ["/", assetsDir, "home.min.js"]) $ empty

writeHomeCss :: Effect Unit 
writeHomeCss = maybe (pure unit) (writeTextFile UTF8 dest) homeStyle
  where 
    dest = Path.concat [distDir, assetsDir, "home.css"]

writeHome :: Effect Unit
writeHome = do
  writeHomeCss
  writeHtml dest homeMarkup
  where
    dest = Path.concat [distDir, "index.html"]
