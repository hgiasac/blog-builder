module Template.Common where 

import Prelude

import Data.Foldable (traverse_)
import Text.Smolder.HTML as HTML
import Text.Smolder.HTML.Attributes as HA
import Text.Smolder.Markup (Markup, empty, text, (!))

webTitle :: String 
webTitle = "A Simple Developer"
  
copyright :: String
copyright = "© Toan Nguyen 2017-2018"

iconLink :: forall e. String -> String -> Markup e
iconLink url iconClass = 
  HTML.a ! HA.className "secondary" ! HA.href url $  
    HTML.i ! HA.className iconClass $ text " "

navLinkData :: Array (Array String)
navLinkData = 
  [ ["/", "Home"]
  , ["/works.html", "Works"]
  , ["/posts", "Posts"]
  , ["/about.html", "About"]
  , ["https://www.visualcv.com/toan-nguyen", "CV"]
  ]

navItemMarkup :: forall e. Array String -> Markup e 
navItemMarkup [url, name] =   HTML.li $ HTML.a ! HA.href url $ text name 
navItemMarkup _ = empty

navListMarkup :: forall e. String -> Array (Array String) -> Markup e 
navListMarkup cls links = 
  HTML.nav ! HA.className cls $ 
    HTML.ul $ traverse_ navItemMarkup links

defaultNav :: forall e. String -> Markup e
defaultNav cls = navListMarkup cls navLinkData 
