module Page.Home.CSS where
  
import CSS 
import CSS.Common (none, auto)
import CSS.ListStyle.Type (listStyleType)
import CSS.Media as CM
import CSS.TextAlign (rightTextAlign, textAlign)
import Common.Config (smWidth)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..))
import Prelude (discard, show, ($), (<>), (+))

homeNavSelector :: Selector 
homeNavSelector = fromString ".homenav"

bgSelector :: Selector
bgSelector = fromString "bg" 

absSize :: String -> Number -> Size Abs 
absSize u i = Size (value i <> fromString u)

vhAbs :: Number -> Size Abs 
vhAbs = absSize "vh"

vwAbs :: Number -> Size Abs 
vwAbs = absSize "vw"

minNavWidth :: Number 
minNavWidth = 150.0

leftScreenPosition :: Number 
leftScreenPosition = 50.0 + minNavWidth

homeStyle :: Maybe String
homeStyle = renderedSheet $ render $ do 
  fontFaceSrc $ NonEmpty (FontFaceSrcUrl "https://fonts.googleapis.com/css?family=Roboto:500,700" Nothing) []
  body ? do 
    fontFamily ["Roboto"] (NonEmpty sansSerif [])
    margin (px 0.0) (px 0.0) (px 0.0) (px 0.0)
  bgSelector ? do
    zIndex 0
    position fixed
  homeNavSelector ? do 
    position absolute
    left $ Size (fromString $ "calc(50% - " <> show leftScreenPosition <> "px)")
    bottom $ vh 10.0
    ul ? do 
      listStyleType none
      paddingLeft $ px 0.0
      minWidth $ px minNavWidth
      li ? do 
        textAlign rightTextAlign
        a ? do 
          fontSize $ px 40.0
          textDecoration noneTextDecoration
          color white
        a & hover ? do 
          fontWeight $ weight 700.0


  query CM.screen (NonEmpty (CM.maxWidth $ px smWidth) []) do 
    bgSelector ? do 
      display displayNone

    homeNavSelector ? do 
      left $ auto
      right $ vw 10.0
      position fixed
