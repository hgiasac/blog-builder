module Home.Render where 

import Prelude

import Color (lighten, white)
import Common.Config (primaryColor, secondaryColor, smWidth)
import Control.Apply (lift3)
import Data.Array (sortBy, (..))
import Data.Foldable (foldMap)
import Data.Int (toNumber)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (unwrap)
import Data.Set (isEmpty)
import FRP.Behavior (Behavior, fixB, integral', switcher)
import FRP.Behavior.Mouse (buttons)
import FRP.Behavior.Mouse as Mouse
import FRP.Behavior.Time as Time
import FRP.Event.Mouse (Mouse, down)
import Global (infinity)
import Graphics.Drawing (Drawing, Point, circle, closed, fillColor, filled, lineWidth, outlineColor, outlined, rectangle, scale, translate)
import Home.Behavior (windowSizeBehavior)
import Home.Event (ScreenSize, WindowSize)

type Position = { x :: Number, y :: Number }
type Circle = { x :: Number, y :: Number, size :: Number  }

renderBackground :: ScreenSize -> Drawing
renderBackground { w, h } = leftBg <> rightBg
  where
        
    scaleFactor :: Number 
    scaleFactor = max w h / 16.0

    pointTopLeft :: Point
    pointTopLeft = { x: 0.0, y: 0.0 }

    pointTopRight :: Point
    pointTopRight = { x: w, y: 0.0 }

    pointBottomLeft :: Point
    pointBottomLeft = { x: 0.0, y: h }

    pointBottomRight :: Point
    pointBottomRight = { x: w, y: h }
    
    leftBg :: Drawing 
    leftBg = filled (fillColor primaryColor) renderedShape
      where
        smShape = closed [pointTopRight, pointBottomLeft, pointBottomRight]
        lgShape = rectangle 0.0 0.0 (w / 2.0) h
        renderedShape = if w < smWidth then smShape else lgShape

    rightBg :: Drawing
    rightBg = filled (fillColor white) renderedShape
      where
        smShape = closed [pointTopLeft, pointTopRight, pointBottomLeft] 
        lgShape = rectangle (w / 2.0) 0.0 w h
        renderedShape = if w < smWidth then smShape else lgShape

renderCircles :: Maybe { x :: Int, y :: Int } -> Number -> ScreenSize -> Drawing
renderCircles m sw { w, h } = renderCircles' toCircles

  where
    scaleFactor :: Number 
    scaleFactor = max w h / 16.0

    renderCircle :: Circle -> Drawing
    renderCircle { x, y, size } = 
      scale scaleFactor scaleFactor <<< translate x y <<< scale size size $
        outlined
          (outlineColor (lighten (0.2 + size * 0.2) secondaryColor) <> lineWidth ((1.0 + size * 2.0 ) / scaleFactor))
          (circle 0.0 0.0 0.5)

    renderCircles' :: Array Circle -> Drawing
    renderCircles' = foldMap renderCircle

    oppositePos :: { x :: Int, y :: Int } -> Position
    oppositePos { x, y } = 
      { x: calc (toNumber x) w  
      , y: calc (toNumber y) h
      }
      where 
        calc n t
          | n /= (t / 2.0) = t - n
          | otherwise = n
    
    toCircles :: Array Circle
    toCircles =
      sortBy (comparing (\{ x, y } -> dist x y m)) do 
        i <- 0 .. 16 
        j <- 0 .. 16
        let x = toNumber i 
            y = toNumber j 
            d = dist x y m
        pure { x
             , y 
             , size: 0.1 + (1.0 + sw) / (d + 1.5)
             }
      where
        dist x y = maybe infinity \pos ->
          let 
            { x: mx, y: my } = oppositePos pos
            dx = x - mx / scaleFactor
            dy = y - my / scaleFactor
          in dx * dx + dy * dy 

scene :: Mouse -> WindowSize -> Behavior Drawing
scene mouse wdSize = renderBg <> circles where 

  renderBg :: Behavior Drawing
  renderBg = renderBackground <$> windowSizeBehavior wdSize

  swell :: Behavior Number 
  swell =
    fixB 2.0 \b -> 
      integral' 2.0 (unwrap <$> Time.seconds)
        let db = fixB 10.0 \db_ -> 
          integral' 10.0 (unwrap <$> Time.seconds) (f <$> buttons mouse <*> b <*> db_)
        in switcher db (down $> db)
    where 
      f bs s ds | isEmpty bs = -8.0 * (s - 1.0) - ds * 2.0
                | otherwise = 2.0 * (4.0 - s)
  
  circles :: Behavior Drawing
  circles = lift3 renderCircles (Mouse.position mouse) swell $ windowSizeBehavior wdSize
