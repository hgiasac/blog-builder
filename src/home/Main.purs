module Main where

import Prelude

import Data.Maybe (fromJust)
import Effect (Effect)
import FRP.Behavior (animate)
import FRP.Event (subscribe)
import FRP.Event.Mouse (getMouse)
import Graphics.Canvas (CanvasElement, getCanvasElementById, getContext2D, setCanvasHeight, setCanvasWidth)
import Graphics.Drawing (render)
import Home.Event (ScreenSize, getWindowSize, resizeEvent, windowSize)
import Home.Render (scene)
import Partial.Unsafe (unsafePartial)
import Web.HTML (window)

setCanvasSize :: CanvasElement -> ScreenSize -> Effect Unit 
setCanvasSize mcanvas {w, h} = do
  setCanvasWidth mcanvas w 
  setCanvasHeight mcanvas h


runBackground :: Effect Unit 
runBackground = do 
  wd <- window
  mouse <- getMouse
  wdSize <- windowSize
  mcanvas <- (unsafePartial fromJust) <$> getCanvasElementById "bg"
  ctx <- getContext2D mcanvas 
  setCanvasSize mcanvas =<< getWindowSize wd
  _ <- resizeEvent `subscribe` (setCanvasSize mcanvas)
  _ <-  animate (scene mouse wdSize) (render ctx)
  pure unit

main :: Effect Unit
main = do
  runBackground
