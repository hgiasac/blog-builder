module Home.Event where

import Prelude

import Data.Int (toNumber)
import Effect (Effect)
import Effect.Ref as Ref
import FRP.Event (Event, makeEvent, subscribe)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Window as WD

type ScreenSize = { w :: Number, h :: Number }

resizeEventType :: EventType 
resizeEventType = EventType "resize"
  
getWindowSize :: WD.Window -> Effect ScreenSize
getWindowSize wd = do 
  w <- toNumber <$> WD.outerWidth wd
  h <- toNumber <$> WD.outerHeight wd

  pure { w, h }

newtype WindowSize = WindowSize
  { size :: Ref.Ref ScreenSize
  , dispose :: Effect Unit
  }
  
-- | Get a handle for working with the window resize.
windowSize :: Effect WindowSize
windowSize = do
  wd <- window
  wh <- getWindowSize wd

  size <- Ref.new wh
  let target = WD.toEventTarget wd

  resizeEv <- eventListener \_ -> do
    newSize <- getWindowSize wd
    Ref.write newSize size

  addEventListener resizeEventType resizeEv false target
  let dispose = removeEventListener resizeEventType resizeEv false target

  pure (WindowSize { size, dispose })

disposeWindowSize :: WindowSize -> Effect Unit
disposeWindowSize (WindowSize { dispose }) = dispose


resizeEvent :: Event ScreenSize
resizeEvent = makeEvent \k -> do
  wd <- window
  let wdTarget = WD.toEventTarget wd
  resizeEv <- eventListener \_ -> k =<< getWindowSize wd

  addEventListener resizeEventType resizeEv false wdTarget
  pure (removeEventListener resizeEventType resizeEv false wdTarget)

-- | Create an event which also returns the current window size.
withWindowSize
  :: forall a
  . WindowSize
  -> Event a
  -> Event { value :: a, size :: ScreenSize }
withWindowSize (WindowSize { size }) e = makeEvent \k ->
  e `subscribe` \value -> do
    s <- Ref.read size
    k { value, size: s }
