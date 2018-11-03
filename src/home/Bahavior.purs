module Home.Behavior where

import Prelude
import FRP.Behavior (Behavior, behavior)
import Home.Event (WindowSize, ScreenSize, withWindowSize)


windowSizeBehavior :: WindowSize -> Behavior ScreenSize
windowSizeBehavior ws = behavior \e -> (\{ value, size } -> value size) <$> (withWindowSize ws e)
