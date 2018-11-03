module Common.Config where

import Prelude

import Color (Color, fromHexString)
import Color.Scheme.X11 (gray, lightblue)
import Data.Maybe (fromMaybe)

primaryColor :: Color
primaryColor = fromMaybe lightblue $ fromHexString "#3490DC"

secondaryColor :: Color 
secondaryColor = fromMaybe gray $ fromHexString "#606c76"

smWidth :: Number
smWidth = 768.0
