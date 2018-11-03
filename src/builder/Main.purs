module Builder.Main where

import Prelude

import Effect (Effect)
import Page.Front (writeFrontPages)
import Page.Home (writeHome)
import Page.Post (writePosts)
import Utils.Writer (prepareDirs)

main :: Effect Unit
main = do
  prepareDirs
  writeHome
  writeFrontPages
  writePosts
