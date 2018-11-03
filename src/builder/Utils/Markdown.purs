module Utils.Markdown where
  
import Prelude

import Data.List (List(..), foldl, (:))
import Text.Markdown.SlamDown (Block(..), Inline(..), SlamDownP(..))
  

getFirstParagraph :: forall a. SlamDownP a -> String
getFirstParagraph (SlamDown bss) = getFirstParagraph' bss where
  getFirstParagraph' Nil = ""
  getFirstParagraph' (b:bs) =
    case b of 
      (Paragraph is) -> toInlines is 
      _ -> getFirstParagraph' bs 

toInlines :: forall a. List (Inline a) -> String
toInlines is = foldl (\str il -> str <> toInline il) "" is

toInline :: forall a. Inline a -> String
toInline il = case il of
  (Str s) -> s
  (Entity s) -> s
  (Space) -> " "
  (SoftBreak) -> " "
  (LineBreak) -> " "
  _ -> ""
