module Utils.String where 

import Prelude

import Data.Array (init)
import Data.Foldable (intercalate)
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), Replacement(..))
import Data.String as Str

stripExtension :: String -> String 
stripExtension s = intercalate "." (fromMaybe ss $ init ss)
  where
    ss = Str.split (Pattern ".") s

replaceExtension :: String -> String -> String 
replaceExtension ex s = stripExtension s <> "." <> ex

fileNameToUri :: String -> String
fileNameToUri = Str.replaceAll (Pattern "_") (Replacement "-")

fileNameToTitle :: String -> String 
fileNameToTitle fileName = title <> " (" <> before <> ")"
  where 
    { after, before } = Str.splitAt 10 $ stripExtension fileName
    title = Str.replaceAll (Pattern "_") (Replacement " ") $ Str.drop 1 after

stripeSentences :: String -> Int -> String 
stripeSentences s maxN 
  | Str.length s <= maxN = s
  | otherwise = let str = Str.split (Pattern ".") (Str.take maxN s)
                in (intercalate "." $ fromMaybe str (init str)) <> "..."


