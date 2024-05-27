{-# LANGUAGE OverloadedStrings #-}

module Util (mkCantoNumeral, mkScriptTag, renderSVG) where

import qualified Data.Map as Map
import System.IO
import Text.Blaze ()
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

mkCantoNumeral :: Int -> String
mkCantoNumeral num =
  let cantoMap = Map.fromList [(1, "I"), (2, "II"), (4, "IV")] :: Map.Map Int String
   in cantoMap Map.! num

mkScriptTag :: H.Html -> H.Html
mkScriptTag = H.script H.! A.src "/assets/js/client/scroller.js"

renderSVG :: FilePath -> IO H.Html
renderSVG fp =
  let raw = readFile' fp
   in H.toHtml <$> raw
