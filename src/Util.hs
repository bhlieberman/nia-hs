{-# LANGUAGE OverloadedStrings #-}


module Util (mkCantoNumeral, renderSVG) where

import qualified Data.Map as Map
import Data.Text
import System.IO
import Text.Blaze ()
import qualified Text.Blaze.Html5 as H

mkCantoNumeral :: Int -> String
mkCantoNumeral num =
  let cantoMap = Map.fromList [(1, "I"), (2, "II"), (4, "IV")] :: Map.Map Int String
   in cantoMap Map.! num

renderSVG :: FilePath -> IO H.Html
renderSVG fp =
  let raw = readFile' fp
   in H.toHtml . pack <$> raw
