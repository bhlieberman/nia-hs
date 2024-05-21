module Util (mkCantoNumeral) where

import qualified Data.Map as Map

mkCantoNumeral :: Int -> String
mkCantoNumeral num =
  let cantoMap = Map.fromList [(1, "I"), (2, "II"), (4, "IV")] :: Map.Map Int String
   in cantoMap Map.! num