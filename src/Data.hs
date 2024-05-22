module Data where

import qualified Data.Foldable as F
import System.Directory.Tree
import Util (mkCantoNumeral)

walk :: Maybe (DirTree String) -> (DirTree String -> Bool) -> IO (Maybe String)
walk d filt =
  let filterFn = filterDir filt
   in return $ fmap (F.concat . sortDir . filterFn) d

wholePoem :: IO (DirTree String)
wholePoem = do
  _ :/ tree <- readDirectory "resources/public"
  let poem = filterDir (\d -> name d /= "js") tree
  return poem

byCanto :: Int -> IO (AnchoredDirTree String)
byCanto c = do
  let cantoName = "canto_" ++ mkCantoNumeral c
   in readDirectory $ "resources/public/" ++ cantoName

byFootnotes :: Int -> IO (Maybe (DirTree String))
byFootnotes f = do
  anc@(_ :/ _) <- readDirectory $ "resources/public/" ++ "canto_" ++ mkCantoNumeral f
  return $ dirTree <$> dropTo "footnotes" anc

byParens :: Int -> IO (Maybe (DirTree String))
byParens p = do
  anc@(_ :/ _) <- readDirectory $ "resources/public/" ++ "canto_" ++ mkCantoNumeral p
  return $ dirTree <$> dropTo "parenthesis" anc

allCantos :: IO [AnchoredDirTree String]
allCantos = sequence [byCanto 1, byCanto 2, byCanto 4]

byParens' :: [Int] -> AnchoredDirTree String -> IO (Maybe String)
byParens' parens dir = do
  let tree = dirTree <$> dropTo "parenthesis" dir
  let filt_ = map (\i -> "par_" ++ show i ++ ".txt") parens
  let filt = (\d -> name d `elem` filt_)
  walk tree filt

allFootnotes :: IO [Maybe (DirTree String)]
allFootnotes = sequence [byFootnotes 1, byFootnotes 2, byFootnotes 4]

allParens :: IO [Maybe (DirTree String)]
allParens = sequence [byParens 1, byParens 2, byParens 4]
