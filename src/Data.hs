module Data where

import qualified Data.Foldable as F
import Data.List
import System.Directory.Tree
import Util (mkCantoNumeral)

walk :: Maybe (DirTree String) -> (DirTree String -> Bool) -> IO (Maybe String)
walk d filt =
  let filterFn = filterDir filt
   in return $ fmap (F.concat . sortDir . filterFn) d

wholePoem :: IO (AnchoredDirTree String)
wholePoem = filterDir (\d -> name d /= "assets") </$> readDirectory "resources/public"

sortCantoDir :: DirTree String -> Maybe [DirTree String]
sortCantoDir dir =
  let unsorted = contents dir
      thesis = find (\c -> name c == "thesis.txt") unsorted
      parens = sortDir <$> find (\c -> name c == "parenthesis") unsorted
      footnotes = sortDir <$> find (\c -> name c == "footnotes") unsorted
   in sequenceA [thesis, parens, footnotes]

byCanto' :: Int -> IO String
byCanto' c = do
  let path = mconcat ["resources/public/", "canto_", mkCantoNumeral c, "/thesis.txt"]
  (_ :/ tree) <- readDirectory path
  putStrLn $ "searching for file at " ++ path
  return $ file tree

mkResourcePath :: Int -> String
mkResourcePath c = "resources/public/" ++ "canto_" ++ mkCantoNumeral c

resourceDir :: Int -> IO (AnchoredDirTree String)
resourceDir c = readDirectory $ mkResourcePath c

dropTo' :: AnchoredDirTree a -> FileName -> Maybe (AnchoredDirTree a)
dropTo' = flip dropTo

footnotesParens :: String -> Int -> IO (Maybe (DirTree String))
footnotesParens fp i = do
  dir <- resourceDir i
  return $ dirTree <$> dropTo' dir fp

allCantos :: IO [String]
allCantos = sequence [byCanto' 1, byCanto' 2, byCanto' 4]

byParens' :: [Int] -> AnchoredDirTree String -> IO (Maybe String)
byParens' parens dir = do
  let tree = dirTree <$> dropTo "parenthesis" dir
  let filt_ = map (\i -> "par_" ++ show i ++ ".txt") parens
  let filt = (\d -> name d `elem` filt_)
  walk tree filt

allFootnotes :: IO [Maybe (DirTree String)]
allFootnotes =
  let f = footnotesParens "footnotes"
   in sequence [f 1, f 2, f 4]

allParens :: IO [Maybe (DirTree String)]
allParens =
  let f = footnotesParens "parenthesis"
   in sequence [f 1, f 2, f 4]
