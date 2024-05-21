module Data where

import qualified Data.Foldable as F
import Data.Maybe
import System.Directory.Tree

import Util (mkCantoNumeral)

walk :: IO (DirTree String) -> IO String
walk = fmap F.concat

wholePoem :: IO (DirTree String)
wholePoem = do
    _:/tree <- readDirectory "resources/public"
    let poem = filterDir (\d -> name d /= "js") tree
    return poem

byCanto :: Int -> IO (DirTree String)
byCanto c = do
    let cantoName = "canto_" ++ mkCantoNumeral c
    _:/tree <- readDirectory $ "resources/public/" ++ cantoName
    return tree

byFootnotes :: Int -> IO (Maybe (DirTree String))
byFootnotes f = do
    anc@(_:/_) <- readDirectory $ "resources/public/" ++ "canto_" ++ mkCantoNumeral f
    return $ dirTree <$> dropTo "footnotes" anc

allCantos :: IO [DirTree String]
allCantos = sequence [byCanto 1, byCanto 2, byCanto 4]

allFootnotes :: IO [DirTree String]
allFootnotes = do
    footnotes <- sequence [byFootnotes 1, byFootnotes 2, byFootnotes 4]
    return $ map fromJust footnotes