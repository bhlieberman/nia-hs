module Data where

import qualified Data.Foldable as F
import System.Directory.Tree

import Util (mkCantoNumeral)

walk :: Maybe (DirTree String) -> IO (Maybe String)
walk d = return $ fmap (F.concat . sortDir) d

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

byParens :: Int -> IO (Maybe (DirTree String))
byParens p = do
    anc@(_:/_) <- readDirectory $ "resources/public/" ++ "canto_" ++ mkCantoNumeral p
    return $ dirTree <$> dropTo "parenthesis" anc

allCantos :: IO [DirTree String]
allCantos = sequence [byCanto 1, byCanto 2, byCanto 4]

allFootnotes :: IO [Maybe (DirTree String)]
allFootnotes = sequence [byFootnotes 1, byFootnotes 2, byFootnotes 4]

allParens :: IO [Maybe (DirTree String)]
allParens = sequence [byParens 1, byParens 2, byParens 4]