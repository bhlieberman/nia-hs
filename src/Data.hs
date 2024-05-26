module Data where

import qualified Data.Foldable as F
import System.Directory.Tree
import Text.Blaze (AttributeValue, stringValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Data.Text.Lazy as TL
import Util (mkCantoNumeral)

walk :: Maybe (DirTree String) -> (DirTree String -> Bool) -> IO (Maybe String)
walk d filt =
  let filterFn = filterDir filt
   in return $ fmap (F.concat . sortDir . filterFn) d

wholePoem :: IO (AnchoredDirTree String)
wholePoem = filterDir (\d -> name d /= "assets") </$> readDirectory "resources/public"

isFile :: DirTree a -> Bool
isFile f =
  case f of
    File _ _ -> True
    _ -> False

getCanto' :: Int -> IO (DirTree String)
getCanto' i = do
  let num = mkCantoNumeral i
  _:/tree <- readDirectory $ "resources/public/canto_" ++ num
  return tree

getCantoFiles :: DirTree String -> [DirTree String]
getCantoFiles d = filter isFile $ contents $ sortDir d

getCantoText :: [DirTree String] -> String
getCantoText d = mconcat $ map file d

getCantoHtml :: [DirTree String] -> H.Html
getCantoHtml d =
  let files_ = zip [1 ..] $ concatMap (lines . file) d :: [(Int, String)]
      mkId i =
        let id_ = "line-" ++ show i :: String
         in stringValue id_ :: AttributeValue
      elem_ (i, txt) = H.p H.! A.id (mkId i) $ (H.toHtml . TL.pack) txt
      files = map elem_ files_
   in mconcat files

mkResourcePath :: Int -> String
mkResourcePath c = "resources/public/" ++ "canto_" ++ mkCantoNumeral c

resourceDir :: Int -> IO (AnchoredDirTree String)
resourceDir c = readDirectory $ mkResourcePath c
