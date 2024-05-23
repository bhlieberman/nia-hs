{-# LANGUAGE OverloadedStrings #-}

module Poem (renderWholePoem) where

import Control.Monad
import Data (sortCantoDir, wholePoem)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import System.Directory.Tree
import Text.Blaze (textValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

showWholePoem' :: IO [String]
showWholePoem' = do
  whole <- wholePoem
  let cnt = contents whole :: [DirTree String]
      sorted =
        fromJust $
          forM
            cnt
            ( \c ->
                let sorted_ = sortCantoDir c
                 in sorted_
            )
   in return $ concat <$> concat sorted

renderWholePoem :: IO H.Html
renderWholePoem = do
  whole <- showWholePoem'
  let indexed = zip [1 ..] (concatMap lines whole) :: [(Int, String)]
  html <-
    forM
      indexed
      ( \(i, l) ->
          let txt = H.toHtml $ TL.pack l :: H.Html
              idVal = textValue $ T.pack $ "line-" ++ show i
              elem_ = H.p H.! A.id idVal $ txt
           in pure elem_
      )
  return $ mconcat html
