module Poem (renderWholePoem) where

import Control.Monad
import Data (sortCantoDir, wholePoem)
import Data.Maybe
import qualified Data.Text.Lazy as TL
import System.Directory.Tree
import qualified Text.Blaze.Html5 as H

showWholePoem' :: IO [String]
showWholePoem' = do
  whole <- wholePoem
  let cnt = contents whole :: [DirTree String]
      sorted =
        fromJust $
          forM
            cnt
            ( \c -> do
                let sorted_ = sortCantoDir c
                 in sorted_
            )
   in return $ concat <$> concat sorted

renderWholePoem :: IO H.Html
renderWholePoem = do
  whole <- showWholePoem'
  html <-
    forM
      (concatMap lines whole)
      ( \l ->
          let txt = H.toHtml $ TL.pack l
              elem_ = H.p txt
           in pure elem_
      )
  return $ mconcat html
