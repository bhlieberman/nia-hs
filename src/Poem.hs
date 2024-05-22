module Poem (renderWholePoem) where

import Control.Monad
import Data (walk, wholePoem)
import Data.Maybe
import qualified Data.Text.Lazy as TL
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

showWholePoem :: IO (Maybe String)
showWholePoem = do
  poem <- pure <$> wholePoem
  walk poem $ const True

renderWholePoem :: IO H.Html
renderWholePoem = do
  whole <- fromJust <$> showWholePoem
  html <-
    forM
      (lines whole)
      ( \l ->
          let txt = H.toHtml $ TL.pack l
              elem_ = H.p txt
           in pure elem_
      )
  return $ mconcat html
