{-# LANGUAGE OverloadedStrings #-}

module Server (runScotty) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data (byCanto')
import qualified Data as D
import Data.Either
import Data.Maybe
import qualified Data.Text.Lazy as TL
import Network.URI
import Network.Wai.Middleware.Static
import System.IO
import Templates
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Util (mkCantoNumeral)
import Web.Scotty (ActionM, get, html, middleware, parseParamList, pathParam, queryParamMaybe, scotty, text)

getPoemTheses :: IO [String]
getPoemTheses = forM ["canto_I", "canto_II", "canto_IV"] $ do
  ( \n -> do
      let path_ = "resources/public/" ++ n ++ "/thesis.txt"
      readFile' path_
    )

decodeParams :: String -> Either TL.Text [Int]
decodeParams s = parseParamList . TL.pack $ unEscapeString s

stdErrorHandler :: IOException -> IO String
stdErrorHandler e = do
  hPutStr stderr $ show e
  return ""

getCantoFootnotes :: Int -> [Int] -> IO [String]
getCantoFootnotes canto footnote =
  forM ["footnote_" ++ show filename ++ ".txt" | filename <- footnote :: [Int]] $ do
    ( \f -> do
        let romanNumeral = mkCantoNumeral canto
            path_ = "resources/public/canto_" ++ romanNumeral ++ "/footnotes/" ++ f
         in readFile' path_ `catch` stdErrorHandler
      )

mkScriptTag :: H.Html -> H.Html
mkScriptTag = H.script H.! A.type_ "text/plain"

silentFailParse :: String -> [Int]
silentFailParse s = fromRight [] $ decodeParams s

standardTextResp :: [String] -> ActionM ()
standardTextResp s = text $ mconcat $ map TL.pack s

runScotty :: IO ()
runScotty = scotty 3000 $ do
  middleware $ staticPolicy (noDots >-> addBase "resources/public")
  get "/" $ do
    thesis <- liftIO getPoemTheses
    template <- liftIO homeTemplate
    let theses = map (mkScriptTag . H.toHtml) thesis
    html $ renderHtml $ mconcat [template, mconcat theses]
  get "/infinite" $ do
    whole_ <- liftIO wholeTemplate
    html $ renderHtml whole_
  get "/canto/:id" $ do
    id_ <- pathParam "id"
    canto_ <- liftIO $ byCanto' id_
    template <- liftIO $ mainLayout . H.toHtml . TL.pack $ canto_
    html $ renderHtml template
  get "/footnotes/:canto/:footnote" $ do
    canto <- pathParam "canto"
    footnote <- pathParam "footnote"
    let parsed = silentFailParse footnote
    footnotes <- liftIO $ getCantoFootnotes canto parsed
    standardTextResp footnotes
  -- have a route that returns text and one that returns HTML for each of
  -- parens and footnotes...
  get "/parens/:canto" $ do
    canto <- pathParam "canto"
    filt <- queryParamMaybe "parens"
    filter_ <-
      case filt of
        Just vals -> liftIO $ return $ silentFailParse vals
        _ -> liftIO $ return [1 .. 5]
    let canto_ = D.byCanto canto >>= D.byParens' filter_
    resp <- liftIO $ TL.pack . fromJust <$> canto_
    text resp
