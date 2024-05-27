{-# LANGUAGE OverloadedStrings #-}

module Server (runScotty) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import qualified Data as D
import Data.ByteString.Builder (lazyByteString)
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy.UTF8 as L
import Data.Either
import qualified Data.List.Split as S
import qualified Data.Text.Lazy as TL
import Network.URI
import Network.Wai
import Network.Wai.Middleware.Static
import System.IO
import Templates
import qualified Text.Blaze.Html.Renderer.Text as RT
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5 as H
import Util (mkCantoNumeral)
import Web.Scotty (ActionM, get, html, middleware, parseParamList, pathParam, scotty, setHeader, stream, text)

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

silentFailParse :: String -> [Int]
silentFailParse s = fromRight [] $ decodeParams s

standardTextResp :: [String] -> ActionM ()
standardTextResp s = text $ mconcat $ map TL.pack s

-- ty https://github.com/renanpvaz/rickastley.live/tree/master for the inspo...
streaming :: StreamingBody
streaming write flush_ = do
  canto_1 <- liftIO $ D.getCanto' 1
  canto_2 <- liftIO $ D.getCanto' 2
  let html_ = D.getCantoHtml . D.getCantoFiles $ canto_1
  let html_2 = D.getCantoHtml . D.getCantoFiles $ canto_2
  let lines' = mconcat [html_, html_2]
  let loop (l : ls) = do
        write $ lazyByteString $ C.unlines l
        flush_
        loop ls
      loop [] = return ["\0"]
  void $ loop lines'

runScotty :: IO ()
runScotty = scotty 3000 $ do
  middleware $ staticPolicy (noDots >-> addBase "resources/public")
  get "/infinite" $ do
    whole_ <- liftIO wholeTemplate
    html $ RT.renderHtml whole_
  get "/streamed" $ do
    setHeader "Access-Control-Allow-Origin" "*"
    stream streaming
  get "/canto/html/:id" $ do
    id_ <- pathParam "id" :: ActionM Int
    canto_ <- liftIO $ D.getCanto' id_
    let html_ = D.getCantoHtml . D.getCantoFiles $ canto_
    template <- liftIO $ mainLayout $ pure ()
    html $ RT.renderHtml template
  get "/canto/text/:id" $ do
    id_ <- pathParam "id"
    canto_ <- liftIO $ D.getCanto' id_
    text $ (TL.pack . D.getCantoText . D.getCantoFiles) canto_
  get "/footnotes/:canto/:footnote" $ do
    canto <- pathParam "canto"
    footnote <- pathParam "footnote"
    let parsed = silentFailParse footnote
    footnotes <- liftIO $ getCantoFootnotes canto parsed
    standardTextResp footnotes

-- have a route that returns text and one that returns HTML for each of
-- parens and footnotes...
