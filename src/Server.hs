{-# LANGUAGE OverloadedStrings #-}

module Server (runScotty) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Either
import qualified Data.Text.Lazy as TL
import Network.URI
import Network.Wai.Middleware.Static
import System.IO
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty (ActionM, get, html, middleware, parseParamList, pathParam, scotty, text)

import Util (mkCantoNumeral)

htmlHeader :: H.Html
htmlHeader =
  let charSet = H.meta H.! A.charset "UTF-8"
      name_ = H.meta H.! A.name "viewport" H.! A.content "width=device-width"
      docTitle = H.title "NIAv2.5"
      css = H.link H.! A.rel "stylesheet" H.! A.href "https://cdn.jsdelivr.net/npm/water.css@2/out/dark.min.css"
      head_ = H.head $ mconcat [charSet, name_, docTitle, css]
   in H.html head_

getStaticData :: FilePath -> IO String
getStaticData = readFile'

getPoemTheses :: IO [String]
getPoemTheses = forM ["canto_I", "canto_II", "canto_IV"] $ do
  ( \n -> do
      let path_ = "resources/public/" ++ n ++ "/thesis.txt"
      getStaticData path_
    )

decodeParams :: String -> Either TL.Text [Int]
decodeParams s = parseParamList . TL.pack $ unEscapeString s

stdErrorHandler :: IOException -> IO String
stdErrorHandler e = do
  hPutStr stderr $ show e
  return ""

getCantoParens :: Int -> [Int] -> IO [String]
getCantoParens canto parens =
  let file = "resources/public/canto_" ++ mkCantoNumeral canto ++ "/parenthesis"
   in forM [file ++ "/par_" ++ show filename ++ ".txt" | filename <- parens :: [Int]] $ do
        ( \p -> do
            let parensTxt = getStaticData p
             in parensTxt `catch` stdErrorHandler
          )

getCantoFootnotes :: Int -> [Int] -> IO [String]
getCantoFootnotes canto footnote =
  forM ["footnote_" ++ show filename ++ ".txt" | filename <- footnote :: [Int]] $ do
    ( \f -> do
        let romanNumeral = mkCantoNumeral canto
            path_ = "resources/public/canto_" ++ romanNumeral ++ "/footnotes/" ++ f
         in getStaticData path_ `catch` stdErrorHandler
      )

mkScriptTag :: H.Html -> H.Html
mkScriptTag = H.script H.! A.type_ "text/plain"

homeTemplate :: H.Html
homeTemplate =
  let scriptTag = H.script H.! A.src "/js/client/main.js" $ pure ()
      root = H.div H.! A.id "root" $ pure ()
   in H.docTypeHtml $
        H.html $
          mconcat
            [ htmlHeader,
              H.body $
                mconcat
                  [ root,
                    scriptTag
                  ]
            ]
        
silentFailParse :: String -> [Int]
silentFailParse s = fromRight [] $ decodeParams s

standardTextResp :: [String] -> ActionM ()
standardTextResp s = text $ mconcat $ map TL.pack s

runScotty :: IO ()
runScotty = scotty 3000 $ do
  middleware $ staticPolicy (noDots >-> addBase "resources/public")
  get "/" $ do
    thesis <- liftIO getPoemTheses :: ActionM [String]
    let theses = map (mkScriptTag . H.toHtml) thesis
    html $ renderHtml $ mconcat [homeTemplate, mconcat theses]
  get "/footnotes/:canto/:footnote" $ do
    canto <- pathParam "canto" :: ActionM Int
    footnote <- pathParam "footnote" :: ActionM String
    let parsed = silentFailParse footnote
    footnotes <- liftIO $ getCantoFootnotes canto parsed :: ActionM [String]
    standardTextResp footnotes
  get "/parens/:canto/:parens" $ do
    canto <- pathParam "canto" :: ActionM Int
    parens <- pathParam "parens" :: ActionM String
    let parsed = silentFailParse parens
    parensParsed <- liftIO $ getCantoParens canto parsed :: ActionM [String]
    standardTextResp parensParsed
