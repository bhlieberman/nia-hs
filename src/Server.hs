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

nameTemplate :: H.Html -> H.Html
nameTemplate name = H.h1 $ "Hello" <> name

htmlHeader :: H.Html
htmlHeader =
  let charSet = H.meta H.! A.charset "UTF-8"
      name_ = H.meta H.! A.name "viewport" H.! A.content "width=device-width"
      docTitle = H.title "NIAv2.5"
      css = H.link H.! A.rel "stylesheet" H.! A.href "https://cdn.jsdelivr.net/npm/water.css@2/out/dark.min.css"
      head_ = H.head $ mconcat [charSet, name_, docTitle, css]
   in H.html head_

getStaticData :: FilePath -> IO String
getStaticData = readFile

getPoemTheses :: IO [String]
getPoemTheses = forM ["canto_I", "canto_II", "canto_IV"] $ do
  ( \n -> do
      let path_ = "resources/public/" ++ n ++ "/thesis.txt"
      getStaticData path_
    )

decodeParams :: String -> Either TL.Text [Int]
decodeParams s = parseParamList . TL.pack $ unEscapeString s

getCantoFootnotes :: Int -> [Int] -> IO [String]
getCantoFootnotes canto footnote =
  forM ["footnote_" ++ show filename ++ ".txt" | filename <- footnote :: [Int]] $ do
    ( \f -> do
        let canto_idx = ["I", "II", "IV"] :: [String]
            romanNumeral = canto_idx !! canto
            path_ = "resources/public/canto_" ++ romanNumeral ++ "/footnotes/" ++ f
         in getStaticData path_
              `catch` ( \e -> do
                          let err = show (e :: IOException)
                          hPutStr stderr err
                          return ""
                      )
      )

sampleData :: H.Html -> H.Html
sampleData = H.script H.! A.type_ "text/plain"

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

runScotty :: IO ()
runScotty = scotty 3000 $ do
  middleware $ staticPolicy (noDots >-> addBase "resources/public")
  get "/" $ do
    thesis <- liftIO getPoemTheses :: ActionM [String]
    let theses = map (sampleData . H.toHtml) thesis
    html $ renderHtml $ mconcat [homeTemplate, mconcat theses]
  get "/footnotes/:canto/:footnote" $ do
    canto <- pathParam "canto" :: ActionM Int
    footnote <- pathParam "footnote" :: ActionM String
    let parsed = fromRight [] $ decodeParams footnote
    footnotes <- liftIO $ getCantoFootnotes canto parsed :: ActionM [String]
    text $ mconcat $ map TL.pack footnotes
  get "/:name" $ do
    name <- pathParam "name" :: ActionM String
    html $ renderHtml $ nameTemplate $ H.toHtml name
