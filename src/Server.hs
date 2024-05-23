{-# LANGUAGE OverloadedStrings #-}

module Server (runScotty) where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import qualified Data as D
import Data.Either
import Data.Maybe
import qualified Data.Text.Lazy as TL
import Network.URI
import Network.Wai.Middleware.Static
import Poem
import Data (byCanto')
import System.IO
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Util (mkCantoNumeral)
import Web.Scotty (ActionM, get, html, middleware, parseParamList, pathParam, queryParamMaybe, scotty, text)

htmlHeader :: H.Html
htmlHeader =
  let charSet = H.meta H.! A.charset "UTF-8"
      name_ = H.meta H.! A.name "viewport" H.! A.content "width=device-width"
      docTitle = H.title "NIAv2.5"
      waterCss = H.link H.! A.rel "stylesheet" H.! A.href "https://cdn.jsdelivr.net/npm/water.css@2/out/dark.min.css"
      addCss = H.link H.! A.rel "stylesheet" H.! A.href "/css/nav.css"
      head_ = H.head $ mconcat [charSet, name_, docTitle, waterCss, addCss]
   in H.html head_

navBar :: H.Html
navBar =
  H.html $
    H.nav H.! A.id "main-nav" $
      H.ul $
        mconcat
          [ H.li $ (H.a H.! A.href "/infinite") "Infinite",
            H.li $ (H.a H.! A.href "/canto/1") "Canto I",
            H.li $ (H.a H.! A.href "/canto/2") "Canto II",
            H.li $ (H.a H.! A.href "/canto/4") "Canto IV"
          ]

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
                  [ navBar,
                    root,
                    scriptTag
                  ]
            ]

wholeTemplate :: IO H.Html
wholeTemplate = do
  rendered <- liftIO renderWholePoem
  let bodyTag = H.body H.! A.id "main-content" $ mconcat [htmlHeader, navBar, rendered]
   in return (H.docTypeHtml . H.html $ bodyTag)

mainLayout :: H.Html -> H.Html
mainLayout html_ = do
  let body = H.body H.! A.id "main-content" $ mconcat [navBar, html_]
   in (H.docTypeHtml . H.html $ mconcat [htmlHeader, body])

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
  get "/infinite" $ do
    whole_ <- liftIO wholeTemplate
    html $ renderHtml whole_
  get "/canto/:id" $ do
    id_ <- pathParam "id" :: ActionM Int
    canto_ <- liftIO $ byCanto' id_ :: ActionM String
    html $ (renderHtml . mainLayout . H.toHtml . TL.pack) canto_
  get "/footnotes/:canto/:footnote" $ do
    canto <- pathParam "canto" :: ActionM Int
    footnote <- pathParam "footnote" :: ActionM String
    let parsed = silentFailParse footnote
    footnotes <- liftIO $ getCantoFootnotes canto parsed :: ActionM [String]
    standardTextResp footnotes
  -- have a route that returns text and one that returns HTML for each of
  -- parens and footnotes...
  get "/parens/:canto" $ do
    canto <- pathParam "canto" :: ActionM Int
    filt <- queryParamMaybe "parens" :: ActionM (Maybe String)
    filter_ <-
      case filt of
        Just vals -> liftIO $ return $ silentFailParse vals
        _ -> liftIO $ return [1 .. 5]
    let canto_ = D.byCanto canto >>= D.byParens' filter_
    resp <- liftIO $ TL.pack . fromJust <$> canto_ :: ActionM TL.Text
    text resp
