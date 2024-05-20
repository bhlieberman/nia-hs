{-# LANGUAGE OverloadedStrings #-}

module Server (runScotty) where

import Control.Monad.IO.Class
import Network.Wai.Middleware.Static
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty (ActionM, get, html, middleware, pathParam, scotty)

nameTemplate :: H.Html -> H.Html
nameTemplate name = H.h1 $ "Hello" <> name

htmlHeader :: H.Html
htmlHeader = 
    let charSet = H.meta H.! A.charset "UTF-8"
        name_ = H.meta H.! A.name "viewport" H.! A.content "width=device-width"
        docTitle = H.title "NIAv2.5"
        head_ = H.head $ mconcat [charSet, name_, docTitle]
    in H.html head_

getStaticData :: IO String
getStaticData = readFile "resources/public/canto_I/thesis.txt"

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
    thesis <- liftIO getStaticData :: ActionM String
    html $ renderHtml $ mconcat [homeTemplate, sampleData $ H.toHtml thesis]
  get "/:name" $ do
    name <- pathParam "name" :: ActionM String
    html $ renderHtml $ nameTemplate $ H.toHtml name
