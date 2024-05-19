{-# LANGUAGE OverloadedStrings #-}

module Server (runScotty) where

import Network.Wai.Middleware.Static
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Web.Scotty (ActionM, get, html, middleware, pathParam, scotty)

nameTemplate :: H.Html -> H.Html
nameTemplate name = H.h1 $ "Hello" <> name

homeTemplate :: H.Html
homeTemplate =
  let scriptTag = H.script H.! A.src "/js/client/main.js" $ pure ()
      root = H.div H.! A.id "root" $ pure ()
   in H.docTypeHtml $
        H.html $
          H.body $
            mconcat
              [ root,
                scriptTag
              ]

runScotty :: IO ()
runScotty = scotty 3000 $ do
  middleware $ staticPolicy (noDots >-> addBase "resources/public")
  get "/" $ do
    html $ renderHtml homeTemplate
  get "/:name" $ do
    name <- pathParam "name" :: ActionM String
    html $ renderHtml $ nameTemplate $ H.toHtml name
