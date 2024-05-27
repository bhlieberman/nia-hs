{-# LANGUAGE OverloadedStrings #-}

module Templates where

import Control.Monad.IO.Class
import Poem
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Util

htmlHeader :: H.Html
htmlHeader =
  let charSet = H.meta H.! A.charset "UTF-8"
      name_ = H.meta H.! A.name "viewport" H.! A.content "width=device-width"
      docTitle = H.title "NIAv2.5"
      waterCss = H.link H.! A.rel "stylesheet" H.! A.href "https://cdn.jsdelivr.net/npm/water.css@2/out/dark.min.css"
      addCss = H.link H.! A.rel "stylesheet" H.! A.href "/assets/css/nav.css"
      fontAwesome = H.script H.! A.src "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.2/js/all.min.js" $ pure ()
      head_ = H.head $ mconcat [charSet, name_, docTitle, waterCss, addCss, fontAwesome]
   in H.html head_

navBar :: IO H.Html
navBar = do
  _svg_ <- renderSVG "resources/public/assets/numeral_one.svg"
  return $
    H.html $
      H.nav H.! A.id "main-nav" $
        H.ul $
          mconcat
            [ H.li $ (H.a H.! A.href "/infinite") (H.i H.! A.class_ "fa fa-solid fa-infinity" $ pure ()),
              H.li $ (H.a H.! A.href "/canto/1") "Canto I",
              H.li $ (H.a H.! A.href "/canto/2") "Canto II",
              H.li $ (H.a H.! A.href "/canto/4") "Canto IV"
            ]

homeTemplate :: IO H.Html
homeTemplate = do
  nav_ <- navBar
  let scriptTag = H.script H.! A.src "assets/js/client/main.js" $ pure ()
      root = H.div H.! A.id "root" $ pure ()
   in return $
        H.docTypeHtml $
          H.html $
            mconcat
              [ htmlHeader,
                H.body $
                  mconcat
                    [ nav_,
                      root,
                      scriptTag
                    ]
              ]

wholeTemplate :: IO H.Html
wholeTemplate = do
  rendered <- liftIO renderWholePoem
  nav_ <- navBar
  let bodyTag = H.body H.! A.id "main-content" $ mconcat [htmlHeader, nav_, rendered]
   in return (H.docTypeHtml . H.html $ bodyTag)

mainLayout :: H.Html -> IO H.Html
mainLayout html_ = do
  nav_ <- navBar
  let body = H.body H.! A.id "main-content" $ mconcat [nav_, html_]
   in return (H.docTypeHtml . H.html $ mconcat [htmlHeader, body, mkScriptTag $ pure ()])
