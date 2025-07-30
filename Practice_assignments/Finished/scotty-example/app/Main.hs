{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Web.Scotty
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as R
import Data.Text.Internal.Lazy (Text)

main = scotty 3000 $ do
    get "/" $ do
        html "Hello World from Haskell! 28/07/2025"
    
    get "/greet" $ do
        html "Greetings World!"
    
    get "/greet/:name" $ do
        n <- param "name"
        html(response n)
    
    get "/long-hello/:name" $ do
        n <- param "name"
        html(longResponse n)

response :: Text -> Text
response n = do
            R.renderHtml $ do
                H.h1 ("Hello " >> H.toHtml n)

longResponse :: Text -> Text
longResponse n = do
                R.renderHtml $ do
                    H.head $ H.title "Long Hello"
                    H.body $ do
                        H.h1 "Welcome"
                        H.p ("Hello to " >> H.toHtml n)
