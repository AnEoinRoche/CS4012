{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty as S
import qualified Text.Blaze.Html5 as H 
import Text.Blaze.Html5.Attributes  
import qualified Text.Blaze.Html.Renderer.Text as R
import Data.Text.Lazy         (Text, pack, unpack, append, fromStrict)
import Data.Monoid
import qualified Data.Text.IO as T
import qualified System.IO as SI

main :: IO ()
main = S.scotty 3000 $ do 
    S.get "/" $ do
      S.html . R.renderHtml $ do
        H.head $ 
          H.title "An Blog"  
        H.body $ do
          H.h1 "AN BLog"          
          H.h2 $ H.a H.! href ("/post1") $ "Post 1"

    S.get "/post1" $ file "text"
         


    
