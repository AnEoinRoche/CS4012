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
import System.Directory (listDirectory)

main :: IO ()
main = S.scotty 3000 $ do 
    S.get "/" $ do
      S.html . R.renderHtml $ do
        H.head $ 
          H.title "An Blog"  
        H.body $ do
          topBar
          H.h1 "My Blog"
          showLinks $ ["Pieces/Article1", "Pieces/Article2"]

    S.get "Pieces/:title" $ do
      --title <- param "title"
      --file $ "Pieces/" ++ title
      S.html . R.renderHtml $ do
        H.head $
          H.title "title"
        H.body $ do
          topBar
    

    S.get "/login" $ do
      S.html . R.renderHtml $ do
        H.head $ 
          H.title "New Post"
        H.body $ login
         
 
showLinks :: [H.AttributeValue] -> H.Markup  
showLinks [] = error "empty list"
showLinks (x:[]) = H.h2 $ H.a H.! href(x) $ "Post"
showLinks (x:xs) = do 
  H.h2 $ H.a H.! href(x) $ "Post"
  showLinks xs 
 
sortLinks :: [String] -> [String]
sortLinks [] = error "empty list"

topBar :: H.Html
topBar = H.div H.! class_ "container" $ do
           H.a H.! href("/") $ do
             H.button H.! type_ "button" $ "Home" 
           H.a H.! href("/newPost") $ do
             H.button H.! type_ "button" $ "New Post"  

login :: H.Html
login = do  
             H.h1 "Login"
             H.form H.! Text.Blaze.Html5.Attributes.id "pw" $ "Password (It's Wew)"
             H.input H.! type_ "text"
             H.button H.! onclick "checkPW()" $ "Click!"
             H.h2 H.! Text.Blaze.Html5.Attributes.id "out" $ ""
             H.script $ "function checkPW() { var x = documents.getElementById('pw'); var text = x.elements[0].value; document.getElementById('out').innerHTML = text; }" 







