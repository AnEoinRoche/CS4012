{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Web.Scotty as S
import qualified Text.Blaze.Html5 as H 
import Text.Blaze.Html5.Attributes  
import qualified Text.Blaze.Html.Renderer.Text as R
import Data.Text.Lazy         (Text, pack, unpack, append, fromStrict)
import Data.Monoid
import Control.Monad.IO.Class
import qualified Data.Text.Lazy.IO as T
import qualified System.IO as SI
import System.Directory (listDirectory)
import qualified Data.ByteString.Lazy as B
import System.FilePath ((</>))
import Network.Wai.Middleware.Static

main :: IO ()
main = S.scotty 3000 $ do 
    middleware $ staticPolicy (noDots >-> addBase "Pieces")

    S.get "/" $ do
      S.html . R.renderHtml $ do
        H.head $ 
          H.title "An Blog"  
        H.body $ do
          topBar
          H.h1 "My Blog"
          

    S.get "/Pieces/:title" $ do
      (title :: String) <- param "title"
      S.html . R.renderHtml $ do
        H.head $
          H.title "An Atricle"
        H.body $ do
          topBar
          H.div $ do
            H.object H.! data_ ("Pieces/Article1") $ ""

    
    S.get "/login" $ do
      S.html . R.renderHtml $ do
        H.head $ 
          H.title "Login"
        H.body $ login

    S.post "/login" $ do
      (pass :: String) <- param "password"
      if pass == "Wew"
        then do redirect "/newPost"
        else do redirect "/login"
         
    S.get "/newPost" $ do
      S.html . R.renderHtml $ do
        H.head $
          H.title "New Post"
        H.body $ newPost

    S.post "/newPost" $ do
      (title :: String) <- param "title"
      (content :: B.ByteString) <- param "content"
      liftIO $ sequence_ [B.writeFile ("Pieces/" </> title) content]
      redirect "/"  

 
sortLinks :: [String] -> [String]
sortLinks [] = error "empty list"

topBar :: H.Html
topBar = H.div H.! class_ "container" $ do
           H.a H.! href("/") $ do
             H.button H.! type_ "button" $ "Home" 
           H.a H.! href("/login") $ do
             H.button H.! type_ "button" $ "login"  

newPost :: H.Html
newPost = do topBar
             H.br
             H.h2 "Title"
             H.form H.! action "/newPost" H.! method "post" H.! Text.Blaze.Html5.Attributes.id "inform" $ do
               H.input H.! type_ "text" H.! name "title"
               H.h2 "Content"
               H.textarea H.! rows "10" H.! cols "50" H.! name "content" H.! form "inform" $ ""
               H.br
               H.input H.! type_ "submit" H.! name "newPost" H.! value "submit"

login :: H.Html
login = do  topBar
            H.h1 "Login"
            H.form H.! action "/login" H.! method "post" $ do
              H.input H.! type_ "password" H.! name "password" 
              H.input H.! type_ "submit" H.! name "login" H.! value "login" 
           




