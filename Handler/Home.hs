{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Data.Conduit
import Data.ByteString
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Base64 as Base64

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost uploadForm
    let submission  = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost uploadForm
    let handlerName = "postHomeR" :: Text
    submission <- (case result of
                      FormSuccess info -> do
                        bytes <- fileSource info $$ encodeSink
                        let encoded = decodeUtf8 bytes
                        return $ Just (info,encoded)
                      _ -> do 
                        return Nothing)

    defaultLayout $ do
        setTitle "Base64 Encoded Data-URI Generator"
        $(widgetFile "homepage")

uploadForm :: Form FileInfo
uploadForm = renderDivs $ fileAFormReq "Choose a file"

{-TODO: pretty sure this could just be awaitForever Base64.encode-}
encodeSink :: Sink ByteString Handler ByteString
encodeSink = do
  file <- await
  case file of
    Nothing -> return ""
    Just bytes -> return $ Base64.encode bytes
