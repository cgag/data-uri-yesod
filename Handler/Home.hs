{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Data.Text as T
import Data.ByteString
import qualified Data.ByteString.Char8 as BC
import Data.Conduit
import Data.Conduit.Binary
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Base64 as Base64
import System.Directory
import System.IO (hClose)
import System.IO.Temp

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
    let submission  = Nothing :: Maybe DataURIs
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
                          (tmpPath, tmpHandle) <- liftIO $ openTempFile "." "encodeTmp"
                          liftIO $ hClose tmpHandle 
                          runResourceT $ fileSource info $$ sinkFile tmpPath
                          bytes <- liftIO $ BC.readFile tmpPath
                          liftIO $ removeFile tmpPath
                          let mime = fileContentType info
                          let encoded = decodeUtf8 $ Base64.encode bytes
                          return $ Just $ buildDataURIs mime encoded
                      _ -> do 
                          return Nothing)

    defaultLayout $ do
        setTitle "Base64 Encoded Data-URI Generator"
        $(widgetFile "homepage")

uploadForm :: Form FileInfo
uploadForm = renderDivs $ fileAFormReq "Or manually select a file:"

{-TODO: pretty sure this could just be awaitForever Base64.encode-}
encodeConduit :: (Monad m) => Conduit ByteString m ByteString
encodeConduit = do
  file <- await
  case file of
    Nothing -> return ()
    Just bytes -> do
      yield $ Base64.encode bytes
      encodeConduit

data DataURIs = DataURIs {
    imageTag :: Maybe Text
  , raw :: Text
}

buildDataURIs :: Text -> Text -> DataURIs
buildDataURIs mime encoded = DataURIs {
    raw  = encoded
  , imageTag = if isImg mime
               then Just $ dataImgTag mime encoded 
               else Just "not an image :("
}

dataImgTag :: Text -> Text -> Text
dataImgTag mime encoded = "<img src=\"data:" <> mime <> ";base64," <> encoded <> "\" />"

isImg :: Text -> Bool
isImg mime = case T.split (== '/') mime of
                (t:_) -> "image" == t
                _ -> False
