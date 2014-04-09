module DataUri.DataUri
( fromPath
, fromPathAndFile
) where

import Prelude
import qualified Data.Text as T
import Text.Printf

import Network.Mime
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Base64 as Base64

type Base64  = T.Text
type DataUri = T.Text

encode64 :: B.ByteString -> Base64
encode64 = T.pack . BC.unpack . Base64.encode 

dataUri :: MimeType -> Base64 -> DataUri
dataUri mime s = 
  let m = filter (/='"') $ show mime 
  in T.pack $ printf "data:%s;base64,%s" m $ T.unpack s

fromPathAndFile :: T.Text -> B.ByteString -> DataUri
fromPathAndFile path file = dataUri mime encoded
  where mime    = defaultMimeLookup path
        encoded = encode64 file

fromPath :: String -> IO DataUri
fromPath path = do
  let mime = defaultMimeLookup $ T.pack path
  file <- B.readFile path
  let encoded = encode64 file
  let rawUri = dataUri mime encoded
  let uri = if isImg mime
            then toImgTag $ rawUri
            else rawUri
  return uri
  

toImgTag :: DataUri -> T.Text
toImgTag d = T.pack $ printf "<img src=\"%s\" />" $ T.unpack d

isImg :: MimeType -> Bool
isImg mime = 
  let mimeSuperType = head $ BC.split '/' mime
  in  mimeSuperType == BC.pack "image"
