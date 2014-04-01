module Net.Network
( getHTML,
  getBrowserHTML
) where

import Network.HTTP
import Network.Browser
import Codec.Binary.UTF8.String

getHTML:: String -> IO String
getHTML url = do
  rsp <- simpleHTTP (getRequest url) 
  body <- getResponseBody rsp
  return (decodeString body)

getBrowserHTML:: String -> IO String
getBrowserHTML url = do
  (_, rsp) <- browse $ do
    setAllowRedirects True -- handle HTTP redirects
    request $ getRequest url
  return (decodeString (rspBody rsp))