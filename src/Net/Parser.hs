module Net.Parser
( parseLinks,
  parseMeta,
  parseTitle
) where

import Text.XML.HXT.Core
import Text.HandsomeSoup

parseLinks:: String -> IO [String]-- [String]
parseLinks html = do
  let doc = readString [withParseHTML yes, withWarnings no] html
  let res = runX $ doc >>> css "a" >>> getAttrValue "href" 
  res >>= \r-> return r

parseTitle:: String  -> IO [String]-- [String]
parseTitle html = do
  let doc = readString [withParseHTML yes, withWarnings no] html
  let res = runX $ doc >>> css "title" >>> getChildren >>> getText 
  res >>= \r-> return r  

parseMeta:: String  -> String -> IO [String]-- [String]
parseMeta metaType html = do
  let doc = readString [withParseHTML yes, withWarnings no] html
  let res = runX $ doc >>> css "meta" >>> hasAttrValue "name" (== metaType) >>> getAttrValue "content"
  res >>= \r-> return r  