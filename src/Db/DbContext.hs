module Db.DbContext
( selectPages,
  connect,
  getPage,
  addPage,
  updatePage,
  createOrUpdate,
  Page(..)
) where

import Data.Maybe
import Database.HDBC
import Database.HDBC.PostgreSQL
import Data.ByteString.UTF8

data Page = Page { pageId :: Integer,
	                 pageTitle :: String,
	                 pageKeywords :: String,
	                 pageDescription :: String,
	                 pageOgImage :: String,
	                 pageOgDescription :: String,
	                 url :: String } deriving (Show)


connect:: String -> IO Connection
connect conString = do  
  conn <- connectPostgreSQL conString
  return conn

selectPages::IConnection a => a -> IO [Page]
selectPages con = do
  result <- quickQuery' con "select * from page" []
  return $ map unpack result
  where unpack [SqlInteger page_id, SqlByteString page_title, SqlByteString page_keywords, SqlByteString page_description, SqlByteString page_og_image, SqlByteString page_og_description, SqlByteString domain] =             
          Page { pageId = page_id,
                 pageTitle = toString page_title,
                 pageKeywords = toString page_keywords,
                 pageDescription = toString page_description,
                 pageOgImage = toString page_og_image,
                 pageOgDescription = toString page_description,
                 url = toString domain }

createOrUpdate::IConnection a => Maybe Page -> a -> Page -> IO Integer
createOrUpdate (Just existed) con p = withTransaction con (updatePage p)
createOrUpdate Nothing con p  = withTransaction con (addPage p)  

addPage::IConnection a => Page -> a -> IO Integer
addPage p con = do
  result <- run con "INSERT INTO page VALUES(nextval('page_ids'),?,?,?,?,?,?)" [SqlString (pageTitle p), SqlString (pageKeywords p), SqlString (pageDescription p), SqlString (pageOgImage p), SqlString (pageOgDescription p), SqlString (url p)]
  return result

updatePage::IConnection a => Page -> a -> IO Integer
updatePage p con = do
  result <- run con "UPDATE page SET page_title=?,page_keywords=?,page_description=?,page_og_image=?,page_og_description=?,domain=? where page_id=?" [SqlString (pageTitle p), SqlString (pageKeywords p), SqlString (pageDescription p), SqlString (pageOgImage p), SqlString (pageOgDescription p), SqlString (url p), SqlInteger (pageId p)]
  return result


getPage::IConnection a => a -> String -> IO (Maybe Page)
getPage con url = do
  result <- quickQuery' con "select * from page where domain=?" [SqlString url]    
  putStrLn(show result)
  return $ unpack result  
  where unpack [[SqlInteger page_id, SqlByteString page_title, SqlByteString page_keywords, SqlByteString page_description, SqlByteString page_og_image, SqlByteString page_og_description, SqlByteString domain]] =             
          Just Page { pageId = page_id,
                 pageTitle = toString page_title,
                 pageKeywords = toString page_keywords,
                 pageDescription = toString page_description,
                 pageOgImage = toString page_og_image,
                 pageOgDescription = toString page_description,
                 url = toString domain }
        unpack [] = Nothing        