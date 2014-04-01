import Net.Network
import Net.Parser
import System.IO
import System.Environment
import Data.String.Utils
import Data.List
import Db.DbContext

main = do
  args <- getArgs
  startParsing args  

startParsing:: [String] ->  IO()
startParsing args  	
  | null args = putStrLn "Please enter url"
  | otherwise = parseUrl (head args) [head args] []

parseUrl:: String->[String] -> [String] ->  IO()
parseUrl _ [] parsedUrls = do
  putStrLn "Finished parsing"
parseUrl baseDomain (u:urls) parsedUrls  
  | (startswith "http" u) || ( startswith "//" u) = do   	  	  	  	      	
    html <- getBrowserHTML(u)
    title <- parseTitle html          	   
    metaKeywords <- parseMeta "keywords" html    
    metaDescriptions <- parseMeta "description" html    
    metaOgImage <- parseMeta "og:image" html    
    metaOgDescription <- parseMeta "og:description" html    
    
    conn <- connect "host=127.0.0.1 dbname=databayo user=postgres password=postgres"    
    
    existedPage <- getPage conn u        
    createOrUpdate existedPage conn Page { pageId = 0,
                        pageTitle = getFirstOrEmpty title,
                        pageKeywords = getFirstOrEmpty metaKeywords,
                        pageDescription = getFirstOrEmpty metaDescriptions,
                        pageOgImage = getFirstOrEmpty metaOgImage,
                        pageOgDescription = getFirstOrEmpty metaOgDescription,
                        url = u }

    links <- parseLinks html           
 	--putStrLn (show (filter (\f -> notIn (parsedUrls++[u]) f) (filter (startswith "http://ybigus.com") $ fixBaseLinks "http://ybigus.com" (nub $ merge links urls))))
    --mapM_ putStrLn 
    --	(filter (\f -> notIn (parsedUrls++[u]) f)
    --	((filter (startswith "http://ybigus.com") $ fixBaseLinks "http://ybigus.com" (nub $ merge links urls)))
    --	)
    parseUrl 
    	baseDomain
    	(filter (\f -> notIn (parsedUrls++[u]) f) 
    	(filter (startswith baseDomain) $ fixBaseLinks (baseDomain) (nub $ merge links urls)))
    	(nub (parsedUrls++[u]))
 
  | otherwise = do
    putStrLn "Unknown url"    

getFirstOrEmpty :: [String] -> String 
getFirstOrEmpty [] =  ""
getFirstOrEmpty (x:xs) =  take 255 x

merge :: [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

fixBaseLinks :: String -> [String] -> [String]
fixBaseLinks baseDomain [] = []
fixBaseLinks baseDomain (u:urls) 
	| (startswith "//" u) = u : fixBaseLinks baseDomain urls
	| (u == "/") = baseDomain : fixBaseLinks baseDomain urls 
	| (startswith "/" u) = (baseDomain ++ u) : fixBaseLinks baseDomain urls 
	| otherwise  = u : fixBaseLinks baseDomain urls

notIn :: [String] -> String -> Bool
notIn [] _ = True
notIn (x:xs) url
	| url == x = False
	| otherwise = notIn xs url

--handle <- openFile "parsed.html" WriteMode  
    --  html <- getHTML(head args)    
	    -- mapM_ prepareParsePageByLink links                        
    --todo: links join unique links and urls
    -- filter (startswith "http://ybigus.com") ["http://ybigus.com","xxx.com","http://ybigus.com"] 
    --parseUrl $ nub $ merge links urls
    --mapM_ putStrLn (filter (startswith "http://ybigus.com") $ fixBaseLinks "http://ybigus.com" (nub $ merge links urls))
    --mapM_ putStrLn  (parsedUrls++[u])
    --hPutStr handle html
    --hClose handle    