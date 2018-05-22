
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Network.HTTP.Simple
import Data.ByteString
import Data.Text.Encoding
import Data.Text
import Text.HTML.DOM
import Text.XML 
import Text.XML.Cursor 
  

main :: IO ()
main = execParser opts >>= runWithOptions
  where
    opts = info (optionsParser <**> helper) ( fullDesc
      <> progDesc "Crawls open directories for tasty links")

optionsParser :: Parser Options
optionsParser = Options
      <$> argument str (metavar "URL" <> help "The target URL")
      <*> profileParser

profileParser :: Parser Profile
profileParser = option auto
              ( long "profile"
              <> short 'p'
              <> metavar "PROFILE"
              <> value All
              <> help "Profile for allowed extensions" )  

profileExtensions :: Profile -> [Text] 
profileExtensions Videos = ["mkv", "avi", "mp4"]
profileExtensions Pictures = ["jpeg", "png"]
profileExtensions Music = ["mp3", "wave"]
profileExtensions Docs = ["pdf", "epub", "txt", "doc"]
profileExtensions All = []

runWithOptions :: Options -> IO ()
runWithOptions opts =
  let url = target opts
      desiredExtensions = profileExtensions $ profile opts
      config = Config desiredExtensions
  in  businessTime config url  

businessTime :: Config -> String -> IO ()
businessTime config url = do
  body <- httpCall url
  let doc = bodyToDoc body
  let extractedLinks = extractLinks doc
  let urlTxt = Data.Text.pack url
  let links = Prelude.map (createLink urlTxt) extractedLinks
  let resources = Prelude.map createResource links
  mapM_ (handleResource config urlTxt) resources

handleResource :: Config -> Text -> Resource -> IO()
handleResource config url r = 
  case r of 
    File l | shouldPrint config l ->
        Prelude.putStrLn $ prettyLink l
    Folder l | shouldFollow l url ->      
        businessTime config (Data.Text.unpack $ fullLink l)
    _ ->
        pure () 

shouldPrint :: Config -> Link -> Bool
shouldPrint config l = 
  let fullResourceUrl = fullLink l
      extensions = allowedExtensions config
  in  Prelude.null extensions || Prelude.any (`Data.Text.isSuffixOf` fullResourceUrl) extensions

shouldFollow :: Link -> Text -> Bool
shouldFollow l url = 
  let fullResourceUrl = fullLink l
      isChildren = Data.Text.isPrefixOf url fullResourceUrl
      isParentLink = Data.Text.isSuffixOf "../" fullResourceUrl
  in  isChildren && not isParentLink

-- https://hackage.haskell.org/package/http-conduit-2.3.1/docs/Network-HTTP-Simple.html
httpCall :: String -> IO ByteString
httpCall url = do
  req <- parseRequest url
  response <- httpBS req
  return $ getResponseBody response

-- https://hackage.haskell.org/package/html-conduit-1.3.0/docs/Text-HTML-DOM.html
bodyToDoc :: ByteString -> Document
bodyToDoc body = 
  parseSTChunks [decodeUtf8 body]

--https://hackage.haskell.org/package/xml-conduit-1.8.0/docs/Text-XML-Cursor.html
extractLinks :: Document -> [Text]
extractLinks doc = 
  fromDocument doc
    $/ child
    &/ element "a"
    &.// attribute "href"    

prettyLink :: Link -> String
prettyLink l = Data.Text.unpack (name l) ++ " --> " ++ Data.Text.unpack (fullLink l)

createLink :: Text -> Text -> Link
createLink url display = Link display (Data.Text.concat [url, display])

createResource :: Link -> Resource
createResource linkResource = 
  if Data.Text.last (fullLink linkResource) == '/' then -- not bullet proof
    Folder linkResource
  else
    File linkResource  
       
data Profile = All | Videos | Music | Pictures | Docs deriving Read    
data Options = Options { target :: String, profile :: Profile }    
newtype Config = Config { allowedExtensions :: [Text] }    

data Link = Link { name :: Text, fullLink :: Text } deriving Show
data Resource = Folder { link :: Link } | File { link :: Link } deriving Show   