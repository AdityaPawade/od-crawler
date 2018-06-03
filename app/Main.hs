
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Network.HTTP.Simple
import qualified Network.HTTP.Base as NHB
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Text.HTML.DOM as DOM
import qualified Text.XML as XML
import Text.XML.Cursor 
  

main :: IO ()
main = execParser opts >>= runWithOptions
  where
    opts = info (optionsParser <**> helper) ( fullDesc
      <> progDesc "Crawls open directories for tasty links")

optionsParser :: Parser Options
optionsParser =
  Options <$> urlParser <*> profileParser <*> verbosityParser

urlParser :: Parser String
urlParser =
  argument str (metavar "TARGET" <> help "The target URL or the path to the file containing the target URLs")

profileParser :: Parser Profile
profileParser = option auto
  ( long "profile"
  <> short 'p'
  <> metavar "PROFILE"
  <> value NoProfile
  <> help "Profile for allowed extensions (Videos, Pictures, Music, Docs, SubTitles)" )

data Verbosity = Normal | Verbose

verbosityParser :: Parser Verbosity
verbosityParser = flag Normal Verbose
  ( long "verbose"
  <> short 'v'
  <> help "Enable verbose mode" )

profileExtensions :: Profile -> AllowedExtensions
profileExtensions Videos = Only ["mkv", "avi", "mp4"]
profileExtensions Pictures = Only ["jpeg", "png", "gif", "bmp"]
profileExtensions Music = Only ["mp3", "flac", "wave", "wav"]
profileExtensions Docs = Only ["pdf", "epub", "txt", "doc"]
profileExtensions SubTitles = Only ["srt", "sub"]
profileExtensions NoProfile = AllowAll

runWithOptions :: Options -> IO ()
runWithOptions opts = do
  urls <- urlsFromOption opts
  let desiredExtensions = profileExtensions $ profile opts
  let config = Config desiredExtensions (verbosity opts)
  mapM_ (businessTime config) urls

urlsFromOption :: Options -> IO [String]
urlsFromOption opts =
  let targetStr = target opts
  in if T.isPrefixOf "http" (T.pack targetStr) then
      pure [targetStr]
    else
      readUrlsFromFile targetStr

readUrlsFromFile :: String -> IO [String]
readUrlsFromFile filePath =
  fmap lines (readFile filePath)

businessTime :: Config -> String -> IO ()
businessTime config url = do
  body <- httpCall url
  let doc = bodyToDoc body
  let extractedLinks = extractLinks doc
  verboseMode config doc extractedLinks url
  let urlTxt = T.pack url
  let links = map (createLink urlTxt) extractedLinks
  let resources = map createResource links
  mapM_ (handleResource config urlTxt) resources

verboseMode :: Config -> XML.Document -> [T.Text] -> String -> IO ()
verboseMode config doc links url =
  case debug config of
    Verbose ->
      putStrLn("found " ++ show (length links) ++ " links for URL " ++ url ++ " in page " ++ show doc)
    _ ->
      pure ()

handleResource :: Config -> T.Text -> Resource -> IO()
handleResource config url r = 
  case r of 
    File l | shouldPrint config l ->
        putStrLn $ prettyLink l
    Folder l | shouldFollow l url ->      
        businessTime config (T.unpack $ fullLink l)
    _ ->
        pure () 

shouldPrint :: Config -> Link -> Bool
shouldPrint config l = 
  case extensions config of
    AllowAll -> True
    Only ext -> any (`T.isSuffixOf` fullLink l) ext

shouldFollow :: Link -> T.Text -> Bool
shouldFollow l url = 
  let fullResourceUrl = fullLink l
      isChildren = T.isPrefixOf url fullResourceUrl
      isParentLink = T.isSuffixOf "../" fullResourceUrl
  in  isChildren && not isParentLink

-- https://hackage.haskell.org/package/http-conduit-2.3.1/docs/Network-HTTP-Simple.html
httpCall :: String -> IO BS.ByteString
httpCall url = do
  req <- parseRequest url
  response <- httpBS req
  return $ getResponseBody response

-- https://hackage.haskell.org/package/html-conduit-1.3.0/docs/Text-HTML-DOM.html
bodyToDoc :: BS.ByteString -> XML.Document
bodyToDoc body = 
  DOM.parseSTChunks [TE.decodeUtf8 body]

--https://hackage.haskell.org/package/xml-conduit-1.8.0/docs/Text-XML-Cursor.html
extractLinks :: XML.Document -> [T.Text]
extractLinks doc = 
  fromDocument doc
    $/ descendant
    &/ element "a"
    &.// attribute "href"    

prettyLink :: Link -> String
prettyLink l =
  let nameStr = T.unpack (name l)
      fullNameStr = T.unpack (fullLink l)
  in NHB.urlDecode nameStr ++ " --> " ++ fullNameStr

createLink :: T.Text -> T.Text -> Link
createLink url display = Link display (T.concat [url, display])

createResource :: Link -> Resource
createResource linkResource = 
  if T.last (fullLink linkResource) == '/' then -- not bullet proof
    Folder linkResource
  else
    File linkResource  
       
data Profile = NoProfile | Videos | Music | Pictures | Docs | SubTitles deriving Read
data Options = Options { target :: String, profile :: Profile, verbosity :: Verbosity }

data AllowedExtensions = AllowAll | Only { allowedExtensions :: [T.Text] }
data Config = Config { extensions :: AllowedExtensions, debug :: Verbosity }

data Link = Link { name :: T.Text, fullLink :: T.Text } deriving Show
data Resource = Folder { link :: Link } | File { link :: Link } deriving Show   