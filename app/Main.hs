
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import qualified Control.Exception as CE
import qualified Control.Concurrent.Async as CA
import qualified System.IO as SI
import qualified System.Directory as SD
import qualified Network.HTTP.Simple as NHS
import qualified Data.HashSet as HS
import qualified Network.HTTP.Base as NHB
import qualified Data.ByteString as BS
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Text.HTML.DOM as DOM
import qualified Text.XML as XML
import Text.XML.Cursor -- used as DSL


main :: IO ()
main = execParser opts >>= runWithOptions
  where
    opts = info (optionsParser <**> helper) ( fullDesc
      <> progDesc "Crawls open directories for tasty links")

-- https://github.com/pcapriotti/optparse-applicative
optionsParser :: Parser Options
optionsParser =
  Options <$> targetParser <*> profileParser <*> verbosityParser <*> directoryParser <*> parallelParser

targetParser :: Parser String
targetParser =
  argument str (metavar "TARGET"
  <> help "The target URL or the path to the file containing the target URLs")

profileParser :: Parser Profile
profileParser = option auto
  ( long "profile"
  <> short 'p'
  <> metavar "PROFILE"
  <> value NoProfile
  <> help "Profile for allowed extensions (Videos, Pictures, Music, Docs, SubTitles)" )

verbosityParser :: Parser Verbosity
verbosityParser = flag Normal Verbose
  ( long "verbose"
  <> short 'v'
  <> help "Enable verbose mode" )

directoryParser :: Parser(Maybe String)
directoryParser = optional (
  strOption
   ( long "directory"
    <> short 'd'
    <> metavar "DIRECTORY"
    <> help "The folder where to persist results - only new entries will be shown"))

parallelParser :: Parser Bool
parallelParser = switch
  ( long "parallel"
  <> help "Crawl target URLs in parallel" )

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
  let v = verbosity opts
  let df = persistentFolder opts
  validatePersistingFolder df
  if parallel opts then
    -- https://hackage.haskell.org/package/async-2.1.0/docs/Control-Concurrent-Async.html
    CA.mapConcurrently (businessTime desiredExtensions v df) urls
  else
    mapM (businessTime desiredExtensions v df) urls
  pure ()

validatePersistingFolder :: Maybe String -> IO ()
validatePersistingFolder Nothing = pure ()
validatePersistingFolder (Just df) =
  SD.doesDirectoryExist df >>= \exists ->
   if not exists
    then
      fail("directory " ++ df ++ " does not exist")
    else
       pure ()

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

businessTime :: AllowedExtensions -> Verbosity -> Maybe String -> String -> IO ()
businessTime ext v df url =
  case df of
    Nothing ->
      crawlUrl (Config ext v Nothing) url
    Just folderPath -> do
      let fileName = folderPath ++ "/" ++ fileNameForURL url ++ ".txt"
      createFileIfNotExist fileName
      existingContent <- loadPersistedResultsForURL fileName
      SI.withFile fileName SI.AppendMode (\handler ->
        let upc = URLPersistentConfig fileName handler existingContent
            config = Config ext v (Just upc)
        in crawlUrl config url)

createFileIfNotExist :: String -> IO ()
createFileIfNotExist filePath = do
  exists <- SD.doesFileExist filePath
  if exists
  then
    pure ()
  else
    SI.writeFile filePath ""

crawlUrl :: Config -> String -> IO ()
crawlUrl config url = do
  safeBody <- safeHttpCall url
  case safeBody of
    Left ex -> do
      putStrLn ex
      pure ()
    Right body -> do
      let doc = bodyToDoc body
      let extractedLinks = extractLinks doc
      verboseMode config doc extractedLinks url
      let urlTxt = T.pack url
      let links = map (createLink urlTxt) extractedLinks
      let resources = map createResource links
       --FIXME do not use mapM but an abstraction that allows partial failures
      mapM_ (handleResource config urlTxt) resources

fileNameForURL :: String -> String
fileNameForURL urlS =
  let url = T.pack urlS
      noTrailingSlash = T.dropWhileEnd (== '/')
      afterProtocol = last . T.splitOn "://"
      cleaned = T.replace "/" "-"
  in T.unpack $ (cleaned . afterProtocol . noTrailingSlash) url

-- https://hackage.haskell.org/package/hashmap-1.0.0.2/docs/Data-HashSet.html
-- A bloom filter should actually be enough to reduce memory footprint
loadPersistedResultsForURL :: String -> IO (HS.Set T.Text)
loadPersistedResultsForURL filePath = do
  fileContent <- TIO.readFile filePath
  let entries = fmap (last . T.splitOn " --> ") (T.lines fileContent)
  let populatedSet = foldr HS.insert HS.empty entries
  pure populatedSet

verboseMode :: Config -> XML.Document -> [T.Text] -> String -> IO ()
verboseMode config doc links url =
  case debug config of
    Verbose ->
      putStrLn("found " ++ show (length links) ++ " links for URL " ++ url ++ " in page " ++ show doc)
    _ ->
      pure ()

--FIXME detect cycles
handleResource :: Config -> T.Text -> Resource -> IO ()
handleResource config url r =
  case r of
    File l | linkMatchesConfig config l ->
      case urlPersistentConfig config of
        Just cfg ->
          if HS.member (fullLink l) (urlFilecontent cfg)
            then pure ()
            else do
              let pretty = prettyLink l
              TIO.putStrLn pretty
              TIO.hPutStrLn (fileHandle cfg) pretty
        Nothing ->
          TIO.putStrLn $ prettyLink l
    Folder l | shouldFollow l url ->
        crawlUrl config (T.unpack $ fullLink l)
    _ ->
        pure ()

linkMatchesConfig :: Config -> Link -> Bool
linkMatchesConfig config l =
  case extensions config of
    AllowAll -> True
    Only ext -> any (`T.isSuffixOf` fullLink l) ext

shouldFollow :: Link -> T.Text -> Bool
shouldFollow l url =
  let fullResourceUrl = fullLink l
      isChildren = T.isPrefixOf url fullResourceUrl
      isParentLink = T.isSuffixOf "../" fullResourceUrl
  in  isChildren && not isParentLink

safeHttpCall :: String -> IO (Either String BS.ByteString)
safeHttpCall url = do
  result <- CE.try (httpCall url) :: IO (Either CE.SomeException BS.ByteString)
  case result of
    Left ex  -> pure $ Left (show ex)
    Right val -> pure $ Right val

-- https://hackage.haskell.org/package/http-conduit-2.3.1/docs/Network-HTTP-Simple.html
httpCall :: String -> IO BS.ByteString
httpCall url = do
  req <- NHS.parseRequest url
  response <- NHS.httpBS req
  return $ NHS.getResponseBody response

-- https://hackage.haskell.org/package/html-conduit-1.3.0/docs/Text-HTML-DOM.html
bodyToDoc :: BS.ByteString -> XML.Document
bodyToDoc body =
  DOM.parseBSChunks [body]

--https://hackage.haskell.org/package/xml-conduit-1.8.0/docs/Text-XML-Cursor.html
extractLinks :: XML.Document -> [T.Text]
extractLinks doc =
  fromDocument doc
    $/ descendant
    &/ element "a"
    &.// attribute "href"

prettyLink :: Link -> T.Text
prettyLink l =
  let nameStr = T.unpack (name l)
      fullNameStr = T.unpack (fullLink l)
      pretty = NHB.urlDecode nameStr ++ " --> " ++ fullNameStr
  in T.pack pretty

createLink :: T.Text -> T.Text -> Link
createLink url display = Link display (T.concat [url, display])

createResource :: Link -> Resource
createResource linkResource =
  if T.last (fullLink linkResource) == '/' then -- not bullet proof
    Folder linkResource
  else
    File linkResource

data Profile = NoProfile | Videos | Music | Pictures | Docs | SubTitles deriving Read
data Verbosity = Normal | Verbose

data Options = Options { target :: String, profile :: Profile, verbosity :: Verbosity, persistentFolder :: Maybe String, parallel :: Bool}

data AllowedExtensions = AllowAll | Only { allowedExtensions :: [T.Text] }
data URLPersistentConfig = URLPersistentConfig { urlFilePath :: String, fileHandle :: SI.Handle, urlFilecontent :: HS.Set T.Text}
data Config = Config { extensions :: AllowedExtensions, debug :: Verbosity, urlPersistentConfig :: Maybe URLPersistentConfig }

data Link = Link { name :: T.Text, fullLink :: T.Text } deriving Show
data Resource = Folder { link :: Link } | File { link :: Link } deriving Show