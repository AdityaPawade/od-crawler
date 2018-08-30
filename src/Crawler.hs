{-# LANGUAGE OverloadedStrings #-}

module Crawler where

import Metrics
import Storage
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Control.Concurrent.Async as CA
import qualified Control.Exception as CE
import qualified System.IO as SI
import System.IO (Handle)
import qualified Data.HashSet as HS
import Data.HashSet (HashSet)
import qualified Network.HTTP.Simple as NHS
import qualified Network.URI.Encode as EN
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Text.HTML.DOM as DOM
import qualified Text.XML as XML
import Text.XML.Cursor -- used as DSL

data Link = Link { name :: Text, fullLink :: Text } deriving (Show, Eq)
data Resource = Folder { link :: Link } | File { link :: Link } deriving (Show, Eq)

data Profile = NoProfile | Videos | Music | Pictures | Docs | SubTitles deriving Read

data Verbosity = Normal | Verbose

data AllowedExtensions = AllowAll | Only { allowedExtensions :: [Text] }
data URLPersistentConfig = URLPersistentConfig {
  urlFilePath :: String,
  fileHandle :: Handle,
  urlFilecontent :: HashSet Text
}

data Config = Config {
  extensions :: AllowedExtensions,
  debug :: Verbosity,
  urlPersistentConfig :: Maybe URLPersistentConfig,
  metrics :: Maybe Metrics
}

data Options = Options {
  target :: String,
  profile :: Profile,
  verbosity :: Verbosity,
  persistentFolder :: Maybe String,
  parallel :: Bool,
  monitoring :: Maybe Int
}

runWithOptions :: Options -> IO ()
runWithOptions opts = do
  urls <- urlsFromOption opts
  let desiredExtensions = profileExtensions $ profile opts
  let v = verbosity opts
  let df = persistentFolder opts
  metricsHandler <- handleMonitoring $ monitoring opts
  validatePersistingFolder df
  if parallel opts then
    -- https://hackage.haskell.org/package/async-2.1.0/docs/Control-Concurrent-Async.html
    -- FIXME limit number of concurrent run
    CA.mapConcurrently_ (processRootURL desiredExtensions v df metricsHandler) urls
  else
    mapM_ (processRootURL desiredExtensions v df metricsHandler) urls

urlsFromOption :: Options -> IO [String]
urlsFromOption opts =
  let targetStr = target opts
  in if T.isPrefixOf "http" (T.pack targetStr) then
      pure [targetStr]
    else
      readUrlsFromFile targetStr

profileExtensions :: Profile -> AllowedExtensions
profileExtensions Videos = Only ["mkv", "avi", "mp4"]
profileExtensions Pictures = Only ["jpeg", "png", "gif", "bmp"]
profileExtensions Music = Only ["mp3", "flac", "wave", "wav"]
profileExtensions Docs = Only ["pdf", "epub", "txt", "doc"]
profileExtensions SubTitles = Only ["srt", "sub"]
profileExtensions NoProfile = AllowAll

createLink :: Text -> Text -> Link
createLink url display
  -- relative link
  | display == "../" =
    let urlU = if T.last url == '/' then T.init url else url
        parentLink = T.dropWhileEnd (/='/') urlU
        lastSegment = T.takeWhileEnd (/='/')(T.init parentLink)
    in Link lastSegment parentLink
  -- absolute link
  | T.head display == '/' =
    let chunks = T.splitOn display url
        location = T.concat[head chunks, display, "/"]
    in  Link (T.takeWhileEnd (/='/') display) location
    -- full link
  | T.isPrefixOf "http" display =
    Link display display
  | otherwise = Link display (T.concat [url, display])

-- only follow deeper link to Folder into the current path
shouldFollow :: Link -> Text -> Bool
shouldFollow l url =
  let fullResourceUrl = fullLink l
      isChildren = T.isPrefixOf url fullResourceUrl
  in  fullResourceUrl /= url && isChildren

createResource :: Link -> Resource
createResource linkResource =
  if T.last (fullLink linkResource) == '/' then -- not bullet proof
    Folder linkResource
  else
    File linkResource

--https://hackage.haskell.org/package/uri-encode-1.5.0.5/docs/Network-URI-Encode.html
prettyLink :: Link -> Text
prettyLink l = T.concat [EN.decodeText (name l), " --> ", fullLink l]

extractLinksFromBody :: ByteString -> [Text]
extractLinksFromBody = extractLinks . bodyToDoc

-- https://hackage.haskell.org/package/html-conduit-1.3.0/docs/Text-HTML-DOM.html
bodyToDoc :: ByteString -> XML.Document
bodyToDoc body =
  DOM.parseBSChunks [body]

--https://hackage.haskell.org/package/xml-conduit-1.8.0/docs/Text-XML-Cursor.html
extractLinks :: XML.Document -> [Text]
extractLinks doc =
  fromDocument doc
    $/ descendant
    &/ element "a"
    &.// attribute "href"

-- https://hackage.haskell.org/package/http-conduit-2.3.1/docs/Network-HTTP-Simple.html
httpCall :: String -> IO ByteString
httpCall url = do
  req <- NHS.parseRequest url
  response <- NHS.httpBS req
  pure $ NHS.getResponseBody response

safeHttpCall :: String -> IO (Either String ByteString)
safeHttpCall url = do
  result <- CE.try (httpCall url) :: IO (Either CE.SomeException ByteString)
  case result of
    Left ex  -> pure $ Left (show ex)
    Right val -> pure $ Right val

verboseMode :: Config -> XML.Document -> [Text] -> String -> IO ()
verboseMode config doc links url =
  case debug config of
    Verbose ->
      putStrLn("found " ++ show (length links) ++ " links for URL " ++ url ++ " in page " ++ show doc)
    _ ->
      pure ()

linkMatchesConfig :: Config -> Link -> Bool
linkMatchesConfig config l =
  case extensions config of
    AllowAll -> True
    Only ext -> any (`T.isSuffixOf` fullLink l) ext

--FIXME detect cycles
handleResource :: Config -> Text -> Resource -> IO ()
handleResource config url r =
  case r of
    File l | linkMatchesConfig config l ->
      case urlPersistentConfig config of
        Just cfg ->
          if HS.member (fullLink l) (urlFilecontent cfg)
            then pure ()
            else do
              --FIXME update Set to avoid possible duplicate in the page
              let pretty = prettyLink l
              TIO.putStrLn pretty
              TIO.hPutStrLn (fileHandle cfg) pretty
              incrementCounter (metrics config) files
        Nothing ->
          TIO.putStrLn $ prettyLink l
    Folder l | shouldFollow l url -> do
        incrementCounter (metrics config) folders
        crawlUrl config (T.unpack $ fullLink l)
    _ ->
        pure ()

crawlUrl :: Config -> String -> IO ()
crawlUrl config url = do
  (safeBody, duration) <- timedMs $ safeHttpCall url
  case safeBody of
    Left ex -> do
      putStrLn ex
      incrementCounter (metrics config) errors
      pure ()
    Right body -> do
      addToDistribution (metrics config) httpLatency duration
      let doc = bodyToDoc body
      let extractedLinks = extractLinks doc
      verboseMode config doc extractedLinks url
      let urlTxt = T.pack url
      let links = map (createLink urlTxt) extractedLinks
      let resources = map createResource links
        -- FIXME handle resources in //
      mapM_ (handleResource config urlTxt) resources

processRootURL :: AllowedExtensions -> Verbosity -> Maybe String -> Maybe Metrics  -> String -> IO ()
processRootURL ext v df m url =
  case df of
    Nothing -> do
      crawlUrl (Config ext v Nothing m) url
      incrementCounter m inputUrlsProcessed
    Just folderPath -> do
      let fileName = folderPath ++ "/" ++ fileNameForURL url
      createFileIfNotExist fileName
      existingContent <- loadPersistedResultsForURL fileName
      SI.withFile fileName SI.AppendMode (\handler ->
        let upc = URLPersistentConfig fileName handler existingContent
            config = Config ext v (Just upc) m
        in do
          crawlUrl config url
          incrementCounter m inputUrlsProcessed)