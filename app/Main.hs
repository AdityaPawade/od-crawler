
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative -- used as DSL
import Data.Semigroup ((<>))
import qualified System.Clock as CS
import qualified System.Remote.Monitoring as M
import qualified System.Metrics.Distribution as MD
import qualified System.Remote.Counter as MC
import qualified Control.Exception as CE
import qualified Control.Concurrent.Async as CA
import qualified System.IO as SI
import qualified System.Directory as SD
import qualified Network.HTTP.Simple as NHS
import qualified Data.HashSet as HS
import qualified Network.URI.Encode as EN
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
      <> progDesc "Crawls open directories for interesting links")

-- https://github.com/pcapriotti/optparse-applicative
optionsParser :: Parser Options
optionsParser =
  Options <$> targetParser <*> profileParser <*> verbosityParser <*> directoryParser <*> parallelParser <*> monitoringParser

targetParser :: Parser String
targetParser =
  argument str (metavar "TARGET"
  <> help "The target URL or the path to the file containing the target URLs (one per line)")

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
  <> help "Enable verbose mode for debugging purpose" )

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

monitoringParser :: Parser(Maybe Int)
monitoringParser = optional (
  option auto
   ( long "monitoring"
    <> short 'm'
    <> metavar "MONITORING"
    <> help "The monitoring port where metrics are exposed - http://localhost:$port"))

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
  metricsHandler <- handleMonitoring $ monitoring opts
  validatePersistingFolder df
  if parallel opts then
    -- https://hackage.haskell.org/package/async-2.1.0/docs/Control-Concurrent-Async.html
    -- FIXME limit number of concurrent run
    CA.mapConcurrently_ (businessTime desiredExtensions v df metricsHandler) urls
  else
    mapM_ (businessTime desiredExtensions v df metricsHandler) urls

-- https://hackage.haskell.org/package/ekg
handleMonitoring :: Maybe Int -> IO (Maybe Metrics)
handleMonitoring Nothing =
  pure Nothing
handleMonitoring (Just p) = do
  handle <- M.forkServer "localhost" p
  inputUrlsProcessedCounter <- M.getCounter "crawler.input_urls_processed" handle
  foldersCounter <- M.getCounter "crawler.folders" handle
  fileCounter <- M.getCounter "crawler.files" handle
  errorsCounter <- M.getCounter "crawler.errors" handle
  httpLatencyDistribution <- M.getDistribution "crawler.http_latency_ms" handle
  pure $ Just $ Metrics httpLatencyDistribution inputUrlsProcessedCounter foldersCounter fileCounter errorsCounter

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

businessTime :: AllowedExtensions -> Verbosity -> Maybe String -> Maybe Metrics  -> String -> IO ()
businessTime ext v df m url =
  case df of
    Nothing -> do
      crawlUrl (Config ext v Nothing m) url
      incrementCounter m inputUrlsProcessed
    Just folderPath -> do
      let fileName = folderPath ++ "/" ++ fileNameForURL url ++ ".txt"
      createFileIfNotExist fileName
      existingContent <- loadPersistedResultsForURL fileName
      SI.withFile fileName SI.AppendMode (\handler ->
        let upc = URLPersistentConfig fileName handler existingContent
            config = Config ext v (Just upc) m
        in do
          crawlUrl config url
          incrementCounter m inputUrlsProcessed)

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

fileNameForURL :: String -> String
fileNameForURL urlS =
  let url = T.pack urlS
      noTrailingSlash = T.dropWhileEnd (== '/')
      afterProtocol = last . T.splitOn "://"
      cleaned = T.replace "/" "-"
  in T.unpack $ (cleaned . afterProtocol . noTrailingSlash) url

-- https://www.stackage.org/haddock/lts-11.17/unordered-containers-0.2.9.0/Data-HashSet.html
-- A bloom filter should actually be enough to reduce memory footprint
loadPersistedResultsForURL :: String -> IO (HS.HashSet T.Text)
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

timedMs :: IO a -> IO (a, Double)
timedMs m = do
    start <- getTimeNs
    a <- m
    end <- getTimeNs
    pure (a,  end / 1000000 - start / 1000000)

-- https://www.stackage.org/haddock/lts-12.0/clock-0.7.2/System-Clock.html
getTimeNs :: IO Double
getTimeNs = fmap (fromIntegral . CS.toNanoSecs) (CS.getTime CS.Monotonic)

incrementCounter :: Maybe Metrics -> (Metrics -> MC.Counter) -> IO ()
incrementCounter mm f =
  case mm of
    Nothing -> pure ()
    Just m ->  MC.inc $ f m

addToDistribution :: Maybe Metrics -> (Metrics -> MD.Distribution) -> Double -> IO ()
addToDistribution mm f d =
  case mm of
    Nothing -> pure ()
    Just m ->  MD.add (f m) d

linkMatchesConfig :: Config -> Link -> Bool
linkMatchesConfig config l =
  case extensions config of
    AllowAll -> True
    Only ext -> any (`T.isSuffixOf` fullLink l) ext

shouldFollow :: Link -> T.Text -> Bool
shouldFollow l url =
  let fullResourceUrl = fullLink l
      isChildren = T.isPrefixOf url fullResourceUrl
      isRelativeParentLink = T.isSuffixOf "../" fullResourceUrl
      -- 'createLink' does not handle absolute link differently
      absoluteLink = T.head (name l) == '/'
  in  not absoluteLink && isChildren && not isRelativeParentLink

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

--https://hackage.haskell.org/package/uri-encode-1.5.0.5/docs/Network-URI-Encode.html
prettyLink :: Link -> T.Text
prettyLink l = T.concat [EN.decodeText (name l), " --> ", fullLink l]

-- Does not make a differnce between relative and absolute links
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

data Options = Options {
  target :: String,
  profile :: Profile,
  verbosity :: Verbosity,
  persistentFolder :: Maybe String,
  parallel :: Bool,
  monitoring :: Maybe Int
}

data AllowedExtensions = AllowAll | Only { allowedExtensions :: [T.Text] }
data URLPersistentConfig = URLPersistentConfig {
  urlFilePath :: String,
  fileHandle :: SI.Handle,
  urlFilecontent :: HS.HashSet T.Text
}

data Metrics =  Metrics {
  httpLatency :: MD.Distribution,
  inputUrlsProcessed :: MC.Counter,
  folders :: MC.Counter,
  files :: MC.Counter,
  errors :: MC.Counter
}

data Config = Config {
  extensions :: AllowedExtensions,
  debug :: Verbosity,
  urlPersistentConfig :: Maybe URLPersistentConfig,
  metrics :: Maybe Metrics
}

data Link = Link { name :: T.Text, fullLink :: T.Text } deriving Show
data Resource = Folder { link :: Link } | File { link :: Link } deriving Show