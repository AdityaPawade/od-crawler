module Crawler where

import Metrics
import Storage
import Util
import Types

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as TIO (putStr, putStrLn, hPutStr)
import qualified Control.Exception as CE
import qualified System.IO as SI
import qualified Data.HashSet as HS
import qualified Network.HTTP.Simple as NHS
import qualified Network.URI.Encode as EN
import Data.ByteString (ByteString)
import qualified Text.HTML.DOM as DOM
import qualified Text.XML as XML
import Text.XML.Cursor -- used as DSL

runWithOptions :: Options -> IO ()
runWithOptions opts = do
  let desiredExtensions = profileExtensions $ profile opts
  let v = verbosity opts
  let df = persistentFolder opts
  let par = if parallel opts then 10 else 1
  urls <- urlsFromOption opts
  validatePersistingFolder df
  metricsHandler <- maybe (pure Nothing) (fmap Just . handleMonitoring) (monitoring opts)
  mapAsyncUnordered par urls (processRootURL desiredExtensions v df metricsHandler)

urlsFromOption :: Options -> IO [Url]
urlsFromOption opts =
  if T.isPrefixOf "http" targetTxt then
    pure [targetTxt]
  else
    readUrlsFromFile $ T.unpack targetTxt
  where targetTxt = target opts

profileExtensions :: Profile -> AllowedExtensions
profileExtensions Videos = Only ["mkv", "avi", "mp4", "webm", "ogg"]
profileExtensions Pictures = Only ["jpeg", "png", "gif", "bmp"]
profileExtensions Music = Only ["mp3", "flac", "wave", "wav"]
profileExtensions Docs = Only ["pdf", "epub", "txt", "doc", "mobi"]
profileExtensions SubTitles = Only ["srt", "sub"]
profileExtensions NoProfile = AllowAll

createLink :: Text -> Text -> Link
createLink url display
  -- relative parent (only one parent up link supported)
  | display == "../" =
    let urlU = if T.last url == '/' then T.init url else url
        parentLink = T.dropWhileEnd (/='/') urlU
        lastSegment = T.takeWhileEnd (/='/')(T.init parentLink)
    in Link lastSegment parentLink
   -- absolute link
  | T.head display == '/' =
    if T.isInfixOf display url
      then -- parent link
        let chunks = T.splitOn display url
            displayWithTrailingSlash = if T.last display == '/' then display else T.concat [display, "/"]
            location = T.concat[head chunks, displayWithTrailingSlash]
        in  Link (T.takeWhileEnd (/='/') display) location
      else -- child link (folder or doc)
        let chunks = T.splitOn "/" url
            rootDomain = T.concat [head chunks, "//", chunks !!2]
            location = T.concat [rootDomain, display]
            lastSegment = T.takeWhileEnd (/='/')(if T.last display == '/' then T.init display else display)
        in  Link lastSegment location
  -- full http/https link
  | T.isPrefixOf "http" display =
    Link display display
  -- what is left to handle?
  | otherwise = Link display (T.concat [urlWithTrailingSlash, display])
    where urlWithTrailingSlash = if T.last url == '/' then url else T.concat [url, "/"]

-- only follow deeper link to Folder into the current path
shouldFollow :: Link -> Text -> Bool
shouldFollow l url =
  fullResourceUrl /= url && isChildren
  where
    fullResourceUrl = fullLink l
    isChildren = T.isPrefixOf url fullResourceUrl

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
    &/ laxElement "a"
    &.// laxAttribute "href"

-- https://hackage.haskell.org/package/http-conduit-2.3.1/docs/Network-HTTP-Simple.html
httpCall :: Url -> IO ByteString
httpCall url = do
  req <- NHS.parseRequest $ T.unpack url
  let gzipReq = NHS.setRequestHeader "Accept-Encoding" ["gzip"] req
  NHS.getResponseBody <$> NHS.httpBS gzipReq

safeHttpCall :: Url -> IO (Either CE.SomeException ByteString)
safeHttpCall url = CE.try (httpCall url)

managedHttpCall :: Url -> Maybe Metrics -> IO (Either CE.SomeException ByteString)
managedHttpCall url mm =
  incGauge mm openConnections >> timedMs (safeHttpCall url) >>= \(safeBody, duration) -> do
    decGauge mm openConnections
    incCounter mm totalRequests
    recDistribution mm httpLatency duration
    case safeBody of
      Left ex -> incCounter mm errors >> pure (Left ex)
      Right body -> pure (Right body)

verboseMode :: Config -> XML.Document -> [Text] -> Url -> IO ()
verboseMode config doc links url =
  case debug config of
    Verbose ->
      TIO.putStrLn message
      where
        message = T.concat ["found ", T.pack (show (length links)), " links for URL ", url, " in page ", T.pack (show doc)]
    _ ->
      pure ()

linkMatchesConfig :: Config -> Link -> Bool
linkMatchesConfig config l =
  case extensions config of
    AllowAll -> True
    Only ext -> any (`T.isSuffixOf` fullLink l) ext

--FIXME detect cycles
handleResource :: Config -> Int -> Text -> Resource -> IO ()
handleResource config depth url r =
  case r of
    File l | linkMatchesConfig config l ->
      -- inject a new line manually to not use putStrLn which is not atomic
      let pretty = T.concat [prettyLink l, "\n"]
      in case urlPersistentConfig config of
        Just cfg -> do
          incCounter (metrics config) files
          if HS.member (fullLink l) (urlFilecontent cfg)
            then pure()
            else do
              --FIXME update Set to avoid possible duplicate in the page
              TIO.putStr pretty
              TIO.hPutStr (fileHandle cfg) pretty
              incCounter (metrics config) newFiles
        Nothing ->
          incCounter (metrics config) files >> TIO.putStr pretty
    Folder l | shouldFollow l url ->
        incCounter (metrics config) folders >> crawlUrl config (depth + 1) (fullLink l)
    _ ->
        pure ()

crawlUrl :: Config -> Int -> Url -> IO ()
crawlUrl config depth url =
  managedHttpCall url (metrics config) >>= \case
    Left ex ->
      print ex
    Right body -> do
      let doc = bodyToDoc body
      let extractedLinks = extractLinks doc
      verboseMode config doc extractedLinks url
      let links = map (createLink url) extractedLinks
      let resources = map createResource links
      -- at the root level - we process links by batches of 3 to speed trees with many parent folders
      -- FIXME need manual workers to tackle deep thin trees efficiently?
      let par = if depth == 0 then 3 else 1
      mapAsyncUnordered par resources (handleResource config depth url)

crawlRootURL :: AllowedExtensions -> Verbosity -> Maybe String -> Maybe Metrics -> Url -> IO ()
crawlRootURL ext v df m url = case df of
  Nothing ->
    crawlUrl (Config ext v Nothing m) 0 url
  Just folderPath -> do
    let fileName = folderPath ++ "/" ++ fileNameForURL url
    createFileIfNotExist fileName
    existingContent <- loadPersistedResultsForURL fileName
    SI.withFile fileName SI.AppendMode (\handler ->
      let upc = URLPersistentConfig fileName handler existingContent
          config = Config ext v (Just upc) m
      in crawlUrl config 0 url)

processRootURL :: AllowedExtensions -> Verbosity -> Maybe String -> Maybe Metrics -> Url -> IO ()
processRootURL ext v df m url = do
  incGauge m inputUrlsInProgress
  crawlRootURL ext v df m url
  decGauge m inputUrlsInProgress
  incCounter m inputUrlsProcessed