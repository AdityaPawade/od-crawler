{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Metrics
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Control.Exception as CE
import qualified System.Directory as SD
import qualified System.IO as SI
import qualified Data.HashSet as HS
import qualified Network.HTTP.Simple as NHS
import qualified Network.URI.Encode as EN
import qualified Data.ByteString as BS
import qualified Text.HTML.DOM as DOM
import qualified Text.XML as XML
import Text.XML.Cursor -- used as DSL

data Link = Link { name :: T.Text, fullLink :: T.Text } deriving (Show, Eq)
data Resource = Folder { link :: Link } | File { link :: Link } deriving (Show, Eq)

data Verbosity = Normal | Verbose

data AllowedExtensions = AllowAll | Only { allowedExtensions :: [T.Text] }
data URLPersistentConfig = URLPersistentConfig {
  urlFilePath :: String,
  fileHandle :: SI.Handle,
  urlFilecontent :: HS.HashSet T.Text
}

data Config = Config {
  extensions :: AllowedExtensions,
  debug :: Verbosity,
  urlPersistentConfig :: Maybe URLPersistentConfig,
  metrics :: Maybe Metrics
}

createLink :: T.Text -> T.Text -> Link
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
shouldFollow :: Link -> T.Text -> Bool
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
prettyLink :: Link -> T.Text
prettyLink l = T.concat [EN.decodeText (name l), " --> ", fullLink l]

extractLinksFromBody :: BS.ByteString -> [T.Text]
extractLinksFromBody = extractLinks . bodyToDoc

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

-- https://hackage.haskell.org/package/http-conduit-2.3.1/docs/Network-HTTP-Simple.html
httpCall :: String -> IO BS.ByteString
httpCall url = do
  req <- NHS.parseRequest url
  response <- NHS.httpBS req
  return $ NHS.getResponseBody response

safeHttpCall :: String -> IO (Either String BS.ByteString)
safeHttpCall url = do
  result <- CE.try (httpCall url) :: IO (Either CE.SomeException BS.ByteString)
  case result of
    Left ex  -> pure $ Left (show ex)
    Right val -> pure $ Right val

verboseMode :: Config -> XML.Document -> [T.Text] -> String -> IO ()
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