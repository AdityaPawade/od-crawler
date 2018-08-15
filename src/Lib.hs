{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified Data.Text as T
import qualified Control.Exception as CE
import qualified Network.HTTP.Simple as NHS
import qualified Network.URI.Encode as EN
import qualified Data.ByteString as BS
import qualified Text.HTML.DOM as DOM
import qualified Text.XML as XML
import Text.XML.Cursor -- used as DSL

data Link = Link { name :: T.Text, fullLink :: T.Text } deriving (Show, Eq)
data Resource = Folder { link :: Link } | File { link :: Link } deriving (Show, Eq)

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