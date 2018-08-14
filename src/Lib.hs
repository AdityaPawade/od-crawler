module Lib where

import qualified Data.Text as T

data Link = Link { name :: T.Text, fullLink :: T.Text } deriving (Show, Eq)
data Resource = Folder { link :: Link } | File { link :: Link } deriving (Show, Eq)

relativeParentLink = T.pack "../"
slashTxt = T.pack "/"

createLink :: T.Text -> T.Text -> Link
createLink url display
    -- relative link
    | display == relativeParentLink =
      let urlU = if T.last url == '/' then T.init url else url
          parentLink = T.dropWhileEnd (/='/') urlU
          lastSegment = T.takeWhileEnd (/='/')(T.init parentLink)
      in Link lastSegment parentLink
    -- abolute link
    | T.head display == '/' =
      let chuncks = T.splitOn display url
          location = T.concat[head chuncks, display, slashTxt]
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
    
