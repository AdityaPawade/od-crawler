module Types where

import Metrics
import System.IO (Handle)
import Data.Text (Text)
import Data.HashSet (HashSet)

type Url = Text

data Link = Link {
  name     :: !Text,
  fullLink :: !Text
} deriving (Show, Eq)

data Resource = Folder { link :: !Link } | File { link :: !Link } deriving (Show, Eq)

data Profile = NoProfile | Videos | Music | Pictures | Docs | SubTitles deriving Read

data Verbosity = Normal | Verbose

data AllowedExtensions = AllowAll | Only { allowedExtensions :: ![Text] }

data URLPersistentConfig = URLPersistentConfig {
  urlFilePath    :: !String,
  fileHandle     :: !Handle,
  urlFilecontent :: !(HashSet Text)
}

data Config = Config {
  extensions          :: !AllowedExtensions,
  debug               :: !Verbosity,
  urlPersistentConfig :: !(Maybe URLPersistentConfig),
  metrics             :: !(Maybe Metrics)
}

data Options = Options {
  target           :: !Text,
  profile          :: !Profile,
  verbosity        :: !Verbosity,
  persistentFolder :: !(Maybe String),
  parallel         :: !Bool,
  monitoring       :: !(Maybe Int)
}