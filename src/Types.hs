module Types where

import System.IO (Handle)
import Data.Text (Text)
import Data.HashSet (HashSet)
import System.Metrics.Counter (Counter)
import System.Metrics.Gauge (Gauge)
import System.Metrics.Distribution (Distribution)

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

data Metrics = Metrics {
  httpLatency         :: !Distribution,
  openConnections     :: !Gauge,
  totalRequests       :: !Counter,
  inputUrlsProcessed  :: !Counter,
  inputUrlsInProgress :: !Gauge,
  folders             :: !Counter,
  files               :: !Counter,
  newFiles            :: !Counter,
  errors              :: !Counter
}

data Options = Options {
  target           :: !Text,
  profile          :: !Profile,
  verbosity        :: !Verbosity,
  persistentFolder :: !(Maybe String),
  parallel         :: !Bool,
  monitoring       :: !(Maybe Int)
}