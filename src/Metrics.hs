module Metrics where

import qualified System.Remote.Monitoring as M
import qualified System.Metrics.Counter as MC
import qualified System.Metrics.Gauge as MG
import qualified System.Metrics.Distribution as MD
import System.Metrics.Counter (Counter)
import System.Metrics.Gauge (Gauge)
import System.Metrics.Distribution (Distribution)

data Metrics =  Metrics {
  httpLatency :: Distribution,
  openConnections :: Gauge,
  totalRequests :: Counter,
  inputUrlsProcessed :: Counter,
  inputUrlsInProgress :: Gauge,
  folders :: Counter,
  files :: Counter,
  newFiles :: Counter,
  errors :: Counter
}

-- https://hackage.haskell.org/package/ekg
handleMonitoring :: Maybe Int -> IO (Maybe Metrics)
handleMonitoring Nothing =
  pure Nothing
handleMonitoring (Just p) = do
  handle <- M.forkServer "localhost" p
  inputUrlsProcessedCounter <- M.getCounter "crawler.input_urls.processed" handle
  inputUrlsInProgressGauge <- M.getGauge "crawler.input_urls.in_progress" handle
  foldersCounter <- M.getCounter "crawler.found.folders" handle
  filesCounter <- M.getCounter "crawler.found.files" handle
  newFilesCounter <- M.getCounter "crawler.found.new_files" handle
  errorsCounter <- M.getCounter "crawler.http.errors" handle
  httpLatencyDistribution <- M.getDistribution "crawler.http.latency_ms" handle
  connectionsGauge <- M.getGauge "crawler.http.open_connections" handle
  requestsCounter <- M.getCounter "crawler.http.total_requests" handle
  let m = Metrics httpLatencyDistribution connectionsGauge requestsCounter inputUrlsProcessedCounter inputUrlsInProgressGauge foldersCounter filesCounter newFilesCounter errorsCounter
  pure $ Just m

incCounter :: Maybe Metrics -> (Metrics -> Counter) -> IO ()
incCounter mm f =
  case mm of
    Nothing -> pure ()
    Just m -> MC.inc $ f m

incGauge :: Maybe Metrics -> (Metrics -> Gauge) -> IO ()
incGauge mm f =
  case mm of
    Nothing -> pure ()
    Just m -> MG.inc $ f m

decGauge :: Maybe Metrics -> (Metrics -> Gauge) -> IO ()
decGauge mm f =
  case mm of
    Nothing -> pure ()
    Just m -> MG.dec $ f m

recDistribution :: Maybe Metrics -> (Metrics -> Distribution) -> Double -> IO ()
recDistribution mm f d =
  case mm of
    Nothing -> pure ()
    Just m -> MD.add (f m) d