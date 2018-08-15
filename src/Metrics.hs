{-# LANGUAGE OverloadedStrings #-}

module Metrics where

import qualified System.Clock as CS
import qualified System.Remote.Monitoring as M
import qualified System.Remote.Counter as MC
import qualified System.Metrics.Distribution as MD

data Metrics =  Metrics {
  httpLatency :: MD.Distribution,
  inputUrlsProcessed :: MC.Counter,
  folders :: MC.Counter,
  files :: MC.Counter,
  errors :: MC.Counter
}

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