
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Metrics
import Options.Applicative -- used as DSL
import Data.Semigroup ((<>))
import qualified Control.Concurrent.Async as CA
import qualified System.Directory as SD
import qualified Data.Text as T

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

data Profile = NoProfile | Videos | Music | Pictures | Docs | SubTitles deriving Read

data Options = Options {
  target :: String,
  profile :: Profile,
  verbosity :: Verbosity,
  persistentFolder :: Maybe String,
  parallel :: Bool,
  monitoring :: Maybe Int
}