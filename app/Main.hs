
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Options.Applicative -- used as DSL
import Data.Semigroup ((<>))

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