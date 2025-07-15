{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Boxctl.CLI
  ( ColorMode (..),
    Command (..),
    ListOptions (..),
    Options (..),
    OutputMode (..),
    ProxyFilter (..),
    ProxySelection (..),
    parseOptions,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative
import System.Exit (exitFailure)
import qualified System.IO as IO

data Options = Options
  { optInstance :: Maybe FilePath,
    optVerbose :: Bool,
    optOutputMode :: OutputMode,
    optColorMode :: ColorMode,
    optCommand :: Command
  }
  deriving (Eq, Show)

data OutputMode
  = OutputHuman
  | OutputJson
  deriving (Eq, Show)

data ColorMode
  = ColorAuto
  | ColorAlways
  | ColorNever
  deriving (Eq, Show)

data Command
  = CmdVersion
  | CmdMode
  | CmdSwitch Text
  | CmdList ListOptions
  | CmdShow ProxySelection
  | CmdTest ProxySelection
  | CmdSelect [Text]
  deriving (Eq, Show)

data ListOptions = ListOptions
  { listIncludeNodes :: Bool
  }
  deriving (Eq, Show)

data ProxySelection = ProxySelection
  { selectionFilter :: ProxyFilter,
    selectionNames :: [Text]
  }
  deriving (Eq, Show)

data ProxyFilter
  = FilterAll
  | FilterSelectors
  | FilterUrlTests
  | FilterConflict
  deriving (Eq, Show)

parseOptions :: IO Options
parseOptions = do
  options <-
    execParser $
      info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "Cli controller for sing-box"
            <> header "boxctl"
        )
  case validateOptions options of
    Left err -> do
      IO.hPutStrLn IO.stderr ("boxctl: " <> T.unpack err)
      exitFailure
    Right validOptions ->
      pure validOptions

optionsParser :: Parser Options
optionsParser =
  Options
    <$> optional instanceOption
    <*> switch
      ( long "verbose"
          <> short 'v'
          <> help "Verbose output"
      )
    <*> flag OutputHuman OutputJson
      ( long "json"
          <> help "Emit JSON output"
      )
    <*> option colorModeReader
      ( long "color"
          <> metavar "auto|always|never"
          <> value ColorAuto
          <> showDefaultWith (const "auto")
          <> help "Colorize human output"
      )
    <*> commandParser

instanceOption :: Parser String
instanceOption =
  strOption
    ( long "instance"
        <> short 'i'
        <> metavar "<<PATH>|<ADDR[:<PORT>]>>"
        <> help "Target instance (defaults to 127.0.0.1:9090)"
    )

commandParser :: Parser Command
commandParser =
  hsubparser $
    command "version" (info (pure CmdVersion) (progDesc "Show version"))
      <> command "mode" (info (pure CmdMode) (progDesc "Show current Clash mode"))
      <> command "switch" (info switchParser (progDesc "Switch Clash mode"))
      <> command "list" (info listParser (progDesc "List configured outbounds"))
      <> command "show" (info showParser (progDesc "Show details of outbounds"))
      <> command "test" (info testParser (progDesc "Test delays of outbounds"))
      <> command "select" (info selectParser (progDesc "Select an option for a selector"))

switchParser :: Parser Command
switchParser =
  CmdSwitch . T.pack
    <$> strArgument
      ( metavar "CLASH_MODE"
          <> help "Target Clash mode"
      )

listParser :: Parser Command
listParser =
  CmdList
    <$> ( ListOptions
            <$> switch
              ( long "all"
                  <> short 'a'
                  <> help "Include node outbounds in human output"
              )
        )

showParser :: Parser Command
showParser = CmdShow <$> proxySelectionParser

testParser :: Parser Command
testParser = CmdTest <$> proxySelectionParser

proxySelectionParser :: Parser ProxySelection
proxySelectionParser =
  ProxySelection
    <$> proxyFilterParser
    <*> many
      ( T.pack
          <$> strArgument
            ( metavar "[OUTBOUND1]..."
                <> help "Optional outbound names"
            )
      )

proxyFilterParser :: Parser ProxyFilter
proxyFilterParser =
  toFilter
    <$> switch
      ( long "selectors"
          <> short 's'
          <> help "Only show selectors"
      )
    <*> switch
      ( long "url-tests"
          <> short 'u'
          <> help "Only show url-tests"
      )
  where
    toFilter True False = FilterSelectors
    toFilter False True = FilterUrlTests
    toFilter False False = FilterAll
    toFilter True True = FilterConflict

selectParser :: Parser Command
selectParser =
  CmdSelect
    <$> some
      ( T.pack
          <$> strArgument
            ( metavar "[SELECTOR] <OPTION>"
                <> help "Either OPTION or SELECTOR OPTION"
            )
      )

validateOptions :: Options -> Either Text Options
validateOptions options = do
  validatedCommand <- validateCommand (optCommand options)
  Right (options {optCommand = validatedCommand})

validateCommand :: Command -> Either Text Command
validateCommand = \case
  CmdShow selection ->
    CmdShow <$> validateSelection selection
  CmdTest selection ->
    CmdTest <$> validateSelection selection
  CmdSelect args ->
    case args of
      [_] -> Right (CmdSelect args)
      [_, _] -> Right (CmdSelect args)
      _ -> Left "select expects one or two positional arguments"
  other ->
    Right other

validateSelection :: ProxySelection -> Either Text ProxySelection
validateSelection selection =
  case selectionFilter selection of
    FilterConflict ->
      Left "options --selectors and --url-tests are mutually exclusive"
    _ ->
      Right selection

colorModeReader :: ReadM ColorMode
colorModeReader =
  eitherReader $ \rawValue ->
    case T.toCaseFold (T.pack rawValue) of
      "auto" -> Right ColorAuto
      "always" -> Right ColorAlways
      "never" -> Right ColorNever
      _ -> Left "expected one of: auto, always, never"
