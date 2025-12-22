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
    SelectCommand (..),
    parseOptions,
  )
where

import Boxctl.API (ClashMode, clashModeFromText)
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative

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
  | CmdSwitch ClashMode
  | CmdList ListOptions
  | CmdShow ProxySelection
  | CmdTest ProxySelection
  | CmdSelect SelectCommand
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
  deriving (Eq, Show)

data SelectCommand
  = SelectByOption Text
  | SelectBySelector Text Text
  deriving (Eq, Show)

parseOptions :: IO Options
parseOptions =
  execParser $
    info
      (optionsParser <**> helper)
      ( fullDesc
          <> progDesc "Cli controller for sing-box"
          <> header "boxctl"
      )

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
  CmdSwitch
    <$> argument clashModeReader
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
  flag' FilterSelectors
    ( long "selectors"
        <> short 's'
        <> help "Only show selectors"
    )
    <|> flag' FilterUrlTests
      ( long "url-tests"
          <> short 'u'
          <> help "Only show url-tests"
      )
    <|> pure FilterAll

selectParser :: Parser Command
selectParser =
  CmdSelect
    <$> ( buildSelectCommand
            <$> textArgument "SELECTOR_OR_OPTION" "Selector outbound name, or target option when used alone"
            <*> optional (textArgument "OPTION" "Target option name")
        )

buildSelectCommand :: Text -> Maybe Text -> SelectCommand
buildSelectCommand selectorOrOption maybeOption =
  case maybeOption of
    Nothing -> SelectByOption selectorOrOption
    Just optionName -> SelectBySelector selectorOrOption optionName

colorModeReader :: ReadM ColorMode
colorModeReader =
  eitherReader $ \rawValue ->
    case T.toCaseFold (T.pack rawValue) of
      "auto" -> Right ColorAuto
      "always" -> Right ColorAlways
      "never" -> Right ColorNever
      _ -> Left "expected one of: auto, always, never"

clashModeReader :: ReadM ClashMode
clashModeReader =
  eitherReader (Right . clashModeFromText . T.pack)

textArgument :: String -> String -> Parser Text
textArgument name description =
  T.pack
    <$> strArgument
      ( metavar name
          <> help description
      )
