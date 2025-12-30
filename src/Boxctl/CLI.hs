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
    SsmCommand (..),
    SsmListOptions (..),
    SsmOptions (..),
    SsmStatOptions (..),
    parseOptions,
  )
where

import Boxctl.Domain (KnownClashMode, knownClashModeFromText)
import Boxctl.Instance (InstanceTarget, guessInstanceTarget, instanceAddressTarget, instanceConfigTarget)
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative

data Options = Options
  { optInstanceTarget :: Maybe InstanceTarget,
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
  | CmdSwitch KnownClashMode
  | CmdList ListOptions
  | CmdShow ProxySelection
  | CmdTest ProxySelection
  | CmdSelect SelectCommand
  | CmdSsm SsmOptions
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

data SsmOptions = SsmOptions
  { ssmTargetTag :: Maybe Text,
    ssmTargetEndpoint :: Maybe Text,
    ssmSubcommand :: SsmCommand
  }
  deriving (Eq, Show)

data SsmCommand
  = SsmList SsmListOptions
  | SsmShow [Text]
  | SsmAdd Text (Maybe Text)
  | SsmRemove [Text]
  | SsmUpdate Text (Maybe Text)
  | SsmStat SsmStatOptions
  deriving (Eq, Show)

newtype SsmListOptions = SsmListOptions
  { ssmListShowPassword :: Bool
  }
  deriving (Eq, Show)

newtype SsmStatOptions = SsmStatOptions
  { ssmStatClear :: Bool
  }
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

instanceOption :: Parser InstanceTarget
instanceOption =
  guessedInstanceOption
    <|> configInstanceOption
    <|> addressInstanceOption

guessedInstanceOption :: Parser InstanceTarget
guessedInstanceOption =
  guessInstanceTarget
    <$> strOption
      ( long "instance"
          <> short 'i'
          <> metavar "<<PATH>|<ADDR[:<PORT>]>>"
          <> help "Target instance (config path or API address, defaults to 127.0.0.1:9090)"
      )

configInstanceOption :: Parser InstanceTarget
configInstanceOption =
  instanceConfigTarget
    <$> strOption
      ( long "instance-config"
          <> metavar "PATH"
          <> help "Target instance config file"
      )

addressInstanceOption :: Parser InstanceTarget
addressInstanceOption =
  instanceAddressTarget
    <$> strOption
      ( long "instance-address"
          <> metavar "ADDR[:<PORT>]"
          <> help "Target API address"
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
      <> command "ssm" (info ssmParser (progDesc "Manage Shadowsocks users via SSM API"))

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

ssmParser :: Parser Command
ssmParser =
  CmdSsm
    <$> ( SsmOptions
            <$> optional
              ( textOption
                  "tag"
                  't'
                  "NAME"
                  "SSM API service tag from config services[]"
              )
            <*> optional
              ( textOption
                  "endpoint"
                  'e'
                  "PATH"
                  "SSM API endpoint path, e.g. / or /edge"
              )
            <*> ssmCommandParser
        )

ssmCommandParser :: Parser SsmCommand
ssmCommandParser =
  hsubparser $
    command "list" (info ssmListParser (progDesc "List SSM users"))
      <> command "show" (info ssmShowParser (progDesc "Show detailed information for one or more users"))
      <> command "add" (info ssmAddParser (progDesc "Add a user"))
      <> command "remove" (info ssmRemoveParser (progDesc "Remove one or more users"))
      <> command "update" (info ssmUpdateParser (progDesc "Update a user's password"))
      <> command "stat" (info ssmStatParser (progDesc "Show SSM traffic statistics"))

ssmListParser :: Parser SsmCommand
ssmListParser =
  SsmList
    <$> ( SsmListOptions
            <$> switch
              ( long "show-password"
                  <> short 'p'
                  <> help "Show user pre-shared passwords in output"
              )
        )

ssmShowParser :: Parser SsmCommand
ssmShowParser =
  SsmShow
    <$> some
      ( textArgument "USER" "Username"
      )

ssmAddParser :: Parser SsmCommand
ssmAddParser =
  SsmAdd
    <$> textArgument "USER" "Username"
    <*> optional
      ( textOption
          "password"
          'p'
          "TEXT"
          "User pre-shared password; otherwise read silently from terminal"
      )

ssmRemoveParser :: Parser SsmCommand
ssmRemoveParser =
  SsmRemove
    <$> some
      ( textArgument "USER" "Username"
      )

ssmUpdateParser :: Parser SsmCommand
ssmUpdateParser =
  SsmUpdate
    <$> textArgument "USER" "Username"
    <*> optional
      ( textOption
          "password"
          'p'
          "TEXT"
          "User pre-shared password; otherwise read silently from terminal"
      )

ssmStatParser :: Parser SsmCommand
ssmStatParser =
  SsmStat
    <$> ( SsmStatOptions
            <$> switch
              ( long "clear"
                  <> help "Clear counters after reading statistics"
              )
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

clashModeReader :: ReadM KnownClashMode
clashModeReader =
  eitherReader $ \rawValue ->
    case knownClashModeFromText (T.pack rawValue) of
      Just mode -> Right mode
      Nothing -> Left "expected one of: rule, global, direct, script"

textArgument :: String -> String -> Parser Text
textArgument name description =
  T.pack
    <$> strArgument
      ( metavar name
          <> help description
      )

textOption :: String -> Char -> String -> String -> Parser Text
textOption longName shortName valueName description =
  T.pack
    <$> strOption
      ( long longName
          <> short shortName
          <> metavar valueName
          <> help description
      )
