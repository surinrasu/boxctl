{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Boxctl.Instance
  ( ControllerAddress (..),
    InstanceTarget (..),
    ResolvedInstance (..),
    ResolvedTailscaleConfig (..),
    TailscaleEndpointConfig (..),
    guessInstanceTarget,
    instanceAddressTarget,
    instanceConfigTarget,
    normalizeControllerAddress,
    resolveInstance,
    resolveSsmInstance,
    resolveTailscaleConfig,
    resolveTailscaleStateDirectory,
    stripJsonComments,
  )
where

import Boxctl.Error (InstanceError (..))
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..))
import Data.Aeson ((.:), (.:?), (.!=))
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.Char (isDigit, toLower)
import Data.List (isInfixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesDirectoryExist, doesFileExist, makeAbsolute)
import System.Environment (lookupEnv)
import System.FilePath ((</>), hasDrive, isAbsolute, isPathSeparator, takeExtension)
import Text.Read (readMaybe)

newtype ControllerAddress = ControllerAddress
  { unControllerAddress :: Text
  }
  deriving (Eq, Show)

data InstanceTarget
  = InstanceTargetGuess Text
  | InstanceTargetConfig FilePath
  | InstanceTargetAddress ControllerAddress
  deriving (Eq, Show)

data ResolvedInstance = ResolvedInstance
  { resolvedBaseUrl :: Text,
    resolvedSecret :: Maybe Text
  }
  deriving (Eq, Show)

data ResolvedTailscaleConfig = ResolvedTailscaleConfig
  { resolvedTailscaleConfigPath :: FilePath,
    resolvedTailscaleWorkdir :: Maybe FilePath,
    resolvedTailscaleEndpoints :: [TailscaleEndpointConfig]
  }
  deriving (Eq, Show)

data TailscaleEndpointConfig = TailscaleEndpointConfig
  { tailscaleEndpointLabel :: Text,
    tailscaleEndpointTag :: Maybe Text,
    tailscaleEndpointStateDirectoryRaw :: FilePath
  }
  deriving (Eq, Show)

defaultControllerAddress :: Text
defaultControllerAddress = "127.0.0.1:9090"

data ConfigFile = ConfigFile
  { configExperimental :: Maybe ExperimentalBlock,
    configServices :: [ServiceBlock],
    configEndpoints :: [EndpointBlock]
  }

newtype ExperimentalBlock = ExperimentalBlock
  { experimentalClashApi :: Maybe ClashAPIBlock
  }

data ClashAPIBlock = ClashAPIBlock
  { clashApiExternalController :: Maybe Text,
    clashApiSecret :: Maybe Text
  }

data ServiceBlock = ServiceBlock
  { serviceType :: Text,
    serviceTag :: Maybe Text,
    serviceListen :: Maybe Text,
    serviceListenPort :: Maybe Int,
    serviceServers :: Map Text Text,
    serviceTls :: Maybe ServiceTLSBlock
  }

newtype ServiceTLSBlock = ServiceTLSBlock
  { serviceTlsEnabled :: Maybe Bool
  }

data EndpointBlock = EndpointBlock
  { endpointType :: Text,
    endpointTag :: Maybe Text,
    endpointStateDirectory :: Maybe FilePath
  }

data NameMatch a
  = MissingName
  | UniqueName a
  | AmbiguousName [a]

instance Aeson.FromJSON ConfigFile where
  parseJSON = Aeson.withObject "ConfigFile" $ \obj ->
    ConfigFile
      <$> obj .:? "experimental"
      <*> (obj .:? "services" .!= [])
      <*> (obj .:? "endpoints" .!= [])

instance Aeson.FromJSON ExperimentalBlock where
  parseJSON = Aeson.withObject "ExperimentalBlock" $ \obj ->
    ExperimentalBlock <$> obj .:? "clash_api"

instance Aeson.FromJSON ClashAPIBlock where
  parseJSON = Aeson.withObject "ClashAPIBlock" $ \obj ->
    ClashAPIBlock
      <$> obj .:? "external_controller"
      <*> obj .:? "secret"

instance Aeson.FromJSON ServiceBlock where
  parseJSON = Aeson.withObject "ServiceBlock" $ \obj ->
    ServiceBlock
      <$> obj .: "type"
      <*> obj .:? "tag"
      <*> obj .:? "listen"
      <*> obj .:? "listen_port"
      <*> (obj .:? "servers" .!= Map.empty)
      <*> obj .:? "tls"

instance Aeson.FromJSON ServiceTLSBlock where
  parseJSON = Aeson.withObject "ServiceTLSBlock" $ \obj ->
    ServiceTLSBlock <$> obj .:? "enabled"

instance Aeson.FromJSON EndpointBlock where
  parseJSON = Aeson.withObject "EndpointBlock" $ \obj ->
    EndpointBlock
      <$> obj .: "type"
      <*> obj .:? "tag"
      <*> obj .:? "state_directory"

guessInstanceTarget :: String -> InstanceTarget
guessInstanceTarget =
  InstanceTargetGuess . T.pack

instanceConfigTarget :: FilePath -> InstanceTarget
instanceConfigTarget =
  InstanceTargetConfig

instanceAddressTarget :: String -> InstanceTarget
instanceAddressTarget =
  InstanceTargetAddress . ControllerAddress . T.pack

resolveInstance :: Maybe InstanceTarget -> ExceptT InstanceError IO ResolvedInstance
resolveInstance cliInstance = do
  (target, envSecret) <- resolveTargetSelection cliInstance
  resolved <- resolveTargetWith resolveClashFromConfig resolveFromAddress target
  pure resolved {resolvedSecret = envSecret <|> resolvedSecret resolved}

resolveSsmInstance :: Maybe InstanceTarget -> Maybe Text -> Maybe Text -> ExceptT InstanceError IO ResolvedInstance
resolveSsmInstance cliInstance maybeServiceTag maybeEndpoint = do
  (target, envSecret) <- resolveTargetSelection cliInstance
  resolved <-
    resolveTargetWith
      (resolveSsmFromConfig maybeServiceTag maybeEndpoint)
      (resolveSsmFromAddress maybeEndpoint)
      target
  pure resolved {resolvedSecret = envSecret <|> resolvedSecret resolved}

resolveTailscaleConfig :: Maybe InstanceTarget -> Maybe FilePath -> ExceptT InstanceError IO ResolvedTailscaleConfig
resolveTailscaleConfig cliInstance maybeWorkdir = do
  configPath <- resolveConfigTargetSelection cliInstance
  config <- readConfigFile configPath
  let tailscaleEndpoints = collectTailscaleEndpoints (configEndpoints config)
  if null tailscaleEndpoints
    then ExceptT (pure (Left MissingTailscaleEndpoints))
    else do
      absoluteWorkdir <- traverse (liftIO . makeAbsolute) maybeWorkdir
      pure
        ResolvedTailscaleConfig
          { resolvedTailscaleConfigPath = configPath,
            resolvedTailscaleWorkdir = absoluteWorkdir,
            resolvedTailscaleEndpoints = tailscaleEndpoints
          }

resolveTailscaleStateDirectory :: ResolvedTailscaleConfig -> TailscaleEndpointConfig -> ExceptT InstanceError IO FilePath
resolveTailscaleStateDirectory resolved endpoint = do
  expandedStateDirectory <-
    liftIO (expandEnvironmentVariables (tailscaleEndpointStateDirectoryRaw endpoint))
  let stateDirectory = expandedStateDirectory
  if isAbsolute stateDirectory
    then liftIO (makeAbsolute stateDirectory)
    else
      case resolvedTailscaleWorkdir resolved of
        Just workdir ->
          liftIO (makeAbsolute (workdir </> stateDirectory))
        Nothing ->
          ExceptT
            ( pure
                ( Left
                    ( TailscaleWorkdirRequired
                        (tailscaleEndpointLabel endpoint)
                        stateDirectory
                    )
                )
            )

resolveTargetSelection :: Maybe InstanceTarget -> ExceptT InstanceError IO (InstanceTarget, Maybe Text)
resolveTargetSelection cliInstance = do
  envInstance <- liftIO (fmap guessInstanceTarget <$> lookupEnv "BOXCTL_INSTANCE")
  envSecret <- liftIO (fmap T.pack <$> lookupEnv "BOXCTL_SECRET")
  let target =
        fromMaybe
          (InstanceTargetAddress (ControllerAddress defaultControllerAddress))
          (cliInstance <|> envInstance)
  pure (target, envSecret)

resolveConfigTargetSelection :: Maybe InstanceTarget -> ExceptT InstanceError IO FilePath
resolveConfigTargetSelection cliInstance = do
  envInstance <- liftIO (fmap guessInstanceTarget <$> lookupEnv "BOXCTL_INSTANCE")
  let target = fromMaybe (InstanceTargetConfig "config.json") (cliInstance <|> envInstance)
  resolveConfigFilePath target

resolveTargetWith ::
  (FilePath -> ExceptT InstanceError IO a) ->
  (ControllerAddress -> Either InstanceError a) ->
  InstanceTarget ->
  ExceptT InstanceError IO a
resolveTargetWith resolveConfig resolveAddress = \case
  InstanceTargetGuess rawTarget ->
    resolveGuessedTargetWith resolveConfig resolveAddress rawTarget
  InstanceTargetConfig configPath ->
    resolveConfigTargetWith resolveConfig configPath
  InstanceTargetAddress controllerAddress ->
    exceptEither (resolveAddress controllerAddress)

resolveConfigFilePath :: InstanceTarget -> ExceptT InstanceError IO FilePath
resolveConfigFilePath = \case
  InstanceTargetGuess rawTarget -> do
    let target = T.unpack (T.strip rawTarget)
    directoryExists <- liftIO (doesDirectoryExist target)
    if directoryExists
      then ExceptT (pure (Left InstancePathIsDirectory))
      else do
        fileExists <- liftIO (doesFileExist target)
        if fileExists
          then pure target
          else
            if looksLikeConfigPath target
              then ExceptT (pure (Left (InstanceConfigFileNotFound target)))
              else ExceptT (pure (Left InstanceConfigRequired))
  InstanceTargetConfig configPath ->
    resolveConfigTargetWith pure configPath
  InstanceTargetAddress _ ->
    ExceptT (pure (Left InstanceConfigRequired))

resolveConfigTargetWith ::
  (FilePath -> ExceptT InstanceError IO a) ->
  FilePath ->
  ExceptT InstanceError IO a
resolveConfigTargetWith resolveConfig configPath = do
  directoryExists <- liftIO (doesDirectoryExist configPath)
  if directoryExists
    then ExceptT (pure (Left InstancePathIsDirectory))
    else do
      fileExists <- liftIO (doesFileExist configPath)
      if fileExists
        then resolveConfig configPath
        else ExceptT (pure (Left (InstanceConfigFileNotFound configPath)))

resolveGuessedTargetWith ::
  (FilePath -> ExceptT InstanceError IO a) ->
  (ControllerAddress -> Either InstanceError a) ->
  Text ->
  ExceptT InstanceError IO a
resolveGuessedTargetWith resolveConfig resolveAddress rawTarget = do
  let target = T.unpack (T.strip rawTarget)
  directoryExists <- liftIO (doesDirectoryExist target)
  if directoryExists
    then ExceptT (pure (Left InstancePathIsDirectory))
    else do
      fileExists <- liftIO (doesFileExist target)
      if fileExists
        then resolveConfig target
        else
          exceptEither $
            if looksLikeConfigPath target
              then Left (InstanceConfigFileNotFound target)
              else resolveAddress (ControllerAddress (T.pack target))

looksLikeConfigPath :: FilePath -> Bool
looksLikeConfigPath target =
  not ("://" `isInfixOf` target)
    && ( hasDrive target
           || any isPathSeparator target
           || map toLower (takeExtension target) `elem` [".json", ".jsonc"]
       )

resolveClashFromConfig :: FilePath -> ExceptT InstanceError IO ResolvedInstance
resolveClashFromConfig configPath = do
  config <- readConfigFile configPath
  exceptEither $ do
    clashApi <- maybe (Left MissingClashApiConfig) Right (configExperimental config >>= experimentalClashApi)
    externalController <- maybe (Left MissingExternalController) Right (clashApiExternalController clashApi)
    normalized <- resolveFromAddress (ControllerAddress externalController)
    Right normalized {resolvedSecret = clashApiSecret clashApi}

resolveSsmFromConfig :: Maybe Text -> Maybe Text -> FilePath -> ExceptT InstanceError IO ResolvedInstance
resolveSsmFromConfig maybeServiceTag maybeEndpoint configPath = do
  config <- readConfigFile configPath
  exceptEither $ do
    service <- selectSsmService maybeServiceTag (filter isSsmApiService (configServices config))
    endpoint <- selectSsmEndpoint maybeEndpoint service
    buildSsmServiceInstance service endpoint

readConfigFile :: FilePath -> ExceptT InstanceError IO ConfigFile
readConfigFile configPath = do
  configBytes <- liftIO (BS.readFile configPath)
  let sanitized = stripJsonComments configBytes
  exceptEither $
    first (InstanceConfigParseError . T.pack) (Aeson.eitherDecodeStrict' sanitized)

collectTailscaleEndpoints :: [EndpointBlock] -> [TailscaleEndpointConfig]
collectTailscaleEndpoints endpoints =
  [ TailscaleEndpointConfig
      { tailscaleEndpointLabel = maybe fallbackLabel id (endpointTag endpoint),
        tailscaleEndpointTag = endpointTag endpoint,
        tailscaleEndpointStateDirectoryRaw = fromMaybe "tailscale" (endpointStateDirectory endpoint)
      }
  | (index, endpoint) <- zip [1 :: Int ..] endpoints,
    T.toCaseFold (T.strip (endpointType endpoint)) == "tailscale",
    let fallbackLabel = "#" <> T.pack (show index)
  ]

isSsmApiService :: ServiceBlock -> Bool
isSsmApiService service =
  T.toCaseFold (T.strip (serviceType service)) == "ssm-api"

selectSsmService :: Maybe Text -> [ServiceBlock] -> Either InstanceError ServiceBlock
selectSsmService maybeRequestedTag services =
  case maybeRequestedTag of
    Just requestedTag ->
      case findByMaybeName serviceTag services requestedTag of
        MissingName ->
          Left (SsmApiServiceTagNotFound requestedTag)
        UniqueName service ->
          Right service
        AmbiguousName matches ->
          Left (MultipleSsmApiServices (map ssmServiceLabel matches))
    Nothing ->
      case services of
        [] -> Left MissingSsmApiServices
        [service] -> Right service
        many -> Left (MultipleSsmApiServices (map ssmServiceLabel many))

selectSsmEndpoint :: Maybe Text -> ServiceBlock -> Either InstanceError Text
selectSsmEndpoint maybeRequested service =
  case maybeRequested of
    Just requestedEndpoint
      | Map.member requestedEndpoint endpoints ->
          Right requestedEndpoint
      | otherwise ->
          Left (SsmApiEndpointNotFound requestedEndpoint)
    Nothing
      | Map.null endpoints ->
          Left (MissingSsmApiServiceEndpoints serviceLabel)
      | Map.member "/" endpoints ->
          Right "/"
      | [onlyEndpoint] <- Map.keys endpoints ->
          Right onlyEndpoint
      | otherwise ->
          Left (MultipleSsmApiEndpoints serviceLabel (Map.keys endpoints))
  where
    endpoints = serviceServers service
    serviceLabel = ssmServiceLabel service

buildSsmServiceInstance :: ServiceBlock -> Text -> Either InstanceError ResolvedInstance
buildSsmServiceInstance service endpoint = do
  listenAddress <- maybe (Left (MissingSsmApiListen serviceLabel)) Right (serviceListen service)
  listenPortText <-
    case serviceListenPort service of
      Just port
        | port > 0 && port <= 65535 ->
            Right (T.pack (show port))
      _ ->
        Left (MissingSsmApiListenPort serviceLabel)
  baseUrl <-
    buildBaseUrlWithScheme
      (if serviceUsesTls service then "https" else "http")
      (normalizeHost False listenAddress)
      listenPortText
  Right
    ResolvedInstance
      { resolvedBaseUrl = appendSsmApiPath baseUrl (Just endpoint),
        resolvedSecret = Nothing
      }
  where
    serviceLabel = ssmServiceLabel service

serviceUsesTls :: ServiceBlock -> Bool
serviceUsesTls service =
  maybe False (fromMaybe False . serviceTlsEnabled) (serviceTls service)

ssmServiceLabel :: ServiceBlock -> Text
ssmServiceLabel service =
  fromMaybe "<untagged>" (serviceTag service)

resolveFromAddress :: ControllerAddress -> Either InstanceError ResolvedInstance
resolveFromAddress (ControllerAddress rawAddress) = do
  baseUrl <- normalizeControllerAddress rawAddress
  Right ResolvedInstance {resolvedBaseUrl = baseUrl, resolvedSecret = Nothing}

resolveSsmFromAddress :: Maybe Text -> ControllerAddress -> Either InstanceError ResolvedInstance
resolveSsmFromAddress maybeEndpoint controllerAddress = do
  resolved <- resolveFromAddress controllerAddress
  Right
    resolved
      { resolvedBaseUrl = appendSsmApiPath (resolvedBaseUrl resolved) maybeEndpoint
      }

appendSsmApiPath :: Text -> Maybe Text -> Text
appendSsmApiPath rawBaseUrl maybeEndpoint
  | Just endpoint <- maybeEndpoint =
      baseUrl <> normalizeEndpointPrefix endpoint <> "/server/v1"
  | T.isSuffixOf "/server/v1" baseUrl =
      baseUrl
  | otherwise =
      baseUrl <> "/server/v1"
  where
    baseUrl = T.dropWhileEnd (== '/') rawBaseUrl

normalizeEndpointPrefix :: Text -> Text
normalizeEndpointPrefix rawEndpoint =
  case T.dropWhileEnd (== '/') (T.strip rawEndpoint) of
    "" -> ""
    "/" -> ""
    trimmed
      | T.isPrefixOf "/" trimmed -> trimmed
      | otherwise -> "/" <> trimmed

normalizeControllerAddress :: Text -> Either InstanceError Text
normalizeControllerAddress rawValue
  | T.null trimmed = Left (InvalidInstanceAddress "instance address is empty")
  | "://" `T.isInfixOf` trimmed = Right (T.dropWhileEnd (== '/') trimmed)
  | T.isPrefixOf "[" trimmed = normalizeBracketedAddress trimmed
  | otherwise = normalizeSimpleAddress trimmed
  where
    trimmed = T.strip rawValue

normalizeSimpleAddress :: Text -> Either InstanceError Text
normalizeSimpleAddress rawAddress =
  case T.splitOn ":" rawAddress of
    [host] ->
      buildBaseUrlWithScheme "http" (normalizeHost False host) "9090"
    ["", port] ->
      buildBaseUrlWithScheme "http" "127.0.0.1" port
    [host, port] ->
      buildBaseUrlWithScheme "http" (normalizeHost False host) port
    _ ->
      Left (InvalidInstanceAddress "invalid instance address; bracket IPv6 literals like [::1]:9090")

normalizeBracketedAddress :: Text -> Either InstanceError Text
normalizeBracketedAddress rawAddress =
  let (rawHost, rest) = T.breakOn "]" (T.drop 1 rawAddress)
   in if T.null rest
        then Left (InvalidInstanceAddress "invalid bracketed instance address")
        else
          let portPart = T.drop 1 rest
              host = normalizeHost True rawHost
           in case portPart of
                "" -> buildBaseUrlWithScheme "http" host "9090"
                p ->
                  case T.stripPrefix ":" p of
                    Just port -> buildBaseUrlWithScheme "http" host port
                    Nothing -> Left (InvalidInstanceAddress "invalid bracketed instance address")

buildBaseUrlWithScheme :: Text -> Text -> Text -> Either InstanceError Text
buildBaseUrlWithScheme scheme host port
  | T.null host = Left (InvalidInstanceAddress "instance host is empty")
  | not (validPort port) = Left (InvalidInstanceAddress "instance port must be in the range 1-65535")
  | needsBrackets host = Right (scheme <> "://[" <> host <> "]:" <> port)
  | otherwise = Right (scheme <> "://" <> host <> ":" <> port)

validPort :: Text -> Bool
validPort port
  | T.null port = False
  | T.all isDigit port =
      case readMaybe (T.unpack port) of
        Just value -> value >= (1 :: Int) && value <= 65535
        Nothing -> False
  | otherwise = False

normalizeHost :: Bool -> Text -> Text
normalizeHost bracketed rawHost =
  case T.strip (stripHostBrackets rawHost) of
    "" -> "127.0.0.1"
    "*" -> loopback
    "0.0.0.0" -> "127.0.0.1"
    "::" -> "::1"
    host -> host
  where
    loopback
      | bracketed = "::1"
      | otherwise = "127.0.0.1"

stripHostBrackets :: Text -> Text
stripHostBrackets host =
  if T.length trimmed >= 2 && T.head trimmed == '[' && T.last trimmed == ']'
    then T.init (T.tail trimmed)
    else trimmed
  where
    trimmed = T.strip host

needsBrackets :: Text -> Bool
needsBrackets host = T.count ":" host > 1

findByMaybeName :: (a -> Maybe Text) -> [a] -> Text -> NameMatch a
findByMaybeName getName values requestedName =
  case filter (nameMatchesExactly requestedName) values of
    value : [] ->
      UniqueName value
    [] ->
      case filter (nameMatchesFold requestedName) values of
        [] -> MissingName
        [value] -> UniqueName value
        matches -> AmbiguousName matches
    matches ->
      AmbiguousName matches
  where
    nameMatchesExactly requested =
      maybe False (== requested) . getName
    nameMatchesFold requested =
      maybe False ((== T.toCaseFold requested) . T.toCaseFold) . getName

expandEnvironmentVariables :: FilePath -> IO FilePath
expandEnvironmentVariables = go
  where
    go [] = pure []
    go ('$' : '{' : rest) =
      let (nameChars, suffix) = break (== '}') rest
       in case suffix of
            '}' : remaining
              | validEnvName nameChars -> do
                  value <- lookupEnv nameChars
                  ((fromMaybe "") value <>) <$> go remaining
            _ -> ('$' :) <$> go ('{' : rest)
    go ('$' : rest@(nameStart : _))
      | validEnvInitial nameStart =
          let (nameChars, remaining) = span validEnvChar rest
           in do
                value <- lookupEnv nameChars
                ((fromMaybe "") value <>) <$> go remaining
    go (char : rest) =
      (char :) <$> go rest

    validEnvName [] = False
    validEnvName (char : chars) = validEnvInitial char && all validEnvChar chars
    validEnvInitial char =
      char == '_' || ('A' <= char && char <= 'Z') || ('a' <= char && char <= 'z')
    validEnvChar char =
      validEnvInitial char || ('0' <= char && char <= '9')

stripJsonComments :: BS.ByteString -> BS.ByteString
stripJsonComments = BS.pack . goNormal . BS.unpack
  where
    goNormal [] = []
    goNormal (34 : rest) = 34 : goString False rest
    goNormal (47 : 47 : rest) = goLineComment rest
    goNormal (47 : 42 : rest) = goBlockComment rest
    goNormal (char : rest) = char : goNormal rest

    goString _ [] = []
    goString escaped (char : rest)
      | escaped = char : goString False rest
      | char == 92 = char : goString True rest
      | char == 34 = char : goNormal rest
      | otherwise = char : goString False rest

    goLineComment [] = []
    goLineComment (char : rest)
      | char == 10 = 10 : goNormal rest
      | char == 13 = 13 : goNormal rest
      | otherwise = goLineComment rest

    goBlockComment [] = []
    goBlockComment (42 : 47 : rest) = goNormal rest
    goBlockComment (char : rest)
      | char == 10 = 10 : goBlockComment rest
      | char == 13 = 13 : goBlockComment rest
      | otherwise = goBlockComment rest

exceptEither :: Monad m => Either e a -> ExceptT e m a
exceptEither =
  ExceptT . pure
