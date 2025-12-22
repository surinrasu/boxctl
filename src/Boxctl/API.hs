{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Boxctl.API
  ( ClashMode (..),
    ConfigResponse (..),
    Controller,
    DelayHistory (..),
    DelayResponse (..),
    GroupInfo (..),
    ProxyInfo (..),
    ProxyKind (..),
    ProxyScope (..),
    ResolvedInstance (..),
    VersionResponse (..),
    clashModeFromText,
    clashModeText,
    fetchConfigs,
    isGroupProxy,
    isSelectorProxy,
    isUrlTestProxy,
    fetchGroupDelay,
    fetchProxies,
    fetchProxyDelay,
    fetchVersion,
    mkController,
    proxyCurrent,
    proxyKindText,
    proxyMembers,
    selectProxyOption,
    switchMode,
  )
where

import Boxctl.Error (ApiError (..), TransportError (..))
import Control.Exception (try)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), throwE)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.Text.IO as TIO
import Data.Time (UTCTime)
import Network.HTTP.Client
  ( HttpException (..),
    HttpExceptionContent (..),
    Manager,
    RequestBody (..),
    Response,
    httpLbs,
    method,
    parseRequest,
    requestBody,
    requestHeaders,
    responseBody,
    responseStatus,
  )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (methodGet, methodPatch, methodPut)
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.URI (renderQuery, urlEncode)
import System.IO (stderr)

data ClashMode
  = ClashModeRule
  | ClashModeGlobal
  | ClashModeDirect
  | ClashModeScript
  | ClashModeUnknown Text
  deriving (Eq, Show)

data ProxyKind
  = ProxySelector
  | ProxyUrlTest
  | ProxyOther Text
  deriving (Eq, Show)

data ResolvedInstance = ResolvedInstance
  { resolvedBaseUrl :: Text,
    resolvedSecret :: Maybe Text
  }
  deriving (Eq, Show)

data Controller = Controller
  { controllerManager :: Manager,
    controllerInstance :: ResolvedInstance,
    controllerVerbose :: Bool
  }

data VersionResponse = VersionResponse
  { versionText :: Text,
    versionPremium :: Bool,
    versionMeta :: Bool
  }
  deriving (Eq, Show)

data ConfigResponse = ConfigResponse
  { configMode :: ClashMode,
    configModeList :: [ClashMode]
  }
  deriving (Eq, Show)

data DelayHistory = DelayHistory
  { historyTime :: UTCTime,
    historyDelay :: Int
  }
  deriving (Eq, Show)

data ProxyInfo = ProxyInfo
  { proxyKind :: ProxyKind,
    proxyName :: Text,
    proxyUdp :: Bool,
    proxyHistory :: [DelayHistory],
    proxyScope :: ProxyScope
  }
  deriving (Eq, Show)

data ProxyScope
  = ProxyLeaf
  | ProxyGroup GroupInfo
  deriving (Eq, Show)

data GroupInfo = GroupInfo
  { groupCurrent :: Maybe Text,
    groupMembers :: [Text]
  }
  deriving (Eq, Show)

newtype DelayResponse = DelayResponse
  { delayValue :: Int
  }
  deriving (Eq, Show)

newtype ProxyEnvelope = ProxyEnvelope
  { envelopeProxies :: Map Text ProxyInfo
  }

newtype ErrorResponse = ErrorResponse Text

newtype ModePatch = ModePatch ClashMode

newtype SelectRequest = SelectRequest Text

instance FromJSON ClashMode where
  parseJSON = withText "ClashMode" (pure . clashModeFromText)

instance ToJSON ClashMode where
  toJSON = String . clashModeText

instance FromJSON ProxyKind where
  parseJSON = withText "ProxyKind" (pure . proxyKindFromText)

instance ToJSON ProxyKind where
  toJSON = String . proxyKindText

instance FromJSON VersionResponse where
  parseJSON = withObject "VersionResponse" $ \obj ->
    VersionResponse
      <$> obj .: "version"
      <*> obj .:? "premium" .!= False
      <*> obj .:? "meta" .!= False

instance ToJSON VersionResponse where
  toJSON versionInfo =
    object
      [ "version" .= versionText versionInfo,
        "premium" .= versionPremium versionInfo,
        "meta" .= versionMeta versionInfo
      ]

instance FromJSON ConfigResponse where
  parseJSON = withObject "ConfigResponse" $ \obj ->
    ConfigResponse
      <$> obj .: "mode"
      <*> obj .:? "mode-list" .!= []

instance ToJSON ConfigResponse where
  toJSON config =
    object
      [ "mode" .= configMode config,
        "mode-list" .= configModeList config
      ]

instance FromJSON DelayHistory where
  parseJSON = withObject "DelayHistory" $ \obj ->
    DelayHistory
      <$> obj .: "time"
      <*> obj .: "delay"

instance ToJSON DelayHistory where
  toJSON delayHistory =
    object
      [ "time" .= historyTime delayHistory,
        "delay" .= historyDelay delayHistory
      ]

instance FromJSON ProxyInfo where
  parseJSON = withObject "ProxyInfo" $ \obj ->
    ProxyInfo
      <$> obj .: "type"
      <*> obj .: "name"
      <*> obj .:? "udp" .!= False
      <*> obj .:? "history" .!= []
      <*> parseProxyScope obj

instance ToJSON ProxyInfo where
  toJSON proxy =
    object (baseFields <> scopeFields (proxyScope proxy))
    where
      baseFields =
        [ "type" .= proxyKind proxy,
          "name" .= proxyName proxy,
          "udp" .= proxyUdp proxy,
          "history" .= proxyHistory proxy
        ]

      scopeFields ProxyLeaf = []
      scopeFields (ProxyGroup groupInfo) =
        [ "now" .= groupCurrent groupInfo,
          "all" .= groupMembers groupInfo
        ]

instance FromJSON DelayResponse where
  parseJSON = withObject "DelayResponse" $ \obj ->
    DelayResponse <$> obj .: "delay"

instance ToJSON DelayResponse where
  toJSON delayResponse =
    object
      [ "delay" .= delayValue delayResponse
      ]

instance FromJSON ProxyEnvelope where
  parseJSON = withObject "ProxyEnvelope" $ \obj ->
    ProxyEnvelope <$> obj .: "proxies"

instance FromJSON ErrorResponse where
  parseJSON = withObject "ErrorResponse" $ \obj ->
    ErrorResponse <$> obj .: "message"

instance ToJSON ModePatch where
  toJSON (ModePatch newMode) =
    object ["mode" .= newMode]

instance ToJSON SelectRequest where
  toJSON (SelectRequest name) =
    object ["name" .= name]

clashModeFromText :: Text -> ClashMode
clashModeFromText rawValue =
  case normalized of
    "rule" -> ClashModeRule
    "global" -> ClashModeGlobal
    "direct" -> ClashModeDirect
    "script" -> ClashModeScript
    other -> ClashModeUnknown other
  where
    normalized = T.toCaseFold (T.strip rawValue)

clashModeText :: ClashMode -> Text
clashModeText = \case
  ClashModeRule -> "rule"
  ClashModeGlobal -> "global"
  ClashModeDirect -> "direct"
  ClashModeScript -> "script"
  ClashModeUnknown other -> other

proxyKindFromText :: Text -> ProxyKind
proxyKindFromText rawValue =
  case normalized of
    "selector" -> ProxySelector
    "urltest" -> ProxyUrlTest
    "url-test" -> ProxyUrlTest
    other -> ProxyOther other
  where
    normalized = T.toCaseFold (T.strip rawValue)

proxyKindText :: ProxyKind -> Text
proxyKindText = \case
  ProxySelector -> "selector"
  ProxyUrlTest -> "url-test"
  ProxyOther other -> other

parseProxyScope :: Object -> Parser ProxyScope
parseProxyScope obj = do
  maybeMembers <- obj .:? "all"
  case maybeMembers of
    Nothing ->
      pure ProxyLeaf
    Just members ->
      ProxyGroup
        <$> ( GroupInfo
                <$> obj .:? "now"
                <*> pure members
            )

mkController :: ResolvedInstance -> Bool -> IO Controller
mkController controllerInstance controllerVerbose = do
  controllerManager <- newTlsManager
  pure $
    Controller
      { controllerManager = controllerManager,
        controllerInstance = controllerInstance,
        controllerVerbose = controllerVerbose
      }

fetchVersion :: Controller -> ExceptT ApiError IO VersionResponse
fetchVersion controller =
  requestJson controller methodGet ["version"] [] Nothing

fetchConfigs :: Controller -> ExceptT ApiError IO ConfigResponse
fetchConfigs controller =
  requestJson controller methodGet ["configs"] [] Nothing

switchMode :: Controller -> ClashMode -> ExceptT ApiError IO ()
switchMode controller newMode =
  requestNoContent controller methodPatch ["configs"] [] (Just (toJSON (ModePatch newMode)))

fetchProxies :: Controller -> ExceptT ApiError IO [ProxyInfo]
fetchProxies controller = do
  envelope <- requestJson controller methodGet ["proxies"] [] Nothing
  pure $
    filter (\proxy -> proxyName proxy /= "GLOBAL") $
      Map.elems $
        envelopeProxies envelope

isGroupProxy :: ProxyInfo -> Bool
isGroupProxy proxy =
  case proxyScope proxy of
    ProxyLeaf -> False
    ProxyGroup _ -> True

isSelectorProxy :: ProxyInfo -> Bool
isSelectorProxy proxy = proxyKind proxy == ProxySelector

isUrlTestProxy :: ProxyInfo -> Bool
isUrlTestProxy proxy = proxyKind proxy == ProxyUrlTest

proxyCurrent :: ProxyInfo -> Maybe Text
proxyCurrent proxy =
  case proxyScope proxy of
    ProxyLeaf -> Nothing
    ProxyGroup groupInfo -> groupCurrent groupInfo

proxyMembers :: ProxyInfo -> [Text]
proxyMembers proxy =
  case proxyScope proxy of
    ProxyLeaf -> []
    ProxyGroup groupInfo -> groupMembers groupInfo

fetchProxyDelay :: Controller -> Text -> Int -> ExceptT ApiError IO DelayResponse
fetchProxyDelay controller proxyName timeoutMs =
  requestJson controller methodGet ["proxies", proxyName, "delay"] (delayQuery timeoutMs) Nothing

fetchGroupDelay :: Controller -> Text -> Int -> ExceptT ApiError IO (Map Text Int)
fetchGroupDelay controller proxyName timeoutMs =
  requestJson controller methodGet ["group", proxyName, "delay"] (delayQuery timeoutMs) Nothing

selectProxyOption :: Controller -> Text -> Text -> ExceptT ApiError IO ()
selectProxyOption controller selectorName optionName =
  requestNoContent controller methodPut ["proxies", selectorName] [] (Just (toJSON (SelectRequest optionName)))

delayQuery :: Int -> [(BS.ByteString, Maybe BS.ByteString)]
delayQuery timeoutMs =
  [ ("url", Just (TE.encodeUtf8 "https://www.gstatic.com/generate_204")),
    ("timeout", Just (BS8.pack (show timeoutMs)))
  ]

requestJson ::
  FromJSON a =>
  Controller ->
  BS.ByteString ->
  [Text] ->
  [(BS.ByteString, Maybe BS.ByteString)] ->
  Maybe Value ->
  ExceptT ApiError IO a
requestJson controller requestMethod pathSegments queryParams requestBody = do
  httpResponse <- performRequest controller requestMethod pathSegments queryParams requestBody
  body <- exceptEither (responseBodyOrError httpResponse)
  case eitherDecode body of
    Left err ->
      throwE (ApiDecodeError (T.pack err))
    Right value ->
      pure value

requestNoContent ::
  Controller ->
  BS.ByteString ->
  [Text] ->
  [(BS.ByteString, Maybe BS.ByteString)] ->
  Maybe Value ->
  ExceptT ApiError IO ()
requestNoContent controller requestMethod pathSegments queryParams requestBody = do
  httpResponse <- performRequest controller requestMethod pathSegments queryParams requestBody
  void (exceptEither (responseBodyOrError httpResponse))

performRequest ::
  Controller ->
  BS.ByteString ->
  [Text] ->
  [(BS.ByteString, Maybe BS.ByteString)] ->
  Maybe Value ->
  ExceptT ApiError IO (Response BL.ByteString)
performRequest controller requestMethod pathSegments queryParams requestBody = do
  let manager = controllerManager controller
      ResolvedInstance {resolvedBaseUrl, resolvedSecret} = controllerInstance controller
      verbose = controllerVerbose controller
      url = buildUrl resolvedBaseUrl pathSegments queryParams
  liftIO $
    when verbose $
      TIO.hPutStrLn stderr ("[http] " <> decodeUtf8 requestMethod <> " " <> T.pack url)
  initialRequest <- ExceptT $
    first mapHttpException <$> try (parseRequest url)
  let authHeader =
        maybe [] (\secret -> [("Authorization", TE.encodeUtf8 ("Bearer " <> secret))]) resolvedSecret
      jsonRequestBody = maybe (RequestBodyBS BS.empty) (RequestBodyLBS . encode) requestBody
      extraHeaders =
        if maybe False (const True) requestBody
          then ("Content-Type", "application/json") : authHeader
          else authHeader
      request =
        initialRequest
          { method = requestMethod,
            requestHeaders = extraHeaders ++ requestHeaders initialRequest,
            requestBody = jsonRequestBody
          }
  response <- ExceptT $
    first mapHttpException <$> try (httpLbs request manager)
  liftIO $
    when verbose $
      TIO.hPutStrLn stderr ("[http] <- " <> T.pack (show (statusCode (responseStatus response))))
  pure response

responseBodyOrError :: Response BL.ByteString -> Either ApiError BL.ByteString
responseBodyOrError response
  | statusCode status >= 200 && statusCode status < 300 = Right (responseBody response)
  | otherwise = Left (ApiHttpError (statusCode status) (responseErrorMessage response))
  where
    status = responseStatus response

responseErrorMessage :: Response BL.ByteString -> Maybe Text
responseErrorMessage response =
  case eitherDecode (responseBody response) of
    Right (ErrorResponse message) ->
      Just message
    Left _ ->
      let body = decodeUtf8 (BL.toStrict (responseBody response))
       in if T.null body
            then Nothing
            else Just body

mapHttpException :: HttpException -> ApiError
mapHttpException = \case
  InvalidUrlException _ message ->
    ApiRequestBuildError (T.pack message)
  HttpExceptionRequest _ content ->
    ApiTransportError (mapTransportError content)

mapTransportError :: HttpExceptionContent -> TransportError
mapTransportError = \case
  ConnectionTimeout -> TransportTimeout
  ResponseTimeout -> TransportTimeout
  other -> TransportFailure (T.pack (show other))

buildUrl :: Text -> [Text] -> [(BS.ByteString, Maybe BS.ByteString)] -> String
buildUrl baseUrl pathSegments queryParams =
  T.unpack (T.dropWhileEnd (== '/') baseUrl)
    <> concatMap renderPathSegment pathSegments
    <> BS8.unpack (renderQuery True queryParams)

renderPathSegment :: Text -> String
renderPathSegment =
  ("/" <>)
    . BS8.unpack
    . urlEncode True
    . TE.encodeUtf8

decodeUtf8 :: BS.ByteString -> Text
decodeUtf8 = TE.decodeUtf8With lenientDecode

exceptEither :: Monad m => Either e a -> ExceptT e m a
exceptEither =
  ExceptT . pure
