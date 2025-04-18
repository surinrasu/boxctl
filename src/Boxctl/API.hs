{-# LANGUAGE OverloadedStrings #-}

module Boxctl.API
  ( ConfigResponse (..),
    Controller,
    DelayHistory (..),
    DelayResponse (..),
    ProxyInfo (..),
    ResolvedInstance (..),
    VersionResponse (..),
    fetchConfigs,
    fetchGroupDelay,
    fetchProxies,
    fetchProxyDelay,
    fetchVersion,
    mkController,
    selectProxyOption,
    switchMode,
  )
where

import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.Aeson
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
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (methodGet, methodPatch, methodPut)
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.URI (renderQuery, urlEncode)
import System.IO (stderr)

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
  { configMode :: Text,
    configModeList :: [Text]
  }
  deriving (Eq, Show)

data DelayHistory = DelayHistory
  { historyTime :: UTCTime,
    historyDelay :: Int
  }
  deriving (Eq, Show)

data ProxyInfo = ProxyInfo
  { proxyType :: Text,
    proxyName :: Text,
    proxyUdp :: Bool,
    proxyHistory :: [DelayHistory],
    proxyNow :: Maybe Text,
    proxyAll :: Maybe [Text]
  }
  deriving (Eq, Show)

newtype DelayResponse = DelayResponse
  { delayValue :: Int
  }
  deriving (Eq, Show)

data ProxyEnvelope = ProxyEnvelope
  { envelopeProxies :: Map Text ProxyInfo
  }

data ErrorResponse = ErrorResponse
  { errorMessage :: Text
  }

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
      <*> obj .:? "now"
      <*> obj .:? "all"

instance ToJSON ProxyInfo where
  toJSON proxy =
    object
      [ "type" .= proxyType proxy,
        "name" .= proxyName proxy,
        "udp" .= proxyUdp proxy,
        "history" .= proxyHistory proxy,
        "now" .= proxyNow proxy,
        "all" .= proxyAll proxy
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

data ModePatch = ModePatch Text

data SelectRequest = SelectRequest Text

mkController :: ResolvedInstance -> Bool -> IO Controller
mkController controllerInstance controllerVerbose = do
  controllerManager <- newTlsManager
  pure $
    Controller
      { controllerManager = controllerManager,
        controllerInstance = controllerInstance,
        controllerVerbose = controllerVerbose
      }

fetchVersion :: Controller -> IO (Either Text VersionResponse)
fetchVersion controller =
  requestJson controller methodGet ["version"] [] Nothing

fetchConfigs :: Controller -> IO (Either Text ConfigResponse)
fetchConfigs controller =
  requestJson controller methodGet ["configs"] [] Nothing

switchMode :: Controller -> Text -> IO (Either Text ())
switchMode controller newMode =
  requestNoContent controller methodPatch ["configs"] [] (Just (toJSON (ModePatch newMode)))

fetchProxies :: Controller -> IO (Either Text [ProxyInfo])
fetchProxies controller = do
  result <- requestJson controller methodGet ["proxies"] [] Nothing
  pure $ fmap extractProxies result
  where
    extractProxies :: ProxyEnvelope -> [ProxyInfo]
    extractProxies =
      filter (\proxy -> proxyName proxy /= "GLOBAL")
        . Map.elems
        . envelopeProxies

fetchProxyDelay :: Controller -> Text -> Int -> IO (Either Text DelayResponse)
fetchProxyDelay controller proxyName timeoutMs =
  requestJson controller methodGet ["proxies", proxyName, "delay"] (delayQuery timeoutMs) Nothing

fetchGroupDelay :: Controller -> Text -> Int -> IO (Either Text (Map Text Int))
fetchGroupDelay controller proxyName timeoutMs =
  requestJson controller methodGet ["group", proxyName, "delay"] (delayQuery timeoutMs) Nothing

selectProxyOption :: Controller -> Text -> Text -> IO (Either Text ())
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
  IO (Either Text a)
requestJson controller requestMethod pathSegments queryParams requestBody = do
  response <- performRequest controller requestMethod pathSegments queryParams requestBody
  pure $ do
    httpResponse <- response
    body <- responseBodyOrError httpResponse
    case eitherDecode body of
      Left err ->
        Left ("failed to decode API response: " <> T.pack err)
      Right value ->
        Right value

requestNoContent ::
  Controller ->
  BS.ByteString ->
  [Text] ->
  [(BS.ByteString, Maybe BS.ByteString)] ->
  Maybe Value ->
  IO (Either Text ())
requestNoContent controller requestMethod pathSegments queryParams requestBody = do
  response <- performRequest controller requestMethod pathSegments queryParams requestBody
  pure $ do
    httpResponse <- response
    _ <- responseBodyOrError httpResponse
    Right ()

performRequest ::
  Controller ->
  BS.ByteString ->
  [Text] ->
  [(BS.ByteString, Maybe BS.ByteString)] ->
  Maybe Value ->
  IO (Either Text (Response BL.ByteString))
performRequest controller requestMethod pathSegments queryParams requestBody = do
  let manager = controllerManager controller
      ResolvedInstance {resolvedBaseUrl, resolvedSecret} = controllerInstance controller
      verbose = controllerVerbose controller
      url = buildUrl resolvedBaseUrl pathSegments queryParams
  when verbose $
    TIO.hPutStrLn stderr ("[http] " <> decodeUtf8 requestMethod <> " " <> T.pack url)
  requestResult <- try (parseRequest url) :: IO (Either SomeException Request)
  case requestResult of
    Left err ->
      pure (Left ("failed to build request: " <> T.pack (show err)))
    Right initialRequest -> do
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
      responseResult <- try (httpLbs request manager) :: IO (Either SomeException (Response BL.ByteString))
      case responseResult of
        Left err ->
          pure (Left ("request failed: " <> T.pack (show err)))
        Right response -> do
          when verbose $
            TIO.hPutStrLn stderr ("[http] <- " <> T.pack (show (statusCode (responseStatus response))))
          pure (Right response)

responseBodyOrError :: Response BL.ByteString -> Either Text BL.ByteString
responseBodyOrError response
  | statusCode status >= 200 && statusCode status < 300 = Right (responseBody response)
  | otherwise = Left (renderError response)
  where
    status = responseStatus response

renderError :: Response BL.ByteString -> Text
renderError response =
  let status = responseStatus response
      prefix = "HTTP " <> T.pack (show (statusCode status))
   in case eitherDecode (responseBody response) of
        Right (ErrorResponse message) -> prefix <> ": " <> message
        Left _ ->
          let body = decodeUtf8 (BL.toStrict (responseBody response))
           in if T.null body
                then prefix
                else prefix <> ": " <> body

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
