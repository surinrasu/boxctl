{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Boxctl.API
  ( Controller,
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

import Boxctl.Domain
  ( Config (..),
    DelayHistory (..),
    GroupDetails (..),
    GroupKind (..),
    KnownClashMode,
    Proxy (..),
    ProxyMeta (..),
    ProxyShape (..),
    VersionInfo (..),
    clashModeFromText,
    proxyName,
  )
import Boxctl.Error (ApiError (..), TransportError (..))
import Boxctl.Instance (ResolvedInstance (..))
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

data Controller = Controller
  { controllerManager :: Manager,
    controllerInstance :: ResolvedInstance,
    controllerVerbose :: Bool
  }

newtype ApiVersion = ApiVersion
  { unApiVersion :: VersionInfo
  }

newtype ApiConfig = ApiConfig
  { unApiConfig :: Config
  }

data ApiProxy = ApiProxy
  { apiProxyType :: Text,
    apiProxyName :: Text,
    apiProxyUdp :: Bool,
    apiProxyHistory :: [DelayHistory],
    apiProxyCurrent :: Maybe Text,
    apiProxyMembers :: Maybe [Text]
  }

newtype ApiProxyEnvelope = ApiProxyEnvelope (Map Text ApiProxy)

newtype ApiDelay = ApiDelay
  { unApiDelay :: Int
  }

newtype ApiErrorResponse = ApiErrorResponse Text

newtype ModePatch = ModePatch KnownClashMode

newtype SelectRequest = SelectRequest Text

instance FromJSON ApiVersion where
  parseJSON = withObject "VersionResponse" $ \obj ->
    ApiVersion
      <$> ( VersionInfo
              <$> obj .: "version"
              <*> obj .:? "premium" .!= False
              <*> obj .:? "meta" .!= False
          )

instance FromJSON ApiConfig where
  parseJSON = withObject "ConfigResponse" $ \obj ->
    ApiConfig
      <$> ( Config
              <$> (clashModeFromText <$> obj .: "mode")
              <*> fmap (map clashModeFromText) (obj .:? "mode-list" .!= [])
          )

parseDelayHistory :: Value -> Parser DelayHistory
parseDelayHistory =
  withObject "DelayHistory" $ \obj ->
    DelayHistory
      <$> obj .: "time"
      <*> obj .: "delay"

instance FromJSON ApiProxy where
  parseJSON = withObject "DelayHistory" $ \obj ->
    ApiProxy
      <$> obj .: "type"
      <*> obj .: "name"
      <*> obj .:? "udp" .!= False
      <*> (obj .:? "history" .!= [] >>= traverse parseDelayHistory)
      <*> obj .:? "now"
      <*> obj .:? "all"

instance FromJSON ApiDelay where
  parseJSON = withObject "DelayResponse" $ \obj ->
    ApiDelay <$> obj .: "delay"

instance FromJSON ApiProxyEnvelope where
  parseJSON = withObject "ProxyEnvelope" $ \obj ->
    ApiProxyEnvelope <$> obj .: "proxies"

instance FromJSON ApiErrorResponse where
  parseJSON = withObject "ErrorResponse" $ \obj ->
    ApiErrorResponse <$> obj .: "message"

instance ToJSON ModePatch where
  toJSON (ModePatch newMode) =
    object ["mode" .= newMode]

instance ToJSON SelectRequest where
  toJSON (SelectRequest name) =
    object ["name" .= name]

mkController :: ResolvedInstance -> Bool -> IO Controller
mkController controllerInstance controllerVerbose = do
  controllerManager <- newTlsManager
  pure $
    Controller
      { controllerManager = controllerManager,
        controllerInstance = controllerInstance,
        controllerVerbose = controllerVerbose
      }

fetchVersion :: Controller -> ExceptT ApiError IO VersionInfo
fetchVersion controller =
  unApiVersion <$> requestJson controller methodGet ["version"] [] Nothing

fetchConfigs :: Controller -> ExceptT ApiError IO Config
fetchConfigs controller =
  unApiConfig <$> requestJson controller methodGet ["configs"] [] Nothing

switchMode :: Controller -> KnownClashMode -> ExceptT ApiError IO ()
switchMode controller newMode =
  requestNoContent controller methodPatch ["configs"] [] (Just (toJSON (ModePatch newMode)))

fetchProxies :: Controller -> ExceptT ApiError IO [Proxy]
fetchProxies controller = do
  ApiProxyEnvelope apiProxies <- requestJson controller methodGet ["proxies"] [] Nothing
  proxies <-
    exceptEither $
      first ApiDecodeError $
        traverse toDomainProxy (Map.elems apiProxies)
  pure (filter (\proxy -> proxyName proxy /= "GLOBAL") proxies)

fetchProxyDelay :: Controller -> Text -> Int -> ExceptT ApiError IO Int
fetchProxyDelay controller targetName timeoutMs =
  unApiDelay <$> requestJson controller methodGet ["proxies", targetName, "delay"] (delayQuery timeoutMs) Nothing

fetchGroupDelay :: Controller -> Text -> Int -> ExceptT ApiError IO (Map Text Int)
fetchGroupDelay controller targetName timeoutMs =
  requestJson controller methodGet ["group", targetName, "delay"] (delayQuery timeoutMs) Nothing

selectProxyOption :: Controller -> Text -> Text -> ExceptT ApiError IO ()
selectProxyOption controller selectorName optionName =
  requestNoContent controller methodPut ["proxies", selectorName] [] (Just (toJSON (SelectRequest optionName)))

delayQuery :: Int -> [(BS.ByteString, Maybe BS.ByteString)]
delayQuery timeoutMs =
  [ ("url", Just (TE.encodeUtf8 "https://www.gstatic.com/generate_204")),
    ("timeout", Just (BS8.pack (show timeoutMs)))
  ]

toDomainProxy :: ApiProxy -> Either Text Proxy
toDomainProxy apiProxy =
  case (normalizedType, apiProxyMembers apiProxy) of
    ("selector", Just members) ->
      Right
        Proxy
          { proxyMeta = meta,
            proxyShape = ProxyGroup GroupKindSelector (GroupDetails (apiProxyCurrent apiProxy) members)
          }
    ("selector", Nothing) ->
      Left ("selector proxy missing group members: " <> apiProxyName apiProxy)
    ("url-test", Just members) ->
      Right
        Proxy
          { proxyMeta = meta,
            proxyShape = ProxyGroup GroupKindUrlTest (GroupDetails (apiProxyCurrent apiProxy) members)
          }
    ("url-test", Nothing) ->
      Left ("url-test proxy missing group members: " <> apiProxyName apiProxy)
    (_, Just members) ->
      Right
        Proxy
          { proxyMeta = meta,
            proxyShape = ProxyGroup (GroupKindGeneric normalizedType) (GroupDetails (apiProxyCurrent apiProxy) members)
          }
    (_, Nothing)
      | Just _ <- apiProxyCurrent apiProxy ->
          Left ("leaf proxy unexpectedly contains current member: " <> apiProxyName apiProxy)
      | otherwise ->
          Right
            Proxy
              { proxyMeta = meta,
                proxyShape = ProxyLeaf normalizedType
              }
  where
    meta =
      ProxyMeta
        { proxyMetaName = apiProxyName apiProxy,
          proxyMetaUdp = apiProxyUdp apiProxy,
          proxyMetaHistory = apiProxyHistory apiProxy
        }
    normalizedType = normalizeProxyTypeLabel (apiProxyType apiProxy)

normalizeProxyTypeLabel :: Text -> Text
normalizeProxyTypeLabel rawValue =
  case T.toCaseFold (T.strip rawValue) of
    "urltest" -> "url-test"
    normalized -> normalized

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
    Right (ApiErrorResponse message) ->
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
