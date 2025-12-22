{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Boxctl.Error
  ( ApiError (..),
    BoxctlError (..),
    CommandError (..),
    InstanceError (..),
    TransportError (..),
    apiDelayFailureLabel,
    renderApiError,
    renderBoxctlError,
  )
where

import Data.Text (Text)
import qualified Data.Text as T

data BoxctlError
  = BoxctlInstanceError InstanceError
  | BoxctlApiError ApiError
  | BoxctlCommandError CommandError
  deriving (Eq, Show)

data InstanceError
  = InstancePathIsDirectory
  | InstanceConfigFileNotFound FilePath
  | InstanceConfigParseError Text
  | MissingClashApiConfig
  | MissingExternalController
  | InvalidInstanceAddress Text
  deriving (Eq, Show)

data ApiError
  = ApiRequestBuildError Text
  | ApiTransportError TransportError
  | ApiHttpError Int (Maybe Text)
  | ApiDecodeError Text
  deriving (Eq, Show)

data TransportError
  = TransportTimeout
  | TransportFailure Text
  deriving (Eq, Show)

data CommandError
  = UnsupportedMode Text
  | NoMatchingOutbounds
  | OutboundNotFound Text
  | AmbiguousOutbound Text [Text]
  | OutboundDoesNotMatchFilter Text
  | NoSelectorOutbounds
  | MultipleSelectors [Text]
  | SelectorNotFound Text
  | AmbiguousSelector Text [Text]
  | OptionNotFound Text Text
  | AmbiguousOption Text Text [Text]
  | DelayProbeFailed Text ApiError
  | NoDelayResults
  | DelayTestsFailed
  deriving (Eq, Show)

renderBoxctlError :: BoxctlError -> Text
renderBoxctlError = \case
  BoxctlInstanceError err -> renderInstanceError err
  BoxctlApiError err -> renderApiError err
  BoxctlCommandError err -> renderCommandError err

renderInstanceError :: InstanceError -> Text
renderInstanceError = \case
  InstancePathIsDirectory ->
    "instance path must be a config file, not a directory"
  InstanceConfigFileNotFound path ->
    "instance config file not found: " <> T.pack path
  InstanceConfigParseError err ->
    "failed to parse config file: " <> err
  MissingClashApiConfig ->
    "config file does not contain experimental.clash_api"
  MissingExternalController ->
    "config file does not contain experimental.clash_api.external_controller"
  InvalidInstanceAddress err ->
    err

renderApiError :: ApiError -> Text
renderApiError = \case
  ApiRequestBuildError err ->
    "failed to build request: " <> err
  ApiTransportError err ->
    case err of
      TransportTimeout -> "request failed: timeout"
      TransportFailure message -> "request failed: " <> message
  ApiHttpError status maybeMessage ->
    let prefix = "HTTP " <> T.pack (show status)
     in maybe prefix (\message -> prefix <> ": " <> message) maybeMessage
  ApiDecodeError err ->
    "failed to decode API response: " <> err

renderCommandError :: CommandError -> Text
renderCommandError = \case
  UnsupportedMode mode ->
    "mode not supported by server: " <> mode
  NoMatchingOutbounds ->
    "no matching outbounds"
  OutboundNotFound outbound ->
    "outbound not found: " <> outbound
  AmbiguousOutbound requested matches ->
    "ambiguous outbound: "
      <> requested
      <> " (matches: "
      <> T.intercalate ", " matches
      <> ")"
  OutboundDoesNotMatchFilter outbound ->
    "outbound does not match filter: " <> outbound
  NoSelectorOutbounds ->
    "no selector outbounds found"
  MultipleSelectors selectors ->
    "multiple selectors available, specify one of: "
      <> T.intercalate ", " selectors
  SelectorNotFound selectorName ->
    "selector not found: " <> selectorName
  AmbiguousSelector requested matches ->
    "ambiguous selector: "
      <> requested
      <> " (matches: "
      <> T.intercalate ", " matches
      <> ")"
  OptionNotFound selectorName optionName ->
    "option not found in selector " <> selectorName <> ": " <> optionName
  AmbiguousOption selectorName requested matches ->
    "ambiguous option in selector "
      <> selectorName
      <> ": "
      <> requested
      <> " (matches: "
      <> T.intercalate ", " matches
      <> ")"
  DelayProbeFailed proxyName apiError ->
    proxyName <> ": " <> renderApiError apiError
  NoDelayResults ->
    "no delay results were produced"
  DelayTestsFailed ->
    "one or more delay tests failed"

apiDelayFailureLabel :: ApiError -> Maybe Text
apiDelayFailureLabel = \case
  ApiTransportError TransportTimeout ->
    Just "timeout"
  ApiHttpError 503 _ ->
    Just "unavailable"
  ApiHttpError 504 _ ->
    Just "timeout"
  _ ->
    Nothing
