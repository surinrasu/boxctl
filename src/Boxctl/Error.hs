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
  | InstanceConfigRequired
  | MissingClashApiConfig
  | MissingExternalController
  | MissingSsmApiServices
  | SsmApiServiceTagNotFound Text
  | MultipleSsmApiServices [Text]
  | MissingSsmApiServiceEndpoints Text
  | SsmApiEndpointNotFound Text
  | MultipleSsmApiEndpoints Text [Text]
  | MissingSsmApiListen Text
  | MissingSsmApiListenPort Text
  | MissingTailscaleEndpoints
  | TailscaleWorkdirRequired Text FilePath
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
  | UserNotFound Text
  | AmbiguousUser Text [Text]
  | PasswordPromptUnavailable
  | EmptyPassword
  | DelayProbeFailed Text ApiError
  | NoDelayResults
  | DelayTestsFailed
  | SsmShowFailed
  | SsmRemoveFailed
  | MultipleTailscaleEndpoints [Text]
  | TailscaleEndpointNotFound Text
  | AmbiguousTailscaleEndpoint Text [Text]
  | InvalidTailscaleIpFlags
  | TailscaleSnapshotUnavailable Text Text
  | TailscalePeerNotFound Text
  | AmbiguousTailscalePeer Text [Text]
  | TailscaleSelfUnavailable Text
  | NoTailscaleIPs Text
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
  InstanceConfigRequired ->
    "this command requires a sing-box config file target; use --instance-config or --instance PATH"
  MissingClashApiConfig ->
    "config file does not contain experimental.clash_api"
  MissingExternalController ->
    "config file does not contain experimental.clash_api.external_controller"
  MissingSsmApiServices ->
    "config file does not contain any ssm-api services"
  SsmApiServiceTagNotFound tagName ->
    "ssm-api service tag not found: " <> tagName
  MultipleSsmApiServices tags ->
    "multiple ssm-api services available, specify --tag: "
      <> T.intercalate ", " tags
  MissingSsmApiServiceEndpoints serviceLabel ->
    "ssm-api service has no configured endpoints: " <> serviceLabel
  SsmApiEndpointNotFound endpoint ->
    "ssm-api endpoint not found: " <> endpoint
  MultipleSsmApiEndpoints serviceLabel endpoints ->
    "multiple ssm-api endpoints available in "
      <> serviceLabel
      <> ", specify --endpoint: "
      <> T.intercalate ", " endpoints
  MissingSsmApiListen serviceLabel ->
    "ssm-api service is missing listen address: " <> serviceLabel
  MissingSsmApiListenPort serviceLabel ->
    "ssm-api service is missing listen_port: " <> serviceLabel
  MissingTailscaleEndpoints ->
    "config file does not contain any tailscale endpoints"
  TailscaleWorkdirRequired endpointLabel stateDirectory ->
    "tailscale endpoint "
      <> endpointLabel
      <> " uses relative state_directory "
      <> T.pack (show stateDirectory)
      <> "; specify --instance-workdir to match sing-box working directory"
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
  UserNotFound userName ->
    "user not found: " <> userName
  AmbiguousUser requested matches ->
    "ambiguous user: "
      <> requested
      <> " (matches: "
      <> T.intercalate ", " matches
      <> ")"
  PasswordPromptUnavailable ->
    "password not provided and no interactive terminal is available; use --password"
  EmptyPassword ->
    "password must not be empty"
  DelayProbeFailed proxyName apiError ->
    proxyName <> ": " <> renderApiError apiError
  NoDelayResults ->
    "no delay results were produced"
  DelayTestsFailed ->
    "one or more delay tests failed"
  SsmShowFailed ->
    "one or more user detail requests failed"
  SsmRemoveFailed ->
    "one or more user removals failed"
  MultipleTailscaleEndpoints labels ->
    "multiple tailscale endpoints available, specify --tag: "
      <> T.intercalate ", " labels
  TailscaleEndpointNotFound endpointLabel ->
    "tailscale endpoint not found: " <> endpointLabel
  AmbiguousTailscaleEndpoint requested matches ->
    "ambiguous tailscale endpoint: "
      <> requested
      <> " (matches: "
      <> T.intercalate ", " matches
      <> ")"
  InvalidTailscaleIpFlags ->
    "tailscale ip -1, -4, and -6 are mutually exclusive"
  TailscaleSnapshotUnavailable endpointLabel reason ->
    "tailscale endpoint " <> endpointLabel <> " is unavailable: " <> reason
  TailscalePeerNotFound peerName ->
    "tailscale peer not found: " <> peerName
  AmbiguousTailscalePeer requested matches ->
    "ambiguous tailscale peer: "
      <> requested
      <> " (matches: "
      <> T.intercalate ", " matches
      <> ")"
  TailscaleSelfUnavailable endpointLabel ->
    "tailscale endpoint " <> endpointLabel <> " does not have cached self node information"
  NoTailscaleIPs targetLabel ->
    "no tailscale IPs available for: " <> targetLabel

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
