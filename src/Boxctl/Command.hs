{-# LANGUAGE OverloadedStrings #-}

module Boxctl.Command
  ( run,
  )
where

import Boxctl.API
import Boxctl.CLI
import Boxctl.Domain
  ( Config (..),
    GroupDetails (..),
    Proxy,
    ProxyShape (..),
    SsmUser (..),
    TailscaleEndpointStatus (..),
    TailscaleIPResult (..),
    TailscalePeer (..),
    clashModeMatchesKnown,
    isSelectorProxy,
    isUrlTestProxy,
    knownClashModeText,
    proxyName,
    proxyShape,
  )
import Boxctl.Error
import Boxctl.Instance (resolveInstance, resolveSsmInstance)
import qualified Boxctl.Instance as Instance
import Boxctl.Output
  ( CommandDiagnostic (..),
    CommandDiagnosticVisibility (..),
    CommandOutput (..),
    DelayStatus (..),
    TestResult (..),
    emitCommandOutput,
  )
import Boxctl.Tailscale (loadTailscaleEndpointStatus)
import Boxctl.Terminal (resolveRenderStyle)
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (bracket_)
import Control.Monad (when, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE, withExceptT)
import Data.Either (partitionEithers)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Version (showVersion)
import Paths_boxctl (version)
import System.IO (hFlush, hGetEcho, hIsTerminalDevice, hSetEcho, stderr, stdin)

data CommandReport = CommandReport
  { reportOutput :: CommandOutput,
    reportDiagnostics :: [CommandDiagnostic],
    reportFailure :: Maybe CommandError
  }

data NameMatch a
  = MissingName
  | UniqueName a
  | AmbiguousName [a]

run :: Options -> ExceptT BoxctlError IO ()
run options = do
  report <- executeCommand options
  renderStyle <- liftIO (resolveRenderStyle options)
  liftIO $
    emitCommandOutput
      (optOutputMode options)
      renderStyle
      (reportDiagnostics report)
      (reportOutput report)
  case reportFailure report of
    Nothing -> pure ()
    Just commandError -> throwE (BoxctlCommandError commandError)

executeCommand :: Options -> ExceptT BoxctlError IO CommandReport
executeCommand options =
  case optCommand options of
    CmdVersion ->
      runVersionCommand options
    CmdTs tsOptions ->
      runTailscaleCommand options tsOptions
    CmdSsm ssmOptions -> do
      controller <- resolveSsmController options ssmOptions
      runSsmCommand controller ssmOptions
    command -> do
      controller <- resolveClashController options
      runControllerCommand controller command

resolveClashController :: Options -> ExceptT BoxctlError IO Controller
resolveClashController options = do
  resolvedInstance <- withExceptT BoxctlInstanceError (resolveInstance (optInstanceTarget options))
  liftIO (mkController resolvedInstance (optVerbose options))

resolveSsmController :: Options -> SsmOptions -> ExceptT BoxctlError IO Controller
resolveSsmController options ssmOptions = do
  resolvedInstance <-
    withExceptT
      BoxctlInstanceError
      ( resolveSsmInstance
          (optInstanceTarget options)
          (ssmTargetTag ssmOptions)
          (ssmTargetEndpoint ssmOptions)
      )
  liftIO (mkController resolvedInstance (optVerbose options))

runVersionCommand :: Options -> ExceptT BoxctlError IO CommandReport
runVersionCommand options = do
  let localVersion = T.pack (showVersion version)
  serverVersionResult <-
    liftIO $
      runExceptT $
        do
          controller <- resolveClashController options
          withExceptT BoxctlApiError (fetchVersion controller)
  pure $
    case serverVersionResult of
      Right versionInfo ->
        successReport (CommandOutputVersion localVersion (Just versionInfo))
      Left err ->
        CommandReport
          { reportOutput = CommandOutputVersion localVersion Nothing,
            reportDiagnostics =
              [ CommandDiagnostic
                  { diagnosticVisibility = DiagnosticAlways,
                    diagnosticText = "boxctl: server version unavailable: " <> renderBoxctlError err
                  }
              | optVerbose options
              ],
            reportFailure = Nothing
          }

runControllerCommand :: Controller -> Command -> ExceptT BoxctlError IO CommandReport
runControllerCommand controller command =
  case command of
    CmdVersion ->
      pure (successReport (CommandOutputVersion (T.pack (showVersion version)) Nothing))
    CmdMode -> do
      config <- withExceptT BoxctlApiError (fetchConfigs controller)
      pure (successReport (CommandOutputMode config))
    CmdSwitch newMode -> do
      supported <- withExceptT BoxctlApiError (fetchConfigs controller)
      unless (null (configModeList supported) || any (clashModeMatchesKnown newMode) (configModeList supported)) $
        throwE (BoxctlCommandError (UnsupportedMode (knownClashModeText newMode)))
      withExceptT BoxctlApiError (switchMode controller newMode)
      config <- withExceptT BoxctlApiError (fetchConfigs controller)
      pure (successReport (CommandOutputSwitch config))
    CmdList listOptions -> do
      proxies <- withExceptT BoxctlApiError (fetchProxies controller)
      pure (successReport (CommandOutputList listOptions proxies))
    CmdShow selection -> do
      allProxies <- withExceptT BoxctlApiError (fetchProxies controller)
      proxies <- exceptCommand (selectProxiesFrom selection allProxies)
      pure (successReport (CommandOutputShow (proxyIndex allProxies) proxies))
    CmdTest selection -> do
      allProxies <- withExceptT BoxctlApiError (fetchProxies controller)
      proxies <- exceptCommand (selectProxiesFrom selection allProxies)
      tested <- liftIO (mapConcurrently (runExceptT . testProxy controller) proxies)
      let (errs, results) = partitionEithers tested
          renderedErrors = map renderBoxctlError errs
          diagnostics = humanOnlyDiagnostics renderedErrors
          failure
            | null results = Just NoDelayResults
            | null errs = Nothing
            | otherwise = Just DelayTestsFailed
      pure
        CommandReport
          { reportOutput = CommandOutputTest results renderedErrors,
            reportDiagnostics = diagnostics,
            reportFailure = failure
          }
    CmdSelect selectCommand -> do
      proxies <- withExceptT BoxctlApiError (fetchProxies controller)
      (selectorProxy, selectorOption) <- exceptCommand (resolveSelector proxies selectCommand)
      withExceptT BoxctlApiError (selectProxyOption controller (proxyName selectorProxy) selectorOption)
      pure (successReport (CommandOutputSelect (proxyName selectorProxy) selectorOption))
    CmdSsm _ ->
      error "runControllerCommand: unexpected SSM command"
    CmdTs _ ->
      error "runControllerCommand: unexpected TS command"

runTailscaleCommand :: Options -> TsOptions -> ExceptT BoxctlError IO CommandReport
runTailscaleCommand options tsOptions = do
  resolvedConfig <-
    withExceptT
      BoxctlInstanceError
      (Instance.resolveTailscaleConfig (optInstanceTarget options) (optInstanceWorkdir options))
  case tsSubcommand tsOptions of
    TsStatus -> do
      selectedEndpoints <- exceptCommand (selectTailscaleStatusEndpoints tsOptions resolvedConfig)
      statuses <- withExceptT BoxctlInstanceError (loadTailscaleStatuses resolvedConfig selectedEndpoints)
      pure (successReport (CommandOutputTailscaleStatus statuses))
    TsIp ipOptions -> do
      selectedEndpoint <- exceptCommand (selectTailscaleIpEndpoint tsOptions resolvedConfig)
      status <- withExceptT BoxctlInstanceError (loadTailscaleStatus resolvedConfig selectedEndpoint)
      validateTailscaleIpOptions ipOptions
      unless (tailscaleEndpointAvailable status) $
        exceptCommand $
          Left
            ( TailscaleSnapshotUnavailable
                (tailscaleEndpointLabel status)
                (fromMaybe "no cached tailscale status is available" (tailscaleEndpointReason status))
            )
      selectedPeer <- exceptCommand (resolveTailscalePeer status (tsIpPeer ipOptions))
      addresses <- exceptCommand (selectTailscaleIPs ipOptions selectedPeer)
      pure
        ( successReport
            ( CommandOutputTailscaleIP
                TailscaleIPResult
                  { tailscaleIPResultEndpoint = tailscaleEndpointLabel status,
                    tailscaleIPResultPeer = Just (tailscalePeerName selectedPeer),
                    tailscaleIPResultAddresses = addresses,
                    tailscaleIPResultSnapshotTime = tailscaleEndpointSnapshotTime status
                  }
            )
        )

loadTailscaleStatuses ::
  Instance.ResolvedTailscaleConfig ->
  [Instance.TailscaleEndpointConfig] ->
  ExceptT InstanceError IO [TailscaleEndpointStatus]
loadTailscaleStatuses resolvedConfig =
  mapM (loadTailscaleStatus resolvedConfig)

loadTailscaleStatus ::
  Instance.ResolvedTailscaleConfig ->
  Instance.TailscaleEndpointConfig ->
  ExceptT InstanceError IO TailscaleEndpointStatus
loadTailscaleStatus resolvedConfig endpoint = do
  stateDirectory <- Instance.resolveTailscaleStateDirectory resolvedConfig endpoint
  liftIO (loadTailscaleEndpointStatus endpoint stateDirectory)

selectTailscaleStatusEndpoints ::
  TsOptions ->
  Instance.ResolvedTailscaleConfig ->
  Either CommandError [Instance.TailscaleEndpointConfig]
selectTailscaleStatusEndpoints tsOptions resolvedConfig =
  case tsTargetTag tsOptions of
    Nothing ->
      Right (Instance.resolvedTailscaleEndpoints resolvedConfig)
    Just requestedTag ->
      (: [])
        <$> resolveTailscaleEndpointByLabel (Instance.resolvedTailscaleEndpoints resolvedConfig) requestedTag

selectTailscaleIpEndpoint ::
  TsOptions ->
  Instance.ResolvedTailscaleConfig ->
  Either CommandError Instance.TailscaleEndpointConfig
selectTailscaleIpEndpoint tsOptions resolvedConfig =
  case tsTargetTag tsOptions of
    Just requestedTag ->
      resolveTailscaleEndpointByLabel endpoints requestedTag
    Nothing ->
      case endpoints of
        [endpoint] -> Right endpoint
        many -> Left (MultipleTailscaleEndpoints (map Instance.tailscaleEndpointLabel many))
  where
    endpoints = Instance.resolvedTailscaleEndpoints resolvedConfig

resolveTailscaleEndpointByLabel ::
  [Instance.TailscaleEndpointConfig] ->
  Text ->
  Either CommandError Instance.TailscaleEndpointConfig
resolveTailscaleEndpointByLabel endpoints requestedLabel =
  case findByName Instance.tailscaleEndpointLabel endpoints requestedLabel of
    MissingName ->
      Left (TailscaleEndpointNotFound requestedLabel)
    UniqueName endpoint ->
      Right endpoint
    AmbiguousName matches ->
      Left (AmbiguousTailscaleEndpoint requestedLabel (map Instance.tailscaleEndpointLabel matches))

validateTailscaleIpOptions :: TsIpOptions -> ExceptT BoxctlError IO ()
validateTailscaleIpOptions ipOptions = do
  let enabledFlagCount =
        length
          ( filter
              id
              [ tsIpWant1 ipOptions,
                tsIpWant4 ipOptions,
                tsIpWant6 ipOptions
              ]
          )
  when (enabledFlagCount > 1) $
    throwE (BoxctlCommandError InvalidTailscaleIpFlags)

resolveTailscalePeer :: TailscaleEndpointStatus -> Maybe Text -> Either CommandError TailscalePeer
resolveTailscalePeer status maybePeerName =
  case maybePeerName of
    Nothing ->
      maybe
        (Left (TailscaleSelfUnavailable (tailscaleEndpointLabel status)))
        Right
        (tailscaleEndpointSelf status)
    Just requestedPeer ->
      case findMatchingTailscalePeers requestedPeer status of
        [] ->
          Left (TailscalePeerNotFound requestedPeer)
        [peer] ->
          Right peer
        peers ->
          Left (AmbiguousTailscalePeer requestedPeer (map tailscalePeerName peers))

findMatchingTailscalePeers :: Text -> TailscaleEndpointStatus -> [TailscalePeer]
findMatchingTailscalePeers requestedPeer status =
  filter matchesPeer (availablePeers status)
  where
    availablePeers endpointStatus =
      maybe [] pure (tailscaleEndpointSelf endpointStatus)
        <> tailscaleEndpointPeers endpointStatus

    requestedFolded = T.toCaseFold (T.strip requestedPeer)

    matchesPeer peer =
      any ((== requestedFolded) . T.toCaseFold) (tailscalePeerAliases status peer)
        || any ((== requestedFolded) . T.toCaseFold) (tailscalePeerIPs peer)

tailscalePeerAliases :: TailscaleEndpointStatus -> TailscalePeer -> [Text]
tailscalePeerAliases status peer =
  dedupeTextValues $
    [ tailscalePeerName peer ]
      <> maybe [] pure (tailscalePeerHostName peer)
      <> maybe [] pure (tailscalePeerDNSName peer)
      <> maybe [] pure (tailscaleShortMagicDNSName status =<< tailscalePeerDNSName peer)

tailscaleShortMagicDNSName :: TailscaleEndpointStatus -> Text -> Maybe Text
tailscaleShortMagicDNSName status dnsName =
  case tailscaleEndpointMagicDNSSuffix status of
    Just suffix -> T.stripSuffix ("." <> suffix) dnsName
    Nothing -> Nothing

selectTailscaleIPs :: TsIpOptions -> TailscalePeer -> Either CommandError [Text]
selectTailscaleIPs ipOptions peer = do
  let addressFilter
        | tsIpWant4 ipOptions = filter isIPv4Address
        | tsIpWant6 ipOptions = filter isIPv6Address
        | otherwise = id
      filteredAddresses = addressFilter (tailscalePeerIPs peer)
      selectedAddresses =
        if tsIpWant1 ipOptions
          then take 1 filteredAddresses
          else filteredAddresses
  if null selectedAddresses
    then Left (NoTailscaleIPs (tailscalePeerName peer))
    else Right selectedAddresses

isIPv4Address :: Text -> Bool
isIPv4Address address =
  T.count ":" address == 0 && T.count "." address == 3

isIPv6Address :: Text -> Bool
isIPv6Address =
  T.isInfixOf ":"

runSsmCommand :: Controller -> SsmOptions -> ExceptT BoxctlError IO CommandReport
runSsmCommand controller ssmOptions =
  case ssmSubcommand ssmOptions of
    SsmList listOptions -> do
      users <- withExceptT BoxctlApiError (listSsmUsers controller)
      pure (successReport (CommandOutputSsmList (ssmListShowPassword listOptions) users))
    SsmShow requestedUserNames ->
      runSsmShow controller requestedUserNames
    SsmAdd userName maybePassword -> do
      password <- resolvePasswordValue ("Password for " <> userName) maybePassword
      withExceptT BoxctlApiError (addSsmUser controller userName password)
      pure (successReport (CommandOutputSsmAdd userName))
    SsmRemove requestedUserNames ->
      runSsmRemove controller requestedUserNames
    SsmUpdate userName maybePassword -> do
      password <- resolvePasswordValue ("Password for " <> userName) maybePassword
      withExceptT BoxctlApiError (updateSsmUserPassword controller userName password)
      pure (successReport (CommandOutputSsmUpdate userName))
    SsmStat statOptions -> do
      stats <- withExceptT BoxctlApiError (fetchSsmStats controller (ssmStatClear statOptions))
      pure (successReport (CommandOutputSsmStat (ssmStatClear statOptions) stats))

runSsmShow :: Controller -> [Text] -> ExceptT BoxctlError IO CommandReport
runSsmShow controller requestedUserNames = do
  users <- withExceptT BoxctlApiError (listSsmUsers controller)
  selectedUsers <- exceptCommand (resolveUsersByName users requestedUserNames)
  fetchedUsers <-
    liftIO $
      mapConcurrently
        ( runExceptT
            . withExceptT BoxctlApiError
            . fetchSsmUser controller
            . ssmUserName
        )
        selectedUsers
  let (errs, detailedUsers) = partitionEithers fetchedUsers
      renderedErrors = map renderBoxctlError errs
      diagnostics = humanOnlyDiagnostics renderedErrors
      failure
        | null errs = Nothing
        | otherwise = Just SsmShowFailed
  pure
    CommandReport
      { reportOutput = CommandOutputSsmShow detailedUsers renderedErrors,
        reportDiagnostics = diagnostics,
        reportFailure = failure
      }

runSsmRemove :: Controller -> [Text] -> ExceptT BoxctlError IO CommandReport
runSsmRemove controller requestedUserNames = do
  users <- withExceptT BoxctlApiError (listSsmUsers controller)
  selectedUsers <- exceptCommand (resolveUsersByName users requestedUserNames)
  removedUsers <-
    liftIO $
      mapConcurrently
        ( runExceptT
            . removeResolvedUser controller
            . ssmUserName
        )
        selectedUsers
  let (errs, removedUserNames) = partitionEithers removedUsers
      renderedErrors = map renderBoxctlError errs
      diagnostics = humanOnlyDiagnostics renderedErrors
      failure
        | null errs = Nothing
        | otherwise = Just SsmRemoveFailed
  pure
    CommandReport
      { reportOutput = CommandOutputSsmRemove removedUserNames renderedErrors,
        reportDiagnostics = diagnostics,
        reportFailure = failure
      }

removeResolvedUser :: Controller -> Text -> ExceptT BoxctlError IO Text
removeResolvedUser controller userName = do
  withExceptT BoxctlApiError (removeSsmUser controller userName)
  pure userName

resolvePasswordValue :: Text -> Maybe Text -> ExceptT BoxctlError IO Text
resolvePasswordValue promptLabel maybePassword = do
  password <-
    case maybePassword of
      Just providedPassword -> pure providedPassword
      Nothing -> promptPassword promptLabel
  if T.null (T.strip password)
    then throwE (BoxctlCommandError EmptyPassword)
    else pure password

promptPassword :: Text -> ExceptT BoxctlError IO Text
promptPassword promptLabel = do
  isInteractive <- liftIO (hIsTerminalDevice stdin)
  unless isInteractive $
    throwE (BoxctlCommandError PasswordPromptUnavailable)
  liftIO $ do
    originalEcho <- hGetEcho stdin
    TIO.hPutStr stderr (promptLabel <> ": ")
    hFlush stderr
    password <-
      bracket_
        (hSetEcho stdin False)
        (hSetEcho stdin originalEcho)
        (TIO.hGetLine stdin)
    TIO.hPutStrLn stderr ""
    pure password

successReport :: CommandOutput -> CommandReport
successReport output =
  CommandReport
    { reportOutput = output,
      reportDiagnostics = [],
      reportFailure = Nothing
    }

humanOnlyDiagnostics :: [Text] -> [CommandDiagnostic]
humanOnlyDiagnostics =
  map
    (\message -> CommandDiagnostic {diagnosticVisibility = DiagnosticHumanOnly, diagnosticText = message})

exceptCommand :: Either CommandError a -> ExceptT BoxctlError IO a
exceptCommand =
  either (throwE . BoxctlCommandError) pure

selectProxiesFrom :: ProxySelection -> [Proxy] -> Either CommandError [Proxy]
selectProxiesFrom selection allProxies =
  if null requestedNames
    then
      if null filteredAll
        then Left NoMatchingOutbounds
        else Right filteredAll
    else do
      proxies <- traverse (resolveProxyByName allProxies) requestedNames
      filtered <- traverse ensureMatch proxies
      if null filtered
        then Left NoMatchingOutbounds
        else Right filtered
  where
    requestedNames = selectionNames selection
    filteredAll =
      filter
        (filterMatches (selectionFilter selection))
        (sortOn (T.toCaseFold . proxyName) allProxies)

    ensureMatch proxy
      | filterMatches (selectionFilter selection) proxy = Right proxy
      | otherwise = Left (OutboundDoesNotMatchFilter (proxyName proxy))

resolveUsersByName :: [SsmUser] -> [Text] -> Either CommandError [SsmUser]
resolveUsersByName users =
  traverse (resolveUserByName users) . dedupeTextValues

resolveUserByName :: [SsmUser] -> Text -> Either CommandError SsmUser
resolveUserByName users requestedName =
  case findByName ssmUserName users requestedName of
    MissingName ->
      Left (UserNotFound requestedName)
    UniqueName user ->
      Right user
    AmbiguousName matches ->
      Left (AmbiguousUser requestedName (map ssmUserName matches))

dedupeTextValues :: [Text] -> [Text]
dedupeTextValues =
  foldl' insertIfMissing []
  where
    insertIfMissing acc value
      | any (`textEqualsFold` value) acc = acc
      | otherwise = acc <> [value]

resolveSelector :: [Proxy] -> SelectCommand -> Either CommandError (Proxy, Text)
resolveSelector proxies selectCommand = do
  let selectors = filter isSelectorProxy proxies
  selectorProxy <-
    case selectCommand of
      SelectBySelector requestedName _ ->
        resolveSelectorByName selectors requestedName
      SelectByOption _ ->
        case selectors of
          [onlySelector] -> Right onlySelector
          [] -> Left NoSelectorOutbounds
          manySelectors ->
            Left (MultipleSelectors (map proxyName manySelectors))
  selectedOption <- resolveSelectorOption selectorProxy optionName
  Right (selectorProxy, selectedOption)
  where
    optionName =
      case selectCommand of
        SelectByOption singleOption -> singleOption
        SelectBySelector _ namedOption -> namedOption

testProxy :: Controller -> Proxy -> ExceptT BoxctlError IO TestResult
testProxy controller proxy =
  case proxyShape proxy of
    ProxyLeaf _ -> testLeafProxy controller proxy
    ProxyGroup _ details -> testGroupProxy controller proxy details

testLeafProxy :: Controller -> Proxy -> ExceptT BoxctlError IO TestResult
testLeafProxy controller proxy = do
  delayResult <- liftIO (runExceptT (fetchProxyDelay controller (proxyName proxy) 15000))
  case delayResult of
    Right delayMs ->
      pure (ProxyDelay proxy (DelayOk delayMs))
    Left apiErr
      | Just label <- apiDelayFailureLabel apiErr ->
          pure (ProxyDelay proxy (DelayUnavailable label))
      | otherwise ->
          throwE (BoxctlCommandError (DelayProbeFailed (proxyName proxy) apiErr))

testGroupProxy :: Controller -> Proxy -> GroupDetails -> ExceptT BoxctlError IO TestResult
testGroupProxy controller proxy details = do
  groupDelayResult <- liftIO (runExceptT (fetchGroupDelay controller (proxyName proxy) 15000))
  case groupDelayResult of
    Right delays ->
      pure (GroupDelay proxy (materializeGroupDelays details delays))
    Left apiErr
      | Just label <- apiDelayFailureLabel apiErr ->
          pure (GroupDelay proxy (materializeGroupError details label))
      | otherwise ->
          throwE (BoxctlCommandError (DelayProbeFailed (proxyName proxy) apiErr))

materializeGroupDelays :: GroupDetails -> Map Text Int -> [(Text, DelayStatus)]
materializeGroupDelays details measured =
  map renderMember orderedMembers
  where
    orderedMembers =
      case groupMembers details of
        [] -> Map.keys measured
        members -> members
    renderMember memberName =
      case Map.lookup memberName measured of
        Just delayMs -> (memberName, DelayOk delayMs)
        Nothing -> (memberName, DelayUnavailable "unavailable")

materializeGroupError :: GroupDetails -> Text -> [(Text, DelayStatus)]
materializeGroupError details label =
  map (\memberName -> (memberName, DelayUnavailable label)) (groupMembers details)

proxyIndex :: [Proxy] -> Map Text Proxy
proxyIndex proxies =
  Map.fromList [(proxyName proxy, proxy) | proxy <- proxies]

findByName :: (a -> Text) -> [a] -> Text -> NameMatch a
findByName getName values requestedName =
  case filter (\value -> getName value == requestedName) values of
    [value] ->
      UniqueName value
    [] ->
      case filter (\value -> textEqualsFold (getName value) requestedName) values of
        [] -> MissingName
        [value] -> UniqueName value
        matches -> AmbiguousName matches
    matches ->
      AmbiguousName matches

resolveProxyByName :: [Proxy] -> Text -> Either CommandError Proxy
resolveProxyByName proxies requestedName =
  case findByName proxyName proxies requestedName of
    MissingName ->
      Left (OutboundNotFound requestedName)
    UniqueName proxy ->
      Right proxy
    AmbiguousName matches ->
      Left (AmbiguousOutbound requestedName (map proxyName matches))

resolveSelectorByName :: [Proxy] -> Text -> Either CommandError Proxy
resolveSelectorByName selectors requestedName =
  case findByName proxyName selectors requestedName of
    MissingName ->
      Left (SelectorNotFound requestedName)
    UniqueName selectorProxy ->
      Right selectorProxy
    AmbiguousName matches ->
      Left (AmbiguousSelector requestedName (map proxyName matches))

resolveSelectorOption :: Proxy -> Text -> Either CommandError Text
resolveSelectorOption selectorProxy requestedOption =
  case proxyShape selectorProxy of
    ProxyLeaf _ ->
      Left (OptionNotFound (proxyName selectorProxy) requestedOption)
    ProxyGroup _ details ->
      case findByName id (groupMembers details) requestedOption of
        MissingName ->
          Left (OptionNotFound (proxyName selectorProxy) requestedOption)
        UniqueName selectedOption ->
          Right selectedOption
        AmbiguousName matches ->
          Left (AmbiguousOption (proxyName selectorProxy) requestedOption matches)

filterMatches :: ProxyFilter -> Proxy -> Bool
filterMatches FilterAll _ = True
filterMatches FilterSelectors proxy = isSelectorProxy proxy
filterMatches FilterUrlTests proxy = isUrlTestProxy proxy

textEqualsFold :: Text -> Text -> Bool
textEqualsFold left right = T.toCaseFold left == T.toCaseFold right
