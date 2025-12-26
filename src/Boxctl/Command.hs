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
    clashModeMatchesKnown,
    knownClashModeText,
    isSelectorProxy,
    isUrlTestProxy,
    proxyName,
    proxyShape,
  )
import Boxctl.Error
import Boxctl.Instance (resolveInstance)
import Boxctl.Output
  ( CommandDiagnostic (..),
    CommandDiagnosticVisibility (..),
    CommandOutput (..),
    DelayStatus (..),
    TestResult (..),
    emitCommandOutput,
  )
import Boxctl.Terminal (resolveRenderStyle)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE, withExceptT)
import Data.Either (partitionEithers)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Version (showVersion)
import Paths_boxctl (version)

data CommandReport = CommandReport
  { reportOutput :: CommandOutput,
    reportDiagnostics :: [CommandDiagnostic],
    reportFailure :: Maybe CommandError
  }

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
    command -> do
      controller <- resolveController options
      runControllerCommand controller command

resolveController :: Options -> ExceptT BoxctlError IO Controller
resolveController options = do
  resolvedInstance <- withExceptT BoxctlInstanceError (resolveInstance (optInstanceTarget options))
  liftIO (mkController resolvedInstance (optVerbose options))

runVersionCommand :: Options -> ExceptT BoxctlError IO CommandReport
runVersionCommand options = do
  let localVersion = T.pack (showVersion version)
  serverVersionResult <-
    liftIO $
      runExceptT $
        do
          controller <- resolveController options
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
          diagnostics =
            map
              (\message -> CommandDiagnostic {diagnosticVisibility = DiagnosticHumanOnly, diagnosticText = message})
              renderedErrors
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

successReport :: CommandOutput -> CommandReport
successReport output =
  CommandReport
    { reportOutput = output,
      reportDiagnostics = [],
      reportFailure = Nothing
    }

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

data NameMatch a
  = MissingName
  | UniqueName a
  | AmbiguousName [a]

findByName :: (a -> Text) -> [a] -> Text -> NameMatch a
findByName getName values requestedName =
  case filter (\value -> getName value == requestedName) values of
    value : _ ->
      UniqueName value
    [] ->
      case filter (\value -> textEqualsFold (getName value) requestedName) values of
        [] -> MissingName
        [value] -> UniqueName value
        matches -> AmbiguousName matches

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
