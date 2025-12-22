{-# LANGUAGE OverloadedStrings #-}

module Boxctl.Command
  ( run,
  )
where

import Boxctl.API
import Boxctl.CLI
import Boxctl.Error
import Boxctl.Instance (resolveInstance)
import Boxctl.Output
import Boxctl.Terminal (resolveRenderStyle)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE, withExceptT)
import Data.Aeson (Value, encode, object, toJSON, (.=))
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Either (partitionEithers)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Version (showVersion)
import Paths_boxctl (version)
import System.IO (stderr)

run :: Options -> ExceptT BoxctlError IO ()
run options =
  case optCommand options of
    CmdVersion ->
      runVersionCommand options
    command -> do
      renderStyle <- liftIO (resolveRenderStyle options)
      controller <- resolveController options
      runCommand renderStyle (optOutputMode options) controller command

resolveController :: Options -> ExceptT BoxctlError IO Controller
resolveController options = do
  resolvedInstance <- withExceptT BoxctlInstanceError (resolveInstance (optInstance options))
  liftIO (mkController resolvedInstance (optVerbose options))

runVersionCommand :: Options -> ExceptT BoxctlError IO ()
runVersionCommand options = do
  let localVersion = T.pack (showVersion version)
  controller <- resolveController options
  serverVersionResult <-
    liftIO $
      runExceptT $
        withExceptT BoxctlApiError (fetchVersion controller)
  case serverVersionResult of
    Right versionInfo -> do
      liftIO $
        emitOutput
        (optOutputMode options)
        (renderVersion localVersion (Just versionInfo))
        (object ["boxctlVersion" .= localVersion, "server" .= versionInfo])
    Left err -> do
      when (optVerbose options) $
        liftIO $
          TIO.hPutStrLn stderr ("boxctl: server version unavailable: " <> renderBoxctlError err)
      liftIO $
        emitOutput
        (optOutputMode options)
        (renderVersion localVersion Nothing)
        (object ["boxctlVersion" .= localVersion])

runCommand :: RenderStyle -> OutputMode -> Controller -> Command -> ExceptT BoxctlError IO ()
runCommand renderStyle outputMode controller command =
  case command of
    CmdVersion ->
      pure ()
    CmdMode -> do
      config <- withExceptT BoxctlApiError (fetchConfigs controller)
      liftIO $
        emitOutput outputMode (renderMode config) (toJSON config)
    CmdSwitch newMode -> do
      supported <- withExceptT BoxctlApiError (fetchConfigs controller)
      unless (null (configModeList supported) || newMode `elem` configModeList supported) $
        throwE (BoxctlCommandError (UnsupportedMode (clashModeText newMode)))
      withExceptT BoxctlApiError (switchMode controller newMode)
      config <- withExceptT BoxctlApiError (fetchConfigs controller)
      liftIO $
        emitOutput outputMode (renderSwitch config) (toJSON config)
    CmdList listOptions -> do
      proxies <- withExceptT BoxctlApiError (fetchProxies controller)
      liftIO $
        emitOutput
          outputMode
          (renderList renderStyle listOptions proxies)
          (object ["proxies" .= proxies])
    CmdShow selection -> do
      allProxies <- withExceptT BoxctlApiError (fetchProxies controller)
      proxies <- exceptCommand (selectProxiesFrom selection allProxies)
      liftIO $
        emitOutput
          outputMode
          (renderProxyDetails renderStyle (proxyIndex allProxies) proxies)
          (object ["proxies" .= proxies])
    CmdTest selection -> do
      allProxies <- withExceptT BoxctlApiError (fetchProxies controller)
      proxies <- exceptCommand (selectProxiesFrom selection allProxies)
      tested <- liftIO (mapConcurrently (runExceptT . testProxy controller) proxies)
      let (errs, results) = partitionEithers tested
          renderedErrors = map renderBoxctlError errs
          payload = object ["results" .= results, "errors" .= renderedErrors]
      case outputMode of
        OutputHuman ->
          liftIO (mapM_ (TIO.hPutStrLn stderr) renderedErrors)
        OutputJson ->
          pure ()
      if null results
        then do
          case outputMode of
            OutputJson -> liftIO (emitJson payload)
            OutputHuman -> pure ()
          throwE (BoxctlCommandError NoDelayResults)
        else do
          liftIO (emitOutput outputMode (renderTestResults renderStyle results) payload)
          unless (null errs) $
            throwE (BoxctlCommandError DelayTestsFailed)
    CmdSelect selectCommand -> do
      proxies <- withExceptT BoxctlApiError (fetchProxies controller)
      (selectorProxy, selectorOption) <- exceptCommand (resolveSelector proxies selectCommand)
      withExceptT BoxctlApiError (selectProxyOption controller (proxyName selectorProxy) selectorOption)
      liftIO $
        emitOutput
          outputMode
          (renderSelect (proxyName selectorProxy) selectorOption)
          ( object
              [ "selector" .= proxyName selectorProxy,
                "selected" .= selectorOption
              ]
          )

exceptCommand :: Either CommandError a -> ExceptT BoxctlError IO a
exceptCommand =
  either (throwE . BoxctlCommandError) pure

selectProxiesFrom :: ProxySelection -> [ProxyInfo] -> Either CommandError [ProxyInfo]
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

resolveSelector :: [ProxyInfo] -> SelectCommand -> Either CommandError (ProxyInfo, Text)
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

testProxy :: Controller -> ProxyInfo -> ExceptT BoxctlError IO TestResult
testProxy controller proxy
  | isGroupProxy proxy = do
      groupDelayResult <- liftIO (runExceptT (fetchGroupDelay controller (proxyName proxy) 15000))
      case groupDelayResult of
        Right delays ->
          pure (GroupDelay proxy (materializeGroupDelays proxy delays))
        Left apiErr
          | Just label <- apiDelayFailureLabel apiErr ->
              pure (GroupDelay proxy (materializeGroupError proxy label))
          | otherwise ->
              throwE (BoxctlCommandError (DelayProbeFailed (proxyName proxy) apiErr))
  | otherwise = do
      delayResult <- liftIO (runExceptT (fetchProxyDelay controller (proxyName proxy) 15000))
      case delayResult of
        Right delay ->
          pure (ProxyDelay proxy (DelayOk (delayValue delay)))
        Left apiErr
          | Just label <- apiDelayFailureLabel apiErr ->
              pure (ProxyDelay proxy (DelayUnavailable label))
          | otherwise ->
              throwE (BoxctlCommandError (DelayProbeFailed (proxyName proxy) apiErr))

materializeGroupDelays :: ProxyInfo -> Map Text Int -> [(Text, DelayStatus)]
materializeGroupDelays proxy measured =
  map renderMember orderedMembers
  where
    orderedMembers =
      case proxyMembers proxy of
        [] -> Map.keys measured
        members -> members
    renderMember memberName =
      case Map.lookup memberName measured of
        Just delayMs -> (memberName, DelayOk delayMs)
        Nothing -> (memberName, DelayUnavailable "unavailable")

materializeGroupError :: ProxyInfo -> Text -> [(Text, DelayStatus)]
materializeGroupError proxy label =
  map (\memberName -> (memberName, DelayUnavailable label)) orderedMembers
  where
    orderedMembers = proxyMembers proxy

proxyIndex :: [ProxyInfo] -> Map Text ProxyInfo
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

resolveProxyByName :: [ProxyInfo] -> Text -> Either CommandError ProxyInfo
resolveProxyByName proxies requestedName =
  case findByName proxyName proxies requestedName of
    MissingName ->
      Left (OutboundNotFound requestedName)
    UniqueName proxy ->
      Right proxy
    AmbiguousName matches ->
      Left (AmbiguousOutbound requestedName (map proxyName matches))

resolveSelectorByName :: [ProxyInfo] -> Text -> Either CommandError ProxyInfo
resolveSelectorByName selectors requestedName =
  case findByName proxyName selectors requestedName of
    MissingName ->
      Left (SelectorNotFound requestedName)
    UniqueName selectorProxy ->
      Right selectorProxy
    AmbiguousName matches ->
      Left (AmbiguousSelector requestedName (map proxyName matches))

resolveSelectorOption :: ProxyInfo -> Text -> Either CommandError Text
resolveSelectorOption selectorProxy requestedOption =
  case findByName id members requestedOption of
    MissingName ->
      Left (OptionNotFound (proxyName selectorProxy) requestedOption)
    UniqueName selectedOption ->
      Right selectedOption
    AmbiguousName matches ->
      Left (AmbiguousOption (proxyName selectorProxy) requestedOption matches)
  where
    members = proxyMembers selectorProxy

filterMatches :: ProxyFilter -> ProxyInfo -> Bool
filterMatches FilterAll _ = True
filterMatches FilterSelectors proxy = isSelectorProxy proxy
filterMatches FilterUrlTests proxy = isUrlTestProxy proxy

textEqualsFold :: Text -> Text -> Bool
textEqualsFold left right = T.toCaseFold left == T.toCaseFold right

emitOutput :: OutputMode -> Text -> Value -> IO ()
emitOutput outputMode humanOutput jsonOutput =
  case outputMode of
    OutputHuman -> emit humanOutput
    OutputJson -> emitJson jsonOutput

emitJson :: Value -> IO ()
emitJson = BL8.putStrLn . encode

emit :: Text -> IO ()
emit = TIO.putStrLn . T.dropWhileEnd (== '\n')
