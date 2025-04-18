{-# LANGUAGE OverloadedStrings #-}

module Boxctl.Command
  ( run,
  )
where

import Boxctl.API
import Boxctl.CLI
import Boxctl.Instance (resolveInstance)
import Boxctl.Output
import Control.Concurrent.Async (mapConcurrently)
import Data.Aeson (Value, encode, object, toJSON, (.=))
import qualified Data.ByteString.Lazy.Char8 as BL8
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
import System.IO (stderr)

run :: Options -> IO (Either Text ())
run options = do
  resolvedInstanceResult <- resolveInstance (optInstance options)
  case resolvedInstanceResult of
    Left err ->
      pure (Left err)
    Right resolvedInstance -> do
      controller <- mkController resolvedInstance (optVerbose options)
      runCommand (optOutputMode options) controller (optCommand options)

runCommand :: OutputMode -> Controller -> Command -> IO (Either Text ())
runCommand outputMode controller command =
  case command of
    CmdVersion -> do
      versionResult <- fetchVersion controller
      case versionResult of
        Left err ->
          pure (Left err)
        Right versionInfo -> do
          let localVersion = T.pack (showVersion version)
          emitOutput
            outputMode
            (renderVersion localVersion versionInfo)
            (object ["boxctlVersion" .= localVersion, "server" .= versionInfo])
          pure (Right ())
    CmdMode -> do
      configResult <- fetchConfigs controller
      case configResult of
        Left err ->
          pure (Left err)
        Right config -> do
          emitOutput outputMode (renderMode config) (toJSON config)
          pure (Right ())
    CmdSwitch newMode -> do
      supportedModesResult <- fetchConfigs controller
      case supportedModesResult of
        Left err -> pure (Left err)
        Right supported
          | not (null (configModeList supported)) && not (matchesAny (configModeList supported) newMode) ->
              pure (Left ("mode not supported by server: " <> newMode))
          | otherwise -> do
              switchResult <- switchMode controller newMode
              case switchResult of
                Left err ->
                  pure (Left err)
                Right () -> do
                  configResult <- fetchConfigs controller
                  case configResult of
                    Left err ->
                      pure (Left err)
                    Right config -> do
                      emitOutput outputMode (renderSwitch config) (toJSON config)
                      pure (Right ())
    CmdList listOptions -> do
      proxyResult <- fetchProxies controller
      case proxyResult of
        Left err ->
          pure (Left err)
        Right proxies -> do
          emitOutput
            outputMode
            (renderList listOptions proxies)
            (object ["proxies" .= proxies])
          pure (Right ())
    CmdShow selection -> do
      allProxyResult <- fetchProxies controller
      case allProxyResult of
        Left err ->
          pure (Left err)
        Right allProxies ->
          case selectProxiesFrom selection allProxies of
            Left err ->
              pure (Left err)
            Right proxies -> do
              emitOutput
                outputMode
                (renderProxyDetails (proxyIndex allProxies) proxies)
                (object ["proxies" .= proxies])
              pure (Right ())
    CmdTest selection -> do
      allProxyResult <- fetchProxies controller
      case allProxyResult of
        Left err ->
          pure (Left err)
        Right allProxies ->
          case selectProxiesFrom selection allProxies of
            Left err ->
              pure (Left err)
            Right proxies -> do
              tested <- mapConcurrently (testProxy controller) proxies
              let (errs, results) = partitionEithers tested
                  payload = object ["results" .= results, "errors" .= errs]
              case outputMode of
                OutputHuman ->
                  mapM_ (TIO.hPutStrLn stderr) errs
                OutputJson ->
                  pure ()
              if null results
                then do
                  case outputMode of
                    OutputJson -> emitJson payload
                    OutputHuman -> pure ()
                  pure (Left "no delay results were produced")
                else do
                  emitOutput outputMode (renderTestResults results) payload
                  pure $
                    if null errs
                      then Right ()
                      else Left "one or more delay tests failed"
    CmdSelect args -> do
      selectorResult <- resolveSelector controller args
      case selectorResult of
        Left err ->
          pure (Left err)
        Right (selectorProxy, selectorOption) -> do
          updateResult <- selectProxyOption controller (proxyName selectorProxy) selectorOption
          case updateResult of
            Left err ->
              pure (Left err)
            Right () -> do
              emitOutput
                outputMode
                (renderSelect (proxyName selectorProxy) selectorOption)
                ( object
                    [ "selector" .= proxyName selectorProxy,
                      "selected" .= selectorOption
                    ]
                )
              pure (Right ())

selectProxiesFrom :: ProxySelection -> [ProxyInfo] -> Either Text [ProxyInfo]
selectProxiesFrom selection allProxies =
  if null requestedNames
    then
      if null filteredAll
        then Left "no matching outbounds"
        else Right filteredAll
    else do
      proxies <- traverse (lookupProxy proxyMap) requestedNames
      filtered <- traverse ensureMatch proxies
      if null filtered
        then Left "no matching outbounds"
        else Right filtered
  where
    proxyMap = proxyIndex allProxies
    requestedNames = selectionNames selection
    filteredAll =
      filter
        (filterMatches (selectionFilter selection))
        (sortOn (T.toCaseFold . proxyName) allProxies)

    lookupProxy proxiesByName proxyName =
      maybe (Left ("outbound not found: " <> proxyName)) Right (Map.lookup proxyName proxiesByName)

    ensureMatch proxy
      | filterMatches (selectionFilter selection) proxy = Right proxy
      | otherwise = Left ("outbound does not match filter: " <> proxyName proxy)

resolveSelector :: Controller -> [Text] -> IO (Either Text (ProxyInfo, Text))
resolveSelector controller args = do
  proxyResult <- fetchProxies controller
  pure $ do
    proxies <- proxyResult
    let selectors = filter isSelector proxies
        (selectorName, optionName) =
          case args of
            [singleOption] -> (Nothing, singleOption)
            [namedSelector, namedOption] -> (Just namedSelector, namedOption)
            _ -> (Nothing, "")
    selectorProxy <-
      case selectorName of
        Just requestedName ->
          maybe
            (Left ("selector not found: " <> requestedName))
            Right
            (findProxy selectors requestedName)
        Nothing ->
          case selectors of
            [onlySelector] -> Right onlySelector
            [] -> Left "no selector outbounds found"
            manySelectors ->
              Left
                ( "multiple selectors available, specify one of: "
                    <> T.intercalate ", " (map proxyName manySelectors)
                )
    let members = fromMaybe [] (proxyAll selectorProxy)
    if optionName `elem` members
      then Right (selectorProxy, optionName)
      else
        Left
          ( "option not found in selector "
              <> proxyName selectorProxy
              <> ": "
              <> optionName
          )

testProxy :: Controller -> ProxyInfo -> IO (Either Text TestResult)
testProxy controller proxy
  | isGroup proxy = do
      groupDelayResult <- fetchGroupDelay controller (proxyName proxy) 15000
      pure $
        case groupDelayResult of
          Right delays ->
            Right (GroupDelay proxy (materializeGroupDelays proxy delays))
          Left err
            | isTransientDelayError err ->
                Right (GroupDelay proxy (materializeGroupError proxy (delayFailureLabel err)))
            | otherwise ->
                Left (proxyName proxy <> ": " <> err)
  | otherwise = do
      delayResult <- fetchProxyDelay controller (proxyName proxy) 15000
      pure $
        case delayResult of
          Right delay ->
            Right (ProxyDelay proxy (DelayOk (delayValue delay)))
          Left err
            | isTransientDelayError err ->
                Right (ProxyDelay proxy (DelayUnavailable (delayFailureLabel err)))
            | otherwise ->
                Left (proxyName proxy <> ": " <> err)

materializeGroupDelays :: ProxyInfo -> Map Text Int -> [(Text, DelayStatus)]
materializeGroupDelays proxy measured =
  map renderMember orderedMembers
  where
    orderedMembers = fromMaybe (Map.keys measured) (proxyAll proxy)
    renderMember memberName =
      case Map.lookup memberName measured of
        Just delayMs -> (memberName, DelayOk delayMs)
        Nothing -> (memberName, DelayUnavailable "unavailable")

materializeGroupError :: ProxyInfo -> Text -> [(Text, DelayStatus)]
materializeGroupError proxy label =
  map (\memberName -> (memberName, DelayUnavailable label)) orderedMembers
  where
    orderedMembers = fromMaybe [] (proxyAll proxy)

proxyIndex :: [ProxyInfo] -> Map Text ProxyInfo
proxyIndex proxies =
  Map.fromList [(proxyName proxy, proxy) | proxy <- proxies]

findProxy :: [ProxyInfo] -> Text -> Maybe ProxyInfo
findProxy proxies targetName =
  case filter (\proxy -> proxyName proxy == targetName) proxies of
    [] -> Nothing
    proxy : _ -> Just proxy

isSelector :: ProxyInfo -> Bool
isSelector = typeEquals "Selector"

isUrlTest :: ProxyInfo -> Bool
isUrlTest = typeEquals "URLTest"

isGroup :: ProxyInfo -> Bool
isGroup proxy = maybe False (const True) (proxyAll proxy)

typeEquals :: Text -> ProxyInfo -> Bool
typeEquals expected proxy = T.toCaseFold (proxyType proxy) == T.toCaseFold expected

filterMatches :: ProxyFilter -> ProxyInfo -> Bool
filterMatches FilterAll _ = True
filterMatches FilterSelectors proxy = isSelector proxy
filterMatches FilterUrlTests proxy = isUrlTest proxy
filterMatches FilterConflict _ = False

matchesAny :: [Text] -> Text -> Bool
matchesAny choices value =
  any (\choice -> T.toCaseFold choice == T.toCaseFold value) choices

isTransientDelayError :: Text -> Bool
isTransientDelayError err =
  "HTTP 503" `T.isPrefixOf` err
    || "HTTP 504" `T.isPrefixOf` err

delayFailureLabel :: Text -> Text
delayFailureLabel err
  | "Timeout" `T.isInfixOf` err || "HTTP 504" `T.isPrefixOf` err = "timeout"
  | otherwise = "unavailable"

emitOutput :: OutputMode -> Text -> Value -> IO ()
emitOutput outputMode humanOutput jsonOutput =
  case outputMode of
    OutputHuman -> emit humanOutput
    OutputJson -> emitJson jsonOutput

emitJson :: Value -> IO ()
emitJson = BL8.putStrLn . encode

emit :: Text -> IO ()
emit = TIO.putStrLn . T.dropWhileEnd (== '\n')
