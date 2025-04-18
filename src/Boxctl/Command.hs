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
      runCommand controller (optCommand options)

runCommand :: Controller -> Command -> IO (Either Text ())
runCommand controller command =
  case command of
    CmdVersion -> do
      versionResult <- fetchVersion controller
      case versionResult of
        Left err ->
          pure (Left err)
        Right versionInfo -> do
          emit (renderVersion (T.pack (showVersion version)) versionInfo)
          pure (Right ())
    CmdMode -> do
      configResult <- fetchConfigs controller
      case configResult of
        Left err ->
          pure (Left err)
        Right config -> do
          emit (renderMode config)
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
                      emit (renderSwitch config)
                      pure (Right ())
    CmdList -> do
      proxyResult <- fetchProxies controller
      case proxyResult of
        Left err ->
          pure (Left err)
        Right proxies -> do
          emit (renderList proxies)
          pure (Right ())
    CmdShow selection -> do
      proxyResult <- selectProxies controller selection
      case proxyResult of
        Left err ->
          pure (Left err)
        Right proxies -> do
          emit (renderProxyDetails proxies)
          pure (Right ())
    CmdTest selection -> do
      proxyResult <- selectProxies controller selection
      case proxyResult of
        Left err ->
          pure (Left err)
        Right proxies -> do
          tested <- mapConcurrently (testProxy controller) proxies
          let (errs, results) = partitionEithers tested
          mapM_ (TIO.hPutStrLn stderr) errs
          if null results
            then pure (Left "no delay results were produced")
            else do
              emit (renderTestResults results)
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
              emit (renderSelect (proxyName selectorProxy) selectorOption)
              pure (Right ())

selectProxies :: Controller -> ProxySelection -> IO (Either Text [ProxyInfo])
selectProxies controller selection = do
  allProxyResult <- fetchProxies controller
  pure $ do
    allProxies <- allProxyResult
    let proxyMap = Map.fromList [(proxyName proxy, proxy) | proxy <- allProxies]
        requestedNames = selectionNames selection
        selectedByName =
          if null requestedNames
            then Right (sortOn (T.toCaseFold . proxyName) allProxies)
            else traverse (lookupProxy proxyMap) requestedNames
    proxies <- selectedByName
    filtered <- traverse ensureMatch proxies
    if null filtered
      then Left "no matching outbounds"
      else Right filtered
  where
    lookupProxy proxyMap proxyName =
      maybe (Left ("outbound not found: " <> proxyName)) Right (Map.lookup proxyName proxyMap)

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

emit :: Text -> IO ()
emit = TIO.putStrLn . T.dropWhileEnd (== '\n')
