{-# LANGUAGE OverloadedStrings #-}

module Boxctl.Command
  ( run,
  )
where

import Boxctl.API
import Boxctl.CLI
import Boxctl.Instance (resolveInstance)
import Boxctl.Output
import Control.Applicative ((<|>))
import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (bracket)
import Control.Monad (unless)
import Data.Aeson (Value, encode, object, toJSON, (.=))
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Char (digitToInt, isHexDigit, toLower)
import Data.Either (partitionEithers)
import Data.List (sortOn, stripPrefix)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Version (showVersion)
import Paths_boxctl (version)
import System.Environment (lookupEnv)
import System.IO.Error (catchIOError)
import System.IO (hIsTerminalDevice, stderr, stdout)
import System.Posix.IO (OpenMode (ReadWrite), closeFd, defaultFileFlags, fdWrite, openFd)
import qualified System.Posix.IO.ByteString as PosixBS
import System.Posix.Terminal
  ( TerminalAttributes,
    TerminalState (Immediately),
    TerminalMode (EnableEcho, ExtendedFunctions, KeyboardInterrupts, ProcessInput),
    getControllingTerminalName,
    getTerminalAttributes,
    queryTerminal,
    setTerminalAttributes,
    withMinInput,
    withTime,
    withoutMode
  )
import System.Posix.Types (Fd)

run :: Options -> IO (Either Text ())
run options = do
  resolvedInstanceResult <- resolveInstance (optInstance options)
  case resolvedInstanceResult of
    Left err ->
      pure (Left err)
    Right resolvedInstance -> do
      controller <- mkController resolvedInstance (optVerbose options)
      renderStyle <- resolveRenderStyle options
      runCommand renderStyle (optOutputMode options) controller (optCommand options)

runCommand :: RenderStyle -> OutputMode -> Controller -> Command -> IO (Either Text ())
runCommand renderStyle outputMode controller command =
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
            (renderList renderStyle listOptions proxies)
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
                (renderProxyDetails renderStyle (proxyIndex allProxies) proxies)
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
                  emitOutput outputMode (renderTestResults renderStyle results) payload
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

resolveRenderStyle :: Options -> IO RenderStyle
resolveRenderStyle options = do
  colorEnabled <- resolveColorEnabled options
  backgroundTone <-
    if colorEnabled
      then resolveBackgroundTone
      else pure BackgroundDark
  pure
    RenderStyle
      { renderUseColor = colorEnabled,
        renderBackgroundTone = backgroundTone
      }

resolveColorEnabled :: Options -> IO Bool
resolveColorEnabled options = do
  forcedColor <- lookupEnv "CLICOLOR_FORCE"
  noColor <- lookupEnv "NO_COLOR"
  clicolor <- lookupEnv "CLICOLOR"
  term <- lookupEnv "TERM"
  isTerminal <- hIsTerminalDevice stdout
  pure $
    case optColorMode options of
      ColorNever -> False
      ColorAlways -> optOutputMode options == OutputHuman
      ColorAuto
        | optOutputMode options == OutputJson -> False
        | envTruthy forcedColor -> True
        | maybe False (const True) noColor -> False
        | clicolor == Just "0" -> False
        | term == Just "dumb" -> False
        | otherwise -> isTerminal

resolveBackgroundTone :: IO BackgroundTone
resolveBackgroundTone = do
  explicitBackground <- lookupEnv "BOXCTL_BACKGROUND"
  case explicitBackground >>= parseBackgroundTone . T.pack of
    Just tone -> pure tone
    Nothing -> fromMaybe BackgroundDark <$> detectBackgroundTone

detectBackgroundTone :: IO (Maybe BackgroundTone)
detectBackgroundTone = do
  osc11Tone <- queryOsc11BackgroundTone
  colorFgBg <- lookupEnv "COLORFGBG"
  pure (osc11Tone <|> (colorFgBg >>= colorFgBgTone))

queryOsc11BackgroundTone :: IO (Maybe BackgroundTone)
queryOsc11BackgroundTone =
  catchIOError go (const (pure Nothing))
  where
    go = do
      ttyPath <- getControllingTerminalName
      bracket (openFd ttyPath ReadWrite defaultFileFlags) closeFd $ \ttyFd -> do
        isTerminal <- queryTerminal ttyFd
        if not isTerminal
          then pure Nothing
          else withOsc11InputMode ttyFd $ do
            writeAll ttyFd osc11Query
            response <- readOsc11Response ttyFd
            pure (response >>= osc11ResponseTone)

withOsc11InputMode :: Fd -> IO a -> IO a
withOsc11InputMode ttyFd action =
  bracket acquire restore (const action)
  where
    acquire = do
      original <- getTerminalAttributes ttyFd
      setTerminalAttributes ttyFd (osc11InputAttributes original) Immediately
      pure original
    restore original = setTerminalAttributes ttyFd original Immediately

osc11InputAttributes :: TerminalAttributes -> TerminalAttributes
osc11InputAttributes original =
  withTime
    ( withMinInput
        (foldl' withoutMode original [ProcessInput, EnableEcho, KeyboardInterrupts, ExtendedFunctions])
        0
    )
    1

osc11Query :: String
osc11Query = "\ESC]11;?\a"

readOsc11Response :: Fd -> IO (Maybe String)
readOsc11Response ttyFd = go []
  where
    go chunks
      | responseLength >= maxOsc11ResponseLength =
          pure (Just (concat (reverse chunks)))
      | otherwise = do
          chunk <- PosixBS.fdRead ttyFd 64
          if BS8.null chunk
            then finish chunks
            else
              let chunks' = BS8.unpack chunk : chunks
                  response = concat (reverse chunks')
               in
                if osc11ResponseComplete response
                  then pure (Just response)
                  else go chunks'
      where
        responseLength = sum (map length chunks)

    finish [] = pure Nothing
    finish chunks = pure (Just (concat (reverse chunks)))

writeAll :: Fd -> String -> IO ()
writeAll _ [] = pure ()
writeAll ttyFd bytes = do
  written <- fdWrite ttyFd bytes
  unless (written <= 0) (writeAll ttyFd (drop (fromIntegral written) bytes))

maxOsc11ResponseLength :: Int
maxOsc11ResponseLength = 256

osc11ResponseComplete :: String -> Bool
osc11ResponseComplete rawValue =
  '\a' `elem` rawValue || "\ESC\\" `contains` rawValue

contains :: Eq a => [a] -> [a] -> Bool
contains needle haystack =
  any (maybe False (const True) . stripPrefix needle) (tails haystack)
  where
    tails [] = [[]]
    tails xs@(_ : rest) = xs : tails rest

osc11ResponseTone :: String -> Maybe BackgroundTone
osc11ResponseTone rawValue = do
  payload <- osc11Payload rawValue
  rgb <- parseOsc11Rgb payload
  pure (backgroundToneFromRgb rgb)

osc11Payload :: String -> Maybe String
osc11Payload rawValue = do
  prefixed <- stripAfter "\ESC]11;" rawValue
  takeUntilOscTerminator prefixed

stripAfter :: Eq a => [a] -> [a] -> Maybe [a]
stripAfter prefix =
  go
  where
    go input =
      case stripPrefix prefix input of
        Just rest -> Just rest
        Nothing ->
          case input of
            [] -> Nothing
            _ : rest -> go rest

takeUntilOscTerminator :: String -> Maybe String
takeUntilOscTerminator =
  go []
  where
    go _ [] = Nothing
    go acc ('\a' : _) = Just (reverse acc)
    go acc ('\ESC' : '\\' : _) = Just (reverse acc)
    go acc (char : rest) = go (char : acc) rest

parseOsc11Rgb :: String -> Maybe (Double, Double, Double)
parseOsc11Rgb rawValue =
  case stripPrefix "rgb:" normalized <|> stripPrefix "rgba:" normalized of
    Just payload ->
      case splitOn '/' payload of
        red : green : blue : _ ->
          (,,) <$> parseRgbChannel red <*> parseRgbChannel green <*> parseRgbChannel blue
        _ -> Nothing
    Nothing ->
      case stripPrefix "#" normalized of
        Just hexValue -> parseHexRgb hexValue
        Nothing -> Nothing
  where
    normalized = map toLower rawValue

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn separator =
  foldr step [[]]
  where
    step char acc@(current : rest)
      | char == separator = [] : acc
      | otherwise = (char : current) : rest
    step _ [] = []

parseHexRgb :: String -> Maybe (Double, Double, Double)
parseHexRgb hexValue =
  case length hexValue `divMod` 3 of
    (digitsPerChannel, 0)
      | digitsPerChannel > 0 ->
          let (redDigits, rest) = splitAt digitsPerChannel hexValue
              (greenDigits, blueDigits) = splitAt digitsPerChannel rest
           in
            (,,) <$> parseRgbChannel redDigits <*> parseRgbChannel greenDigits <*> parseRgbChannel blueDigits
    _ -> Nothing

parseRgbChannel :: String -> Maybe Double
parseRgbChannel digits
  | null digits = Nothing
  | any (not . isHexDigit) digits = Nothing
  | otherwise =
      let value = fromIntegral (foldl' step 0 digits) :: Double
          maxValue = fromIntegral (((16 :: Int) ^ length digits) - 1) :: Double
       in
        if maxValue <= 0
          then Nothing
          else Just (value / maxValue)
  where
    step acc digit = acc * 16 + digitToInt digit

backgroundToneFromRgb :: (Double, Double, Double) -> BackgroundTone
backgroundToneFromRgb (red, green, blue)
  | luminance >= 0.5 = BackgroundLight
  | otherwise = BackgroundDark
  where
    luminance = 0.2126 * red + 0.7152 * green + 0.0722 * blue

parseBackgroundTone :: Text -> Maybe BackgroundTone
parseBackgroundTone rawValue =
  case T.toCaseFold (T.strip rawValue) of
    "dark" -> Just BackgroundDark
    "light" -> Just BackgroundLight
    _ -> Nothing

colorFgBgTone :: String -> Maybe BackgroundTone
colorFgBgTone rawValue =
  case reverse (splitColorFgBg rawValue) of
    lastToken : _ ->
      case reads lastToken of
        [(code :: Int, "")]
          | code >= 7 -> Just BackgroundLight
          | otherwise -> Just BackgroundDark
        _ -> Nothing
    [] -> Nothing

splitColorFgBg :: String -> [String]
splitColorFgBg rawValue =
  foldr step [[]] rawValue
  where
    step char acc@(current : rest)
      | char == ';' || char == ':' = [] : acc
      | otherwise = (char : current) : rest
    step _ [] = []

envTruthy :: Maybe String -> Bool
envTruthy =
  maybe False (\value -> value /= "" && value /= "0")
