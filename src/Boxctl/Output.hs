{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Boxctl.Output
  ( DelayStatus (..),
    TestResult (..),
    renderList,
    renderMode,
    renderProxyDetails,
    renderSelect,
    renderSwitch,
    renderTestResults,
    renderVersion,
  )
where

import Boxctl.API
import Boxctl.CLI (ListOptions (..))
import Data.Aeson (ToJSON (..), object, (.=))
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

data DelayStatus
  = DelayOk Int
  | DelayUnavailable Text
  deriving (Eq, Show)

instance ToJSON DelayStatus where
  toJSON = \case
    DelayOk delayMs ->
      object
        [ "status" .= ("ok" :: Text),
          "delay" .= delayMs
        ]
    DelayUnavailable reason ->
      object
        [ "status" .= ("unavailable" :: Text),
          "reason" .= reason
        ]

data TestResult
  = ProxyDelay ProxyInfo DelayStatus
  | GroupDelay ProxyInfo [(Text, DelayStatus)]
  deriving (Eq, Show)

instance ToJSON TestResult where
  toJSON = \case
    ProxyDelay proxy status ->
      object
        [ "kind" .= ("proxy" :: Text),
          "proxy" .= proxy,
          "result" .= status
        ]
    GroupDelay proxy delays ->
      object
        [ "kind" .= ("group" :: Text),
          "proxy" .= proxy,
          "results"
            .= map
              (\(name, status) -> object ["name" .= name, "result" .= status])
              delays
        ]

renderVersion :: Text -> VersionResponse -> Text
renderVersion localVersion versionInfo =
  T.intercalate
    "  "
    [ "boxctl " <> localVersion,
      "server=" <> renderVersionLabel versionInfo
    ]

renderMode :: ConfigResponse -> Text
renderMode config =
  T.intercalate
    "  "
    (["mode: " <> configMode config] <> availableModeLines config)

renderSwitch :: ConfigResponse -> Text
renderSwitch config = "mode: " <> configMode config

renderSelect :: Text -> Text -> Text
renderSelect selectorName optionName =
  selectorName <> ": " <> optionName

renderList :: ListOptions -> [ProxyInfo] -> Text
renderList listOptions proxies =
  T.intercalate "\n\n" (catMaybes [groupsSection, nodesSection, hiddenNodesNote])
  where
    groups = sortOn groupSortKey (filter isGroupProxy proxies)
    nodes = sortOn proxyNameKey (filter (not . isGroupProxy) proxies)
    groupsSection =
      Just $
        renderSection
          "Groups"
          ( if null groups
              then ["none"]
              else renderGroupRows groups
          )
    nodesSection
      | listIncludeNodes listOptions =
          Just $
            renderSection
              "Nodes"
              ( if null nodes
                  then ["none"]
                  else renderNodeRows nodes
              )
      | otherwise = Nothing
    hiddenNodesNote
      | listIncludeNodes listOptions || null nodes = Nothing
      | otherwise =
          Just
            ( T.pack (show (length nodes))
                <> " node outbounds hidden; run: boxctl list --all"
            )

renderProxyDetails :: Map Text ProxyInfo -> [ProxyInfo] -> Text
renderProxyDetails proxyIndex proxies =
  T.intercalate "\n\n" (map renderProxy (sortOn proxyDetailSortKey proxies))
  where
    renderProxy proxy
      | isGroupProxy proxy = renderGroupDetails proxyIndex proxy
      | otherwise = renderLeafDetails proxy

renderGroupDetails :: Map Text ProxyInfo -> ProxyInfo -> Text
renderGroupDetails proxyIndex proxy =
  T.unlines $
    headerLines
      <> detailsLines
      <> [""]
      <> ["members:"]
      <> memberLines
  where
    headerLines =
      [ proxyName proxy <> "  " <> proxyTypeLabel proxy
      ]
    detailsLines =
      [ "current: " <> fromMaybe "-" (proxyNow proxy),
        "options: " <> T.pack (show (length members))
      ]
        <> maybe [] (\history -> ["last: " <> historySummary history]) (latestHistory proxy)
    members = fromMaybe [] (proxyAll proxy)
    memberLines =
      if null members
        then ["  none"]
        else renderMemberRows proxyIndex (proxyNow proxy) members

renderLeafDetails :: ProxyInfo -> Text
renderLeafDetails proxy =
  T.intercalate
    "  "
    ( [ proxyName proxy,
        proxyTypeLabel proxy,
        "last=" <> maybe "unknown" historySummary (latestHistory proxy)
      ]
        <> ["udp=no" | not (proxyUdp proxy)]
    )

renderTestResults :: [TestResult] -> Text
renderTestResults =
  T.intercalate "\n\n"
    . map renderResult
    . sortOn testResultSortKey
  where
    renderResult (ProxyDelay proxy status) =
      T.intercalate
        "  "
        [ proxyName proxy,
          proxyTypeLabel proxy,
          "result=" <> renderDelayStatus status
        ]
    renderResult (GroupDelay proxy delays) =
      T.unlines $
        [ proxyName proxy <> "  " <> proxyTypeLabel proxy <> currentSuffix (proxyNow proxy),
          ""
        ]
          <> renderDelayRows (proxyNow proxy) delays

renderDelayStatus :: DelayStatus -> Text
renderDelayStatus = \case
  DelayOk delayMs -> T.pack (show delayMs) <> "ms"
  DelayUnavailable reason -> reason

renderVersionLabel :: VersionResponse -> Text
renderVersionLabel versionInfo =
  case versionFlags of
    [] -> versionText versionInfo
    flags ->
      versionText versionInfo
        <> " ("
        <> T.intercalate ", " flags
        <> ")"
  where
    versionFlags =
      [ "premium" | versionPremium versionInfo
      ]
        <> [ "meta" | versionMeta versionInfo
           ]

availableModeLines :: ConfigResponse -> [Text]
availableModeLines config =
  case dedupeModes (configModeList config) of
    [] -> []
    [onlyMode]
      | equalsFold onlyMode (configMode config) -> []
    modes -> ["available=" <> T.intercalate ", " modes]

dedupeModes :: [Text] -> [Text]
dedupeModes = foldl insertMode []
  where
    insertMode acc mode
      | any (`equalsFold` mode) acc = acc
      | otherwise = acc <> [mode]

renderGroupRows :: [ProxyInfo] -> [Text]
renderGroupRows proxies =
  map renderRow proxies
  where
    nameWidth = columnWidth 8 (map (T.length . proxyName) proxies)
    kindWidth = columnWidth 8 (map (T.length . proxyTypeLabel) proxies)
    renderRow proxy =
      pad nameWidth (proxyName proxy)
        <> "  "
        <> pad kindWidth (proxyTypeLabel proxy)
        <> "  "
        <> T.intercalate "  " (groupSummaryParts proxy)

renderNodeRows :: [ProxyInfo] -> [Text]
renderNodeRows proxies =
  map renderRow proxies
  where
    nameWidth = columnWidth 8 (map (T.length . proxyName) proxies)
    kindWidth = columnWidth 8 (map (T.length . proxyTypeLabel) proxies)
    renderRow proxy =
      pad nameWidth (proxyName proxy)
        <> "  "
        <> pad kindWidth (proxyTypeLabel proxy)
        <> "  "
        <> nodeSummary proxy

renderMemberRows :: Map Text ProxyInfo -> Maybe Text -> [Text] -> [Text]
renderMemberRows proxyIndex current members =
  map renderRow memberProxies
  where
    orderedMembers = sortOn (memberSortKey current proxyIndex) members
    memberProxies =
      map (\memberName -> (memberName, Map.lookup memberName proxyIndex)) orderedMembers
    nameWidth = columnWidth 8 (map (T.length . fst) memberProxies)
    kindWidth =
      columnWidth
        8
        ( map
            ( maybe 7 (T.length . proxyTypeLabel)
                . snd
            )
            memberProxies
        )
    renderRow (memberName, maybeProxy) =
      currentMarker current memberName
        <> " "
        <> pad nameWidth memberName
        <> "  "
        <> pad kindWidth (maybe "unknown" proxyTypeLabel maybeProxy)
        <> "  "
        <> maybe "unknown" memberSummary maybeProxy

memberSortKey :: Maybe Text -> Map Text ProxyInfo -> Text -> (Int, Int, Text)
memberSortKey current proxyIndex memberName =
  ( currentRank current memberName,
    maybe maxBound historyDelay (Map.lookup memberName proxyIndex >>= latestHistory),
    T.toCaseFold memberName
  )

renderDelayRows :: Maybe Text -> [(Text, DelayStatus)] -> [Text]
renderDelayRows current delays =
  map renderDelayRow ordered
  where
    ordered = sortOn (delaySortKey current) delays
    nameWidth = columnWidth 8 (map (T.length . fst) ordered)
    renderDelayRow (memberName, status) =
      currentMarker current memberName
        <> " "
        <> pad nameWidth memberName
        <> "  "
        <> renderDelayStatus status

delaySortKey :: Maybe Text -> (Text, DelayStatus) -> (Int, Int, Text)
delaySortKey current (memberName, status) =
  ( currentRank current memberName,
    delayRank status,
    T.toCaseFold memberName
  )

delayRank :: DelayStatus -> Int
delayRank = \case
  DelayOk delayMs -> delayMs
  DelayUnavailable _ -> maxBound

testResultSortKey :: TestResult -> (Int, Text)
testResultSortKey = \case
  GroupDelay proxy _ -> (0, proxyNameKey proxy)
  ProxyDelay proxy _ -> (1, proxyNameKey proxy)

proxyDetailSortKey :: ProxyInfo -> (Int, Text, Text)
proxyDetailSortKey proxy =
  ( groupKindRank proxy,
    proxyNameKey proxy,
    proxyTypeLabel proxy
  )

groupSortKey :: ProxyInfo -> (Int, Text)
groupSortKey proxy =
  ( groupKindRank proxy,
    proxyNameKey proxy
  )

groupKindRank :: ProxyInfo -> Int
groupKindRank proxy
  | isSelectorProxy proxy = 0
  | isUrlTestProxy proxy = 1
  | otherwise = 2

groupSummaryParts :: ProxyInfo -> [Text]
groupSummaryParts proxy =
  catMaybes
    [ fmap ("current=" <>) (proxyNow proxy),
      fmap (\members -> T.pack (show (length members)) <> " options") (proxyAll proxy),
      fmap (\history -> "last=" <> T.pack (show (historyDelay history)) <> "ms") (latestHistory proxy)
    ]

nodeSummary :: ProxyInfo -> Text
nodeSummary proxy =
  T.intercalate
    "  "
    ( [ maybe "unknown" (\history -> T.pack (show (historyDelay history)) <> "ms") (latestHistory proxy)
      ]
        <> ["udp=no" | not (proxyUdp proxy)]
    )

memberSummary :: ProxyInfo -> Text
memberSummary proxy =
  T.intercalate
    "  "
    ( [ maybe "unknown" (\history -> T.pack (show (historyDelay history)) <> "ms") (latestHistory proxy)
      ]
        <> maybe [] (\history -> [formatTimestamp (historyTime history)]) (latestHistory proxy)
    )

latestHistory :: ProxyInfo -> Maybe DelayHistory
latestHistory proxy =
  listToMaybe (sortOn (Down . historyTime) (proxyHistory proxy))

historySummary :: DelayHistory -> Text
historySummary delayHistory =
  T.pack (show (historyDelay delayHistory))
    <> "ms at "
    <> formatTimestamp (historyTime delayHistory)

formatTimestamp :: UTCTime -> Text
formatTimestamp =
  T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%MZ"

renderSection :: Text -> [Text] -> Text
renderSection title rows =
  T.intercalate "\n" (title : map ("  " <>) rows)

proxyTypeLabel :: ProxyInfo -> Text
proxyTypeLabel proxy
  | isUrlTestProxy proxy = "url-test"
  | isSelectorProxy proxy = "selector"
  | otherwise = T.toLower (proxyType proxy)

proxyNameKey :: ProxyInfo -> Text
proxyNameKey = T.toCaseFold . proxyName

isGroupProxy :: ProxyInfo -> Bool
isGroupProxy proxy = maybe False (const True) (proxyAll proxy)

isSelectorProxy :: ProxyInfo -> Bool
isSelectorProxy proxy = equalsFold (proxyType proxy) "Selector"

isUrlTestProxy :: ProxyInfo -> Bool
isUrlTestProxy proxy = equalsFold (proxyType proxy) "URLTest"

currentMarker :: Maybe Text -> Text -> Text
currentMarker current memberName
  | maybe False (`equalsFold` memberName) current = "*"
  | otherwise = "-"

currentRank :: Maybe Text -> Text -> Int
currentRank current memberName
  | maybe False (`equalsFold` memberName) current = 0
  | otherwise = 1

currentSuffix :: Maybe Text -> Text
currentSuffix =
  maybe "" (\current -> "  current=" <> current)

equalsFold :: Text -> Text -> Bool
equalsFold left right = T.toCaseFold left == T.toCaseFold right

pad :: Int -> Text -> Text
pad width text = text <> T.replicate (max 0 (width - T.length text)) " "

columnWidth :: Int -> [Int] -> Int
columnWidth minimumWidth values =
  max minimumWidth $
    case values of
      [] -> minimumWidth
      _ -> maximum values
