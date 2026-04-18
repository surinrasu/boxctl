{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Boxctl.Output
  ( BackgroundTone (..),
    CommandDiagnostic (..),
    CommandDiagnosticVisibility (..),
    CommandOutput (..),
    DelayStatus (..),
    RenderStyle (..),
    TestResult (..),
    emitCommandOutput,
  )
where

import Boxctl.CLI (ListOptions (..), OutputMode (..))
import Boxctl.Domain
  ( ClashMode,
    Config (..),
    DelayHistory (..),
    GroupDetails (..),
    Proxy,
    ProxyShape (..),
    SsmStats (..),
    SsmUser (..),
    TailscaleEndpointStatus (..),
    TailscaleIPResult (..),
    TailscalePeer (..),
    VersionInfo (..),
    clashModeText,
    isGroupProxy,
    isSelectorProxy,
    isUrlTestProxy,
    proxyCurrent,
    proxyHistory,
    proxyName,
    proxyShape,
    proxyTypeLabel,
    proxyUdp,
  )
import Data.Aeson (ToJSON (..), Value, encode, object, (.=))
import Data.Bits (rotateR, shiftR, xor)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Foldable (traverse_)
import Data.Int (Int64)
import Data.List (sortOn)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Word (Word32)
import System.IO (stderr)

data BackgroundTone
  = BackgroundDark
  | BackgroundLight
  deriving (Eq, Show)

data RenderStyle = RenderStyle
  { renderUseColor :: Bool,
    renderBackgroundTone :: BackgroundTone
  }
  deriving (Eq, Show)

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
  = ProxyDelay Proxy DelayStatus
  | GroupDelay Proxy [(Text, DelayStatus)]
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

data CommandDiagnosticVisibility
  = DiagnosticAlways
  | DiagnosticHumanOnly
  deriving (Eq, Show)

data CommandDiagnostic = CommandDiagnostic
  { diagnosticVisibility :: CommandDiagnosticVisibility,
    diagnosticText :: Text
  }
  deriving (Eq, Show)

data CommandOutput
  = CommandOutputVersion Text (Maybe VersionInfo)
  | CommandOutputMode Config
  | CommandOutputSwitch Config
  | CommandOutputList ListOptions [Proxy]
  | CommandOutputShow (Map Text Proxy) [Proxy]
  | CommandOutputTest [TestResult] [Text]
  | CommandOutputSelect Text Text
  | CommandOutputSsmList Bool [SsmUser]
  | CommandOutputSsmShow [SsmUser] [Text]
  | CommandOutputSsmAdd Text
  | CommandOutputSsmRemove [Text] [Text]
  | CommandOutputSsmUpdate Text
  | CommandOutputSsmStat Bool SsmStats
  | CommandOutputTailscaleStatus [TailscaleEndpointStatus]
  | CommandOutputTailscaleIP TailscaleIPResult
  deriving (Eq, Show)

emitCommandOutput :: OutputMode -> RenderStyle -> [CommandDiagnostic] -> CommandOutput -> IO ()
emitCommandOutput outputMode renderStyle diagnostics commandOutput = do
  traverse_ emitDiagnostic (filter (diagnosticVisible outputMode) diagnostics)
  case outputMode of
    OutputHuman ->
      traverse_ emitText (renderCommandHuman renderStyle commandOutput)
    OutputJson ->
      emitJson (renderCommandJson commandOutput)

diagnosticVisible :: OutputMode -> CommandDiagnostic -> Bool
diagnosticVisible outputMode diagnostic =
  case diagnosticVisibility diagnostic of
    DiagnosticAlways -> True
    DiagnosticHumanOnly -> outputMode == OutputHuman

emitDiagnostic :: CommandDiagnostic -> IO ()
emitDiagnostic = TIO.hPutStrLn stderr . diagnosticText

renderCommandHuman :: RenderStyle -> CommandOutput -> Maybe Text
renderCommandHuman renderStyle = \case
  CommandOutputVersion localVersion maybeVersionInfo ->
    Just (renderVersion localVersion maybeVersionInfo)
  CommandOutputMode config ->
    Just (renderMode config)
  CommandOutputSwitch config ->
    Just (renderSwitch config)
  CommandOutputList listOptions proxies ->
    Just (renderList renderStyle listOptions proxies)
  CommandOutputShow proxyIndex proxies ->
    Just (renderProxyDetails renderStyle proxyIndex proxies)
  CommandOutputTest results _
    | null results -> Nothing
    | otherwise -> Just (renderTestResults renderStyle results)
  CommandOutputSelect selectorName optionName ->
    Just (renderSelect selectorName optionName)
  CommandOutputSsmList showPassword users ->
    Just (renderSsmUserList showPassword users)
  CommandOutputSsmShow users _
    | null users -> Nothing
    | otherwise -> Just (renderSsmUserDetails users)
  CommandOutputSsmAdd userName ->
    Just ("added user: " <> userName)
  CommandOutputSsmRemove userNames _
    | null userNames -> Nothing
    | otherwise -> Just (renderSsmRemovedUsers userNames)
  CommandOutputSsmUpdate userName ->
    Just ("updated password: " <> userName)
  CommandOutputSsmStat cleared stats ->
    Just (renderSsmStats cleared stats)
  CommandOutputTailscaleStatus statuses ->
    Just (renderTailscaleStatus statuses)
  CommandOutputTailscaleIP result ->
    Just (renderTailscaleIP result)

renderCommandJson :: CommandOutput -> Value
renderCommandJson = \case
  CommandOutputVersion localVersion maybeVersionInfo ->
    case maybeVersionInfo of
      Nothing ->
        object ["boxctlVersion" .= localVersion]
      Just versionInfo ->
        object
          [ "boxctlVersion" .= localVersion,
            "server" .= versionInfo
          ]
  CommandOutputMode config ->
    toJSON config
  CommandOutputSwitch config ->
    toJSON config
  CommandOutputList _ proxies ->
    object ["proxies" .= proxies]
  CommandOutputShow _ proxies ->
    object ["proxies" .= proxies]
  CommandOutputTest results errors ->
    object
      [ "results" .= results,
        "errors" .= errors
      ]
  CommandOutputSelect selectorName optionName ->
    object
      [ "selector" .= selectorName,
        "selected" .= optionName
      ]
  CommandOutputSsmList showPassword users ->
    object ["users" .= sanitizeSsmUsers showPassword users]
  CommandOutputSsmShow users errors ->
    object
      [ "users" .= users,
        "errors" .= errors
      ]
  CommandOutputSsmAdd userName ->
    object
      [ "action" .= ("add" :: Text),
        "user" .= userName
      ]
  CommandOutputSsmRemove userNames errors ->
    object
      [ "action" .= ("remove" :: Text),
        "users" .= userNames,
        "errors" .= errors
      ]
  CommandOutputSsmUpdate userName ->
    object
      [ "action" .= ("update" :: Text),
        "user" .= userName
      ]
  CommandOutputSsmStat cleared stats ->
    object
      [ "cleared" .= cleared,
        "stats" .= stats
      ]
  CommandOutputTailscaleStatus statuses ->
    object ["endpoints" .= statuses]
  CommandOutputTailscaleIP result ->
    toJSON result

renderVersion :: Text -> Maybe VersionInfo -> Text
renderVersion localVersion maybeVersionInfo =
  case maybeVersionInfo of
    Nothing ->
      "boxctl " <> localVersion
    Just versionInfo ->
      T.intercalate
        "  "
        [ "boxctl " <> localVersion,
          "server " <> renderVersionLabel versionInfo
        ]

renderMode :: Config -> Text
renderMode config =
  T.intercalate
    "  "
    (["mode: " <> clashModeText (configMode config)] <> availableModeLines config)

renderSwitch :: Config -> Text
renderSwitch config = "mode: " <> clashModeText (configMode config)

renderSelect :: Text -> Text -> Text
renderSelect selectorName optionName =
  selectorName <> ": " <> optionName

renderSsmUserList :: Bool -> [SsmUser] -> Text
renderSsmUserList showPassword users =
  T.intercalate "\n\n" (catMaybes [usersSection, hiddenPasswordsNote])
  where
    orderedUsers = sortOn (T.toCaseFold . ssmUserName) users
    renderedUsers = sanitizeSsmUsers showPassword orderedUsers
    usersSection =
      Just $
        renderSection
          "Users"
          ( if null renderedUsers
              then ["none"]
              else map renderUserRow renderedUsers
          )
    hiddenPasswords =
      length [() | user <- orderedUsers, ssmUserPassword user /= Nothing]
    hiddenPasswordsNote
      | showPassword || hiddenPasswords == 0 = Nothing
      | otherwise =
          Just
            ( T.pack (show hiddenPasswords)
                <> " user passwords hidden; run: boxctl ssm list --show-password"
            )
    renderUserRow user =
      case ssmUserPassword user of
        Nothing ->
          ssmUserName user
        Just password ->
          ssmUserName user <> "  password=" <> password

renderSsmUserDetails :: [SsmUser] -> Text
renderSsmUserDetails =
  T.intercalate "\n\n"
    . map renderUser
    . sortOn (T.toCaseFold . ssmUserName)
  where
    renderUser user =
      T.unlines
        [ ssmUserName user,
          "password: " <> fromMaybe "-" (ssmUserPassword user),
          renderTrafficLine "uplink" (ssmUserUplinkBytes user) (ssmUserUplinkPackets user),
          renderTrafficLine "downlink" (ssmUserDownlinkBytes user) (ssmUserDownlinkPackets user),
          "tcp sessions: " <> renderCount (ssmUserTcpSessions user),
          "udp sessions: " <> renderCount (ssmUserUdpSessions user)
        ]

renderSsmRemovedUsers :: [Text] -> Text
renderSsmRemovedUsers userNames =
  case sortOn T.toCaseFold userNames of
    [] -> ""
    [userName] -> "removed user: " <> userName
    orderedUsers ->
      T.unlines $
        ["removed users:"]
          <> map ("  " <>) orderedUsers

renderSsmStats :: Bool -> SsmStats -> Text
renderSsmStats cleared stats =
  T.intercalate "\n\n" (catMaybes [globalSection, usersSection, clearedNote])
  where
    globalSection =
      Just $
        renderSection
          "Global"
          [ renderTrafficLine "uplink" (ssmStatsUplinkBytes stats) (ssmStatsUplinkPackets stats),
            renderTrafficLine "downlink" (ssmStatsDownlinkBytes stats) (ssmStatsDownlinkPackets stats),
            "tcp sessions: " <> renderCount (ssmStatsTcpSessions stats),
            "udp sessions: " <> renderCount (ssmStatsUdpSessions stats)
          ]
    orderedUsers = sortOn (T.toCaseFold . ssmUserName) (ssmStatsUsers stats)
    usersSection =
      Just $
        renderSection
          "Users"
          ( if null orderedUsers
              then ["none"]
              else map renderStatsUserRow orderedUsers
          )
    clearedNote
      | cleared = Just "counters were cleared after this read"
      | otherwise = Nothing
    renderStatsUserRow user =
      T.intercalate
        "  "
        [ ssmUserName user,
          "up=" <> renderByteCount (ssmUserUplinkBytes user),
          "down=" <> renderByteCount (ssmUserDownlinkBytes user),
          "tcp=" <> renderCount (ssmUserTcpSessions user),
          "udp=" <> renderCount (ssmUserUdpSessions user)
        ]

renderTailscaleStatus :: [TailscaleEndpointStatus] -> Text
renderTailscaleStatus statuses =
  T.intercalate "\n\n" (map renderEndpoint (sortOn (T.toCaseFold . tailscaleEndpointLabel) statuses))
  where
    renderEndpoint status =
      T.intercalate
        "\n"
        ( [ tailscaleEndpointLabel status,
            "  source: " <> tailscaleEndpointSource status
          ]
            <> maybe [] (\value -> ["  tailnet: " <> value]) (tailscaleEndpointTailnetName status)
            <> maybe [] (\value -> ["  magic-dns: " <> value]) (tailscaleEndpointMagicDNSSuffix status)
            <> maybe [] (\value -> ["  snapshot: " <> formatTimestamp value]) (tailscaleEndpointSnapshotTime status)
            <> renderAvailability status
        )

    renderAvailability status
      | not (tailscaleEndpointAvailable status) =
          ["  unavailable: " <> fromMaybe "unknown" (tailscaleEndpointReason status)]
      | otherwise =
          [ "  self: " <> maybe "unknown" renderTailscalePeerSummary (tailscaleEndpointSelf status),
            "  peers:"
          ]
            <> renderPeerRows (tailscaleEndpointPeers status)

    renderPeerRows peers =
      case sortOn peerSortKey peers of
        [] -> ["    none"]
        orderedPeers -> map (("    " <>) . renderTailscalePeerSummary) orderedPeers

    peerSortKey peer =
      ( Down (tailscalePeerOnline peer == Just True),
        T.toCaseFold (tailscalePeerName peer)
      )

renderTailscaleIP :: TailscaleIPResult -> Text
renderTailscaleIP =
  T.intercalate "\n" . tailscaleIPResultAddresses

renderTailscalePeerSummary :: TailscalePeer -> Text
renderTailscalePeerSummary peer =
  T.intercalate "  " (baseParts <> statusParts)
  where
    baseParts =
      [ tailscalePeerName peer,
        case tailscalePeerIPs peer of
          [] -> "-"
          ips -> T.intercalate ", " ips
      ]
    statusParts =
      maybe [] (\value -> ["os=" <> value]) (tailscalePeerOS peer)
        <> [ "online"
           | tailscalePeerOnline peer == Just True
           ]
        <> [ "offline"
           | tailscalePeerOnline peer == Just False
           ]
        <> maybe [] (\value -> ["last-seen=" <> formatTimestamp value]) (tailscalePeerLastSeen peer)

sanitizeSsmUsers :: Bool -> [SsmUser] -> [SsmUser]
sanitizeSsmUsers showPassword
  | showPassword = id
  | otherwise = map hideSsmUserPassword

hideSsmUserPassword :: SsmUser -> SsmUser
hideSsmUserPassword user =
  user {ssmUserPassword = Nothing}

renderTrafficLine :: Text -> Int64 -> Int64 -> Text
renderTrafficLine label byteCount packetCount =
  label
    <> ": "
    <> renderByteCount byteCount
    <> "  packets="
    <> renderCount packetCount

renderByteCount :: Int64 -> Text
renderByteCount byteCount =
  humanReadable <> " (" <> T.pack (show byteCount) <> " B)"
  where
    (scaledValue, unit) = scaleBytes (fromIntegral byteCount :: Double) byteUnits
    humanReadable =
      formatScaledValue scaledValue <> " " <> unit

scaleBytes :: Double -> [Text] -> (Double, Text)
scaleBytes value = go value
  where
    go currentValue [] = (currentValue, "B")
    go currentValue [unit] = (currentValue, unit)
    go currentValue (unit : nextUnit : remainingUnits)
      | currentValue < 1024 = (currentValue, unit)
      | otherwise = go (currentValue / 1024) (nextUnit : remainingUnits)

byteUnits :: [Text]
byteUnits = ["B", "KiB", "MiB", "GiB", "TiB", "PiB"]

formatScaledValue :: Double -> Text
formatScaledValue value
  | value >= 100 = T.pack (show (round value :: Int))
  | value >= 10 = fixedPoint 1 value
  | otherwise = fixedPoint 2 value

fixedPoint :: Int -> Double -> Text
fixedPoint decimals value =
  T.pack (show roundedValue)
  where
    multiplier = (10 :: Int) ^ decimals
    roundedValue =
      fromIntegral (round (value * fromIntegral multiplier) :: Int) / fromIntegral multiplier :: Double

renderCount :: Int64 -> Text
renderCount =
  T.pack . show

renderList :: RenderStyle -> ListOptions -> [Proxy] -> Text
renderList renderStyle listOptions proxies =
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
              else renderGroupRows renderStyle groups
          )
    nodesSection
      | listIncludeNodes listOptions =
          Just $
            renderSection
              "Nodes"
              ( if null nodes
                  then ["none"]
                  else renderNodeRows renderStyle nodes
              )
      | otherwise = Nothing
    hiddenNodesNote
      | listIncludeNodes listOptions || null nodes = Nothing
      | otherwise =
          Just
            ( T.pack (show (length nodes))
                <> " node outbounds hidden; run: boxctl list --all"
            )

renderProxyDetails :: RenderStyle -> Map Text Proxy -> [Proxy] -> Text
renderProxyDetails renderStyle proxyIndex proxies =
  T.intercalate "\n\n" (map renderProxy (sortOn proxyDetailSortKey proxies))
  where
    renderProxy proxy =
      case proxyShape proxy of
        ProxyLeaf _ -> renderLeafDetails renderStyle proxy
        ProxyGroup _ details -> renderGroupDetails renderStyle proxyIndex proxy details

renderGroupDetails :: RenderStyle -> Map Text Proxy -> Proxy -> GroupDetails -> Text
renderGroupDetails renderStyle proxyIndex proxy details =
  T.unlines $
    headerLines
      <> detailsLines
      <> [""]
      <> ["members:"]
      <> memberLines
  where
    headerLines =
      [ proxyName proxy <> "  " <> renderTypeText renderStyle (proxyTypeLabel proxy)
      ]
    detailsLines =
      [ "current: " <> fromMaybe "-" (groupCurrent details),
        "options: " <> T.pack (show (length members))
      ]
        <> maybe [] (\history -> ["last: " <> historySummary renderStyle history]) (latestHistory proxy)
    members = groupMembers details
    memberLines =
      if null members
        then ["  none"]
        else renderMemberRows renderStyle proxyIndex (groupCurrent details) members

renderLeafDetails :: RenderStyle -> Proxy -> Text
renderLeafDetails renderStyle proxy =
  T.intercalate
    "  "
    ( [ proxyName proxy,
        renderTypeText renderStyle (proxyTypeLabel proxy),
        "last=" <> maybe "unknown" (historySummary renderStyle) (latestHistory proxy)
      ]
        <> ["udp=no" | not (proxyUdp proxy)]
    )

renderTestResults :: RenderStyle -> [TestResult] -> Text
renderTestResults renderStyle =
  T.intercalate "\n\n"
    . map renderResult
    . sortOn testResultSortKey
  where
    renderResult (ProxyDelay proxy status) =
      T.intercalate
        "  "
        [ proxyName proxy,
          renderTypeText renderStyle (proxyTypeLabel proxy),
          "result=" <> renderDelayStatus renderStyle status
        ]
    renderResult (GroupDelay proxy delays) =
      T.unlines $
        [ proxyName proxy <> "  " <> renderTypeText renderStyle (proxyTypeLabel proxy) <> currentSuffix (proxyCurrent proxy),
          ""
        ]
          <> renderDelayRows renderStyle (proxyCurrent proxy) delays

renderDelayStatus :: RenderStyle -> DelayStatus -> Text
renderDelayStatus renderStyle = \case
  DelayOk delayMs -> renderDelayMs renderStyle delayMs
  DelayUnavailable reason -> reason

renderVersionLabel :: VersionInfo -> Text
renderVersionLabel versionInfo =
  case versionFlags of
    [] -> versionText versionInfo
    flags ->
      versionText versionInfo
        <> " (reported "
        <> T.intercalate ", " flags
        <> ")"
  where
    versionFlags =
      [ "premium" | versionPremium versionInfo
      ]
        <> [ "meta" | versionMeta versionInfo
           ]

availableModeLines :: Config -> [Text]
availableModeLines config =
  case dedupeModes (configModeList config) of
    [] -> []
    [onlyMode]
      | onlyMode == configMode config -> []
    modes -> ["available=" <> T.intercalate ", " (map clashModeText modes)]

dedupeModes :: [ClashMode] -> [ClashMode]
dedupeModes = List.foldl' insertMode []
  where
    insertMode acc mode
      | mode `elem` acc = acc
      | otherwise = acc <> [mode]

renderGroupRows :: RenderStyle -> [Proxy] -> [Text]
renderGroupRows renderStyle proxies =
  map renderRow proxies
  where
    nameWidth = columnWidth 8 (map (T.length . proxyName) proxies)
    kindWidth = columnWidth 8 (map (T.length . proxyTypeLabel) proxies)
    renderRow proxy =
      pad nameWidth (proxyName proxy)
        <> "  "
        <> renderTypeCell renderStyle kindWidth (proxyTypeLabel proxy)
        <> "  "
        <> T.intercalate "  " (groupSummaryParts renderStyle proxy)

renderNodeRows :: RenderStyle -> [Proxy] -> [Text]
renderNodeRows renderStyle proxies =
  map renderRow proxies
  where
    nameWidth = columnWidth 8 (map (T.length . proxyName) proxies)
    kindWidth = columnWidth 8 (map (T.length . proxyTypeLabel) proxies)
    renderRow proxy =
      pad nameWidth (proxyName proxy)
        <> "  "
        <> renderTypeCell renderStyle kindWidth (proxyTypeLabel proxy)
        <> "  "
        <> nodeSummary renderStyle proxy

renderMemberRows :: RenderStyle -> Map Text Proxy -> Maybe Text -> [Text] -> [Text]
renderMemberRows renderStyle proxyIndex current members =
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
        <> renderTypeCell renderStyle kindWidth (maybe "unknown" proxyTypeLabel maybeProxy)
        <> "  "
        <> maybe "unknown" (memberSummary renderStyle) maybeProxy

memberSortKey :: Maybe Text -> Map Text Proxy -> Text -> (Int, Int, Text)
memberSortKey current proxyIndex memberName =
  ( currentRank current memberName,
    maybe maxBound historyDelay (Map.lookup memberName proxyIndex >>= latestHistory),
    T.toCaseFold memberName
  )

renderDelayRows :: RenderStyle -> Maybe Text -> [(Text, DelayStatus)] -> [Text]
renderDelayRows renderStyle current delays =
  map renderDelayRow ordered
  where
    ordered = sortOn (delaySortKey current) delays
    nameWidth = columnWidth 8 (map (T.length . fst) ordered)
    renderDelayRow (memberName, status) =
      currentMarker current memberName
        <> " "
        <> pad nameWidth memberName
        <> "  "
        <> renderDelayStatus renderStyle status

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

proxyDetailSortKey :: Proxy -> (Int, Text, Text)
proxyDetailSortKey proxy =
  ( groupKindRank proxy,
    proxyNameKey proxy,
    proxyTypeLabel proxy
  )

groupSortKey :: Proxy -> (Int, Text)
groupSortKey proxy =
  ( groupKindRank proxy,
    proxyNameKey proxy
  )

groupKindRank :: Proxy -> Int
groupKindRank proxy
  | isSelectorProxy proxy = 0
  | isUrlTestProxy proxy = 1
  | otherwise = 2

groupSummaryParts :: RenderStyle -> Proxy -> [Text]
groupSummaryParts renderStyle proxy =
  case proxyShape proxy of
    ProxyLeaf _ -> []
    ProxyGroup _ details ->
      catMaybes
        [ fmap ("current=" <>) (groupCurrent details),
          Just (T.pack (show (length (groupMembers details))) <> " options"),
          fmap (\history -> "last=" <> renderDelayMs renderStyle (historyDelay history)) (latestHistory proxy)
        ]

nodeSummary :: RenderStyle -> Proxy -> Text
nodeSummary renderStyle proxy =
  T.intercalate
    "  "
    ( [ maybe "unknown" (\history -> renderDelayMs renderStyle (historyDelay history)) (latestHistory proxy)
      ]
        <> ["udp=no" | not (proxyUdp proxy)]
    )

memberSummary :: RenderStyle -> Proxy -> Text
memberSummary renderStyle proxy =
  T.intercalate
    "  "
    ( [ maybe "unknown" (\history -> renderDelayMs renderStyle (historyDelay history)) (latestHistory proxy)
      ]
        <> maybe [] (\history -> [formatTimestamp (historyTime history)]) (latestHistory proxy)
    )

latestHistory :: Proxy -> Maybe DelayHistory
latestHistory proxy =
  listToMaybe (sortOn (Down . historyTime) (proxyHistory proxy))

historySummary :: RenderStyle -> DelayHistory -> Text
historySummary renderStyle delayHistory =
  renderDelayMs renderStyle (historyDelay delayHistory)
    <> " at "
    <> formatTimestamp (historyTime delayHistory)

formatTimestamp :: UTCTime -> Text
formatTimestamp =
  T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%MZ"

renderSection :: Text -> [Text] -> Text
renderSection title rows =
  T.intercalate "\n" (title : map ("  " <>) rows)

proxyNameKey :: Proxy -> Text
proxyNameKey = T.toCaseFold . proxyName

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

renderTypeCell :: RenderStyle -> Int -> Text -> Text
renderTypeCell renderStyle width typeLabel =
  colorizeType renderStyle typeLabel (pad width typeLabel)

renderTypeText :: RenderStyle -> Text -> Text
renderTypeText renderStyle typeLabel =
  colorizeType renderStyle typeLabel typeLabel

colorizeType :: RenderStyle -> Text -> Text -> Text
colorizeType renderStyle colorKey renderedText
  | not (renderUseColor renderStyle) = renderedText
  | otherwise =
      let Rgb red green blue =
            typeColorRgb (renderBackgroundTone renderStyle) colorKey
       in ansiForeground red green blue renderedText

renderDelayMs :: RenderStyle -> Int -> Text
renderDelayMs renderStyle delayMs =
  colorizeRgb renderStyle (delayColorRgb (renderBackgroundTone renderStyle) delayMs) delayText
  where
    delayText = T.pack (show delayMs) <> "ms"

colorizeRgb :: RenderStyle -> Rgb -> Text -> Text
colorizeRgb renderStyle (Rgb red green blue) renderedText
  | not (renderUseColor renderStyle) = renderedText
  | otherwise = ansiForeground red green blue renderedText

ansiForeground :: Int -> Int -> Int -> Text -> Text
ansiForeground red green blue text =
  "\ESC[38;2;"
    <> decimal red
    <> ";"
    <> decimal green
    <> ";"
    <> decimal blue
    <> "m"
    <> text
    <> "\ESC[39m"

decimal :: Int -> Text
decimal = T.pack . show

emitJson :: Value -> IO ()
emitJson = BL8.putStrLn . encode

emitText :: Text -> IO ()
emitText = TIO.putStrLn . T.dropWhileEnd (== '\n')

data Rgb = Rgb !Int !Int !Int

typeColorRgb :: BackgroundTone -> Text -> Rgb
typeColorRgb backgroundTone typeLabel =
  fitOklchToRgb lightness chroma hue
  where
    normalized = T.toCaseFold typeLabel
    seed0 = fnv1a32 (TE.encodeUtf8 normalized)
    hueSeed = mix32 (seed0 + 0x9E3779B9)
    lightnessSeed = mix32 (seed0 + 0x243F6A88)
    chromaSeed = mix32 (seed0 + 0xB7E15162)
    hue = 360.0 * unitInterval hueSeed
    lightness =
      case backgroundTone of
        BackgroundDark -> 0.76 + centeredUnit lightnessSeed * 0.10
        BackgroundLight -> 0.57 + centeredUnit lightnessSeed * 0.10
    chroma =
      case backgroundTone of
        BackgroundDark -> 0.15 + centeredUnit chromaSeed * 0.05
        BackgroundLight -> 0.12 + centeredUnit chromaSeed * 0.04

delayColorRgb :: BackgroundTone -> Int -> Rgb
delayColorRgb backgroundTone delayMs =
  fitOklchToRgb lightness chroma hue
  where
    progress = delayProgress delayMs
    hue = 145.0 - 50.0 * progress
    lightness =
      case backgroundTone of
        BackgroundDark -> 0.82
        BackgroundLight -> 0.62
    chroma =
      case backgroundTone of
        BackgroundDark -> 0.16
        BackgroundLight -> 0.13

delayProgress :: Int -> Double
delayProgress delayMs =
  clamp 0.0 1.0 $
    ( log clampedDelay - log delayFloorMs
    )
      / ( log delayCeilingMs - log delayFloorMs
        )
  where
    clampedDelay = clamp delayFloorMs delayCeilingMs (fromIntegral (max 1 delayMs))
    delayFloorMs = 20.0
    delayCeilingMs = 2500.0

fitOklchToRgb :: Double -> Double -> Double -> Rgb
fitOklchToRgb lightness chroma hue =
  go chroma (0 :: Int)
  where
    boundedLightness = clamp 0.0 1.0 lightness
    go currentChroma attempts =
      case oklchToRgb boundedLightness currentChroma hue of
        Just rgb -> rgb
        Nothing
          | attempts >= 12 -> clippedRgb boundedLightness currentChroma hue
          | otherwise -> go (currentChroma * 0.82) (attempts + 1)

clippedRgb :: Double -> Double -> Double -> Rgb
clippedRgb lightness chroma hue =
  let (redLinear, greenLinear, blueLinear) = oklchToLinearSrgb lightness chroma hue
   in Rgb
        (toSrgb8 (clamp 0.0 1.0 redLinear))
        (toSrgb8 (clamp 0.0 1.0 greenLinear))
        (toSrgb8 (clamp 0.0 1.0 blueLinear))

oklchToRgb :: Double -> Double -> Double -> Maybe Rgb
oklchToRgb lightness chroma hue =
  if all inGamut [redLinear, greenLinear, blueLinear]
    then Just (Rgb (toSrgb8 redLinear) (toSrgb8 greenLinear) (toSrgb8 blueLinear))
    else Nothing
  where
    (redLinear, greenLinear, blueLinear) = oklchToLinearSrgb lightness chroma hue

oklchToLinearSrgb :: Double -> Double -> Double -> (Double, Double, Double)
oklchToLinearSrgb lightness chroma hueDegrees =
  ( redLinear,
    greenLinear,
    blueLinear
  )
  where
    hueRadians = hueDegrees * pi / 180.0
    a = chroma * cos hueRadians
    b = chroma * sin hueRadians
    l' = lightness + 0.3963377774 * a + 0.2158037573 * b
    m' = lightness - 0.1055613458 * a - 0.0638541728 * b
    s' = lightness - 0.0894841775 * a - 1.2914855480 * b
    l = cube l'
    m = cube m'
    s = cube s'
    redLinear = 4.0767416621 * l - 3.3077115913 * m + 0.2309699292 * s
    greenLinear = -1.2684380046 * l + 2.6097574011 * m - 0.3413193965 * s
    blueLinear = -0.0041960863 * l - 0.7034186147 * m + 1.7076147010 * s

cube :: Double -> Double
cube value = value * value * value

toSrgb8 :: Double -> Int
toSrgb8 linear =
  round (255.0 * srgbEncoded)
  where
    srgbEncoded
      | linear <= 0.0031308 = 12.92 * linear
      | otherwise = 1.055 * (linear ** (1.0 / 2.4)) - 0.055

inGamut :: Double -> Bool
inGamut value = value >= (-1.0e-6) && value <= 1.0 + 1.0e-6

clamp :: Double -> Double -> Double -> Double
clamp lower upper value = max lower (min upper value)

centeredUnit :: Word32 -> Double
centeredUnit word = unitInterval word - 0.5

unitInterval :: Word32 -> Double
unitInterval word =
  fromIntegral word / 4294967296.0

fnv1a32 :: BS.ByteString -> Word32
fnv1a32 =
  BS.foldl' step 2166136261
  where
    step acc byte =
      (acc `xor` fromIntegral byte) * 16777619

mix32 :: Word32 -> Word32
mix32 value0 = rotateR value5 7 `xor` value5
  where
    value1 = value0 `xor` (value0 `shiftR` 16)
    value2 = value1 * 0x85EBCA6B
    value3 = value2 `xor` (value2 `shiftR` 13)
    value4 = value3 * 0xC2B2AE35
    value5 = value4 `xor` (value4 `shiftR` 16)
