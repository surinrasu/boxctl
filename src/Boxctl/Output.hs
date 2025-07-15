{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Boxctl.Output
  ( BackgroundTone (..),
    DelayStatus (..),
    RenderStyle (..),
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
import Data.Bits (rotateR, shiftR, xor)
import Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.ByteString as BS
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Word (Word32)

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

renderList :: RenderStyle -> ListOptions -> [ProxyInfo] -> Text
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

renderProxyDetails :: RenderStyle -> Map Text ProxyInfo -> [ProxyInfo] -> Text
renderProxyDetails renderStyle proxyIndex proxies =
  T.intercalate "\n\n" (map renderProxy (sortOn proxyDetailSortKey proxies))
  where
    renderProxy proxy
      | isGroupProxy proxy = renderGroupDetails renderStyle proxyIndex proxy
      | otherwise = renderLeafDetails renderStyle proxy

renderGroupDetails :: RenderStyle -> Map Text ProxyInfo -> ProxyInfo -> Text
renderGroupDetails renderStyle proxyIndex proxy =
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
      [ "current: " <> fromMaybe "-" (proxyNow proxy),
        "options: " <> T.pack (show (length members))
      ]
        <> maybe [] (\history -> ["last: " <> historySummary renderStyle history]) (latestHistory proxy)
    members = fromMaybe [] (proxyAll proxy)
    memberLines =
      if null members
        then ["  none"]
        else renderMemberRows renderStyle proxyIndex (proxyNow proxy) members

renderLeafDetails :: RenderStyle -> ProxyInfo -> Text
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
        [ proxyName proxy <> "  " <> renderTypeText renderStyle (proxyTypeLabel proxy) <> currentSuffix (proxyNow proxy),
          ""
        ]
          <> renderDelayRows renderStyle (proxyNow proxy) delays

renderDelayStatus :: RenderStyle -> DelayStatus -> Text
renderDelayStatus renderStyle = \case
  DelayOk delayMs -> renderDelayMs renderStyle delayMs
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

renderGroupRows :: RenderStyle -> [ProxyInfo] -> [Text]
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

renderNodeRows :: RenderStyle -> [ProxyInfo] -> [Text]
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

renderMemberRows :: RenderStyle -> Map Text ProxyInfo -> Maybe Text -> [Text] -> [Text]
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

memberSortKey :: Maybe Text -> Map Text ProxyInfo -> Text -> (Int, Int, Text)
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

groupSummaryParts :: RenderStyle -> ProxyInfo -> [Text]
groupSummaryParts renderStyle proxy =
  catMaybes
    [ fmap ("current=" <>) (proxyNow proxy),
      fmap (\members -> T.pack (show (length members)) <> " options") (proxyAll proxy),
      fmap (\history -> "last=" <> renderDelayMs renderStyle (historyDelay history)) (latestHistory proxy)
    ]

nodeSummary :: RenderStyle -> ProxyInfo -> Text
nodeSummary renderStyle proxy =
  T.intercalate
    "  "
    ( [ maybe "unknown" (\history -> renderDelayMs renderStyle (historyDelay history)) (latestHistory proxy)
      ]
        <> ["udp=no" | not (proxyUdp proxy)]
    )

memberSummary :: RenderStyle -> ProxyInfo -> Text
memberSummary renderStyle proxy =
  T.intercalate
    "  "
    ( [ maybe "unknown" (\history -> renderDelayMs renderStyle (historyDelay history)) (latestHistory proxy)
      ]
        <> maybe [] (\history -> [formatTimestamp (historyTime history)]) (latestHistory proxy)
    )

latestHistory :: ProxyInfo -> Maybe DelayHistory
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

renderTypeCell :: RenderStyle -> Int -> Text -> Text
renderTypeCell renderStyle width typeLabel =
  colorizeType renderStyle typeLabel (pad width typeLabel)

renderTypeText :: RenderStyle -> Text -> Text
renderTypeText renderStyle typeLabel
  = colorizeType renderStyle typeLabel typeLabel

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
