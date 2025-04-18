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
import Data.List (sortOn)
import Data.Maybe (listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Format.ISO8601 (iso8601Show)

data DelayStatus
  = DelayOk Int
  | DelayUnavailable Text
  deriving (Eq, Show)

data TestResult
  = ProxyDelay ProxyInfo DelayStatus
  | GroupDelay ProxyInfo [(Text, DelayStatus)]
  deriving (Eq, Show)

renderVersion :: Text -> VersionResponse -> Text
renderVersion localVersion versionInfo =
  T.unlines
    [ "boxctl " <> localVersion,
      versionText versionInfo
    ]

renderMode :: ConfigResponse -> Text
renderMode config =
  case configModeList config of
    [] -> configMode config
    modes ->
      configMode config
        <> "\n"
        <> "available: "
        <> T.intercalate ", " modes

renderSwitch :: ConfigResponse -> Text
renderSwitch config = "mode: " <> configMode config

renderSelect :: Text -> Text -> Text
renderSelect selectorName optionName =
  selectorName <> " -> " <> optionName

renderList :: [ProxyInfo] -> Text
renderList proxies =
  T.unlines $
    header : map renderRow ordered
  where
    ordered = sortOn (T.toCaseFold . proxyName) proxies
    typeWidth = columnWidth 4 (map (T.length . proxyType) ordered)
    nameWidth = columnWidth 4 (map (T.length . proxyName) ordered)
    header =
      pad typeWidth "TYPE"
        <> "  "
        <> pad nameWidth "NAME"
        <> "  UDP  DETAILS"
    renderRow proxy =
      pad typeWidth (proxyType proxy)
        <> "  "
        <> pad nameWidth (proxyName proxy)
        <> "  "
        <> pad 3 (boolText (proxyUdp proxy))
        <> "  "
        <> summarize proxy

renderProxyDetails :: [ProxyInfo] -> Text
renderProxyDetails proxies =
  T.intercalate "\n\n" (map renderProxy (sortOn (T.toCaseFold . proxyName) proxies))

renderProxy :: ProxyInfo -> Text
renderProxy proxy =
  T.intercalate
    "\n"
    ( [ proxyName proxy,
        "  type: " <> proxyType proxy,
        "  udp: " <> boolText (proxyUdp proxy)
      ]
        <> maybe [] (\current -> ["  now: " <> current]) (proxyNow proxy)
        <> maybe [] (\members -> ["  all: " <> T.intercalate ", " members]) (proxyAll proxy)
        <> historyLines proxy
    )

historyLines :: ProxyInfo -> [Text]
historyLines proxy =
  case listToMaybe (sortOn (Down . historyTime) (proxyHistory proxy)) of
    Nothing ->
      ["  last-delay: unknown"]
    Just delayHistory ->
      [ "  last-delay: " <> T.pack (show (historyDelay delayHistory)) <> "ms",
        "  last-test: " <> T.pack (iso8601Show (historyTime delayHistory))
      ]

renderTestResults :: [TestResult] -> Text
renderTestResults =
  T.intercalate "\n\n"
    . map renderResult
  where
    renderResult (ProxyDelay proxy status) =
      T.intercalate
        "\n"
        [ proxyName proxy <> " (" <> proxyType proxy <> ")",
          "  " <> renderDelayStatus status
        ]
    renderResult (GroupDelay proxy delays) =
      T.intercalate
        "\n"
        ( [ groupHeader proxy ]
            <> map renderDelayLine delays
        )

    groupHeader proxy =
      proxyName proxy
        <> " ("
        <> proxyType proxy
        <> maybe "" (\current -> ", now=" <> current) (proxyNow proxy)
        <> ")"

    renderDelayLine (name, status) =
      "  " <> name <> ": " <> renderDelayStatus status

renderDelayStatus :: DelayStatus -> Text
renderDelayStatus = \case
  DelayOk delayMs -> T.pack (show delayMs) <> "ms"
  DelayUnavailable reason -> reason

summarize :: ProxyInfo -> Text
summarize proxy =
  let nowPart = maybe "" (\current -> "now=" <> current <> ", ") (proxyNow proxy)
      membersPart = maybe "" (\members -> T.pack (show (length members)) <> " options") (proxyAll proxy)
      historyPart =
        case listToMaybe (sortOn (Down . historyTime) (proxyHistory proxy)) of
          Just delayHistory -> "delay=" <> T.pack (show (historyDelay delayHistory)) <> "ms"
          Nothing -> "delay=?"
   in if T.null membersPart
        then historyPart
        else nowPart <> membersPart

pad :: Int -> Text -> Text
pad width text = text <> T.replicate (max 0 (width - T.length text)) " "

boolText :: Bool -> Text
boolText True = "yes"
boolText False = "no"

columnWidth :: Int -> [Int] -> Int
columnWidth minimumWidth values =
  max minimumWidth $
    case values of
      [] -> minimumWidth
      _ -> maximum values
