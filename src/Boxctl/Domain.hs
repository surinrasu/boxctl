{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Boxctl.Domain
  ( ClashMode (..),
    Config (..),
    DelayHistory (..),
    GroupDetails (..),
    GroupKind (..),
    KnownClashMode (..),
    Proxy (..),
    ProxyMeta (..),
    ProxyShape (..),
    SsmStats (..),
    SsmUser (..),
    TailscaleEndpointStatus (..),
    TailscaleIPResult (..),
    TailscalePeer (..),
    VersionInfo (..),
    clashModeFromText,
    clashModeMatchesKnown,
    clashModeText,
    isGroupProxy,
    isSelectorProxy,
    isUrlTestProxy,
    knownClashModeFromText,
    knownClashModeText,
    proxyCurrent,
    proxyGroupDetails,
    proxyHistory,
    proxyMembers,
    proxyName,
    proxyTypeLabel,
    proxyUdp,
  )
where

import Data.Aeson (ToJSON (..), Value (String), object, (.=))
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)

data KnownClashMode
  = KnownClashModeRule
  | KnownClashModeGlobal
  | KnownClashModeDirect
  | KnownClashModeScript
  deriving (Eq, Show)

data ClashMode
  = ClashModeKnown KnownClashMode
  | ClashModeUnknown Text
  deriving (Eq, Show)

data VersionInfo = VersionInfo
  { versionText :: Text,
    versionPremium :: Bool,
    versionMeta :: Bool
  }
  deriving (Eq, Show)

data Config = Config
  { configMode :: ClashMode,
    configModeList :: [ClashMode]
  }
  deriving (Eq, Show)

data TailscalePeer = TailscalePeer
  { tailscalePeerName :: Text,
    tailscalePeerHostName :: Maybe Text,
    tailscalePeerDNSName :: Maybe Text,
    tailscalePeerOS :: Maybe Text,
    tailscalePeerIPs :: [Text],
    tailscalePeerOnline :: Maybe Bool,
    tailscalePeerLastSeen :: Maybe UTCTime,
    tailscalePeerKeyExpiry :: Maybe UTCTime
  }
  deriving (Eq, Show)

data TailscaleEndpointStatus = TailscaleEndpointStatus
  { tailscaleEndpointLabel :: Text,
    tailscaleEndpointTag :: Maybe Text,
    tailscaleEndpointStateDirectory :: FilePath,
    tailscaleEndpointSource :: Text,
    tailscaleEndpointTailnetName :: Maybe Text,
    tailscaleEndpointMagicDNSSuffix :: Maybe Text,
    tailscaleEndpointSnapshotTime :: Maybe UTCTime,
    tailscaleEndpointAvailable :: Bool,
    tailscaleEndpointReason :: Maybe Text,
    tailscaleEndpointSelf :: Maybe TailscalePeer,
    tailscaleEndpointPeers :: [TailscalePeer]
  }
  deriving (Eq, Show)

data TailscaleIPResult = TailscaleIPResult
  { tailscaleIPResultEndpoint :: Text,
    tailscaleIPResultPeer :: Maybe Text,
    tailscaleIPResultAddresses :: [Text],
    tailscaleIPResultSnapshotTime :: Maybe UTCTime
  }
  deriving (Eq, Show)

data DelayHistory = DelayHistory
  { historyTime :: UTCTime,
    historyDelay :: Int
  }
  deriving (Eq, Show)

data ProxyMeta = ProxyMeta
  { proxyMetaName :: Text,
    proxyMetaUdp :: Bool,
    proxyMetaHistory :: [DelayHistory]
  }
  deriving (Eq, Show)

data GroupDetails = GroupDetails
  { groupCurrent :: Maybe Text,
    groupMembers :: [Text]
  }
  deriving (Eq, Show)

data GroupKind
  = GroupKindSelector
  | GroupKindUrlTest
  | GroupKindGeneric Text
  deriving (Eq, Show)

data ProxyShape
  = ProxyLeaf Text
  | ProxyGroup GroupKind GroupDetails
  deriving (Eq, Show)

data Proxy = Proxy
  { proxyMeta :: ProxyMeta,
    proxyShape :: ProxyShape
  }
  deriving (Eq, Show)

data SsmUser = SsmUser
  { ssmUserName :: Text,
    ssmUserPassword :: Maybe Text,
    ssmUserDownlinkBytes :: Int64,
    ssmUserUplinkBytes :: Int64,
    ssmUserDownlinkPackets :: Int64,
    ssmUserUplinkPackets :: Int64,
    ssmUserTcpSessions :: Int64,
    ssmUserUdpSessions :: Int64
  }
  deriving (Eq, Show)

data SsmStats = SsmStats
  { ssmStatsUplinkBytes :: Int64,
    ssmStatsDownlinkBytes :: Int64,
    ssmStatsUplinkPackets :: Int64,
    ssmStatsDownlinkPackets :: Int64,
    ssmStatsTcpSessions :: Int64,
    ssmStatsUdpSessions :: Int64,
    ssmStatsUsers :: [SsmUser]
  }
  deriving (Eq, Show)

instance ToJSON KnownClashMode where
  toJSON = String . knownClashModeText

instance ToJSON ClashMode where
  toJSON = String . clashModeText

instance ToJSON VersionInfo where
  toJSON versionInfo =
    object
      [ "version" .= versionText versionInfo,
        "premium" .= versionPremium versionInfo,
        "meta" .= versionMeta versionInfo
      ]

instance ToJSON Config where
  toJSON config =
    object
      [ "mode" .= configMode config,
        "mode-list" .= configModeList config
      ]

instance ToJSON TailscalePeer where
  toJSON peer =
    object $
      [ "name" .= tailscalePeerName peer,
        "ips" .= tailscalePeerIPs peer
      ]
        <> maybe [] (\value -> ["hostName" .= value]) (tailscalePeerHostName peer)
        <> maybe [] (\value -> ["dnsName" .= value]) (tailscalePeerDNSName peer)
        <> maybe [] (\value -> ["os" .= value]) (tailscalePeerOS peer)
        <> maybe [] (\value -> ["online" .= value]) (tailscalePeerOnline peer)
        <> maybe [] (\value -> ["lastSeen" .= value]) (tailscalePeerLastSeen peer)
        <> maybe [] (\value -> ["keyExpiry" .= value]) (tailscalePeerKeyExpiry peer)

instance ToJSON TailscaleEndpointStatus where
  toJSON status =
    object $
      [ "label" .= tailscaleEndpointLabel status,
        "stateDirectory" .= tailscaleEndpointStateDirectory status,
        "source" .= tailscaleEndpointSource status,
        "available" .= tailscaleEndpointAvailable status,
        "peers" .= tailscaleEndpointPeers status
      ]
        <> maybe [] (\value -> ["tag" .= value]) (tailscaleEndpointTag status)
        <> maybe [] (\value -> ["tailnet" .= value]) (tailscaleEndpointTailnetName status)
        <> maybe [] (\value -> ["magicDNSSuffix" .= value]) (tailscaleEndpointMagicDNSSuffix status)
        <> maybe [] (\value -> ["snapshotTime" .= value]) (tailscaleEndpointSnapshotTime status)
        <> maybe [] (\value -> ["reason" .= value]) (tailscaleEndpointReason status)
        <> maybe [] (\value -> ["self" .= value]) (tailscaleEndpointSelf status)

instance ToJSON TailscaleIPResult where
  toJSON result =
    object $
      [ "endpoint" .= tailscaleIPResultEndpoint result,
        "addresses" .= tailscaleIPResultAddresses result
      ]
        <> maybe [] (\value -> ["peer" .= value]) (tailscaleIPResultPeer result)
        <> maybe [] (\value -> ["snapshotTime" .= value]) (tailscaleIPResultSnapshotTime result)

instance ToJSON DelayHistory where
  toJSON delayHistory =
    object
      [ "time" .= historyTime delayHistory,
        "delay" .= historyDelay delayHistory
      ]

instance ToJSON Proxy where
  toJSON proxy =
    object (baseFields <> groupFields)
    where
      meta = proxyMeta proxy
      baseFields =
        [ "type" .= proxyTypeLabel proxy,
          "name" .= proxyMetaName meta,
          "udp" .= proxyMetaUdp meta,
          "history" .= proxyMetaHistory meta
        ]
      groupFields =
        case proxyShape proxy of
          ProxyLeaf _ -> []
          ProxyGroup _ details ->
            [ "now" .= groupCurrent details,
              "all" .= groupMembers details
            ]

instance ToJSON SsmUser where
  toJSON user =
    object $
      [ "username" .= ssmUserName user,
        "downlinkBytes" .= ssmUserDownlinkBytes user,
        "uplinkBytes" .= ssmUserUplinkBytes user,
        "downlinkPackets" .= ssmUserDownlinkPackets user,
        "uplinkPackets" .= ssmUserUplinkPackets user,
        "tcpSessions" .= ssmUserTcpSessions user,
        "udpSessions" .= ssmUserUdpSessions user
      ]
        <> maybe [] (\password -> ["password" .= password]) (ssmUserPassword user)

instance ToJSON SsmStats where
  toJSON stats =
    object
      [ "uplinkBytes" .= ssmStatsUplinkBytes stats,
        "downlinkBytes" .= ssmStatsDownlinkBytes stats,
        "uplinkPackets" .= ssmStatsUplinkPackets stats,
        "downlinkPackets" .= ssmStatsDownlinkPackets stats,
        "tcpSessions" .= ssmStatsTcpSessions stats,
        "udpSessions" .= ssmStatsUdpSessions stats,
        "users" .= ssmStatsUsers stats
      ]

knownClashModeFromText :: Text -> Maybe KnownClashMode
knownClashModeFromText rawValue =
  case T.toCaseFold (T.strip rawValue) of
    "rule" -> Just KnownClashModeRule
    "global" -> Just KnownClashModeGlobal
    "direct" -> Just KnownClashModeDirect
    "script" -> Just KnownClashModeScript
    _ -> Nothing

clashModeFromText :: Text -> ClashMode
clashModeFromText rawValue =
  case knownClashModeFromText rawValue of
    Just knownMode -> ClashModeKnown knownMode
    Nothing -> ClashModeUnknown (T.toCaseFold (T.strip rawValue))

knownClashModeText :: KnownClashMode -> Text
knownClashModeText = \case
  KnownClashModeRule -> "rule"
  KnownClashModeGlobal -> "global"
  KnownClashModeDirect -> "direct"
  KnownClashModeScript -> "script"

clashModeText :: ClashMode -> Text
clashModeText = \case
  ClashModeKnown knownMode -> knownClashModeText knownMode
  ClashModeUnknown other -> other

clashModeMatchesKnown :: KnownClashMode -> ClashMode -> Bool
clashModeMatchesKnown knownMode =
  (== knownClashModeText knownMode) . clashModeText

isGroupProxy :: Proxy -> Bool
isGroupProxy proxy =
  case proxyShape proxy of
    ProxyLeaf _ -> False
    ProxyGroup _ _ -> True

isSelectorProxy :: Proxy -> Bool
isSelectorProxy proxy =
  case proxyShape proxy of
    ProxyGroup GroupKindSelector _ -> True
    _ -> False

isUrlTestProxy :: Proxy -> Bool
isUrlTestProxy proxy =
  case proxyShape proxy of
    ProxyGroup GroupKindUrlTest _ -> True
    _ -> False

proxyName :: Proxy -> Text
proxyName = proxyMetaName . proxyMeta

proxyUdp :: Proxy -> Bool
proxyUdp = proxyMetaUdp . proxyMeta

proxyHistory :: Proxy -> [DelayHistory]
proxyHistory = proxyMetaHistory . proxyMeta

proxyGroupDetails :: Proxy -> Maybe GroupDetails
proxyGroupDetails proxy =
  case proxyShape proxy of
    ProxyLeaf _ -> Nothing
    ProxyGroup _ details -> Just details

proxyCurrent :: Proxy -> Maybe Text
proxyCurrent = (groupCurrent =<<) . proxyGroupDetails

proxyMembers :: Proxy -> Maybe [Text]
proxyMembers = fmap groupMembers . proxyGroupDetails

proxyTypeLabel :: Proxy -> Text
proxyTypeLabel proxy =
  case proxyShape proxy of
    ProxyLeaf kind -> kind
    ProxyGroup groupKind _ ->
      case groupKind of
        GroupKindSelector -> "selector"
        GroupKindUrlTest -> "url-test"
        GroupKindGeneric kind -> kind
