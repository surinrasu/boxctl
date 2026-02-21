{-# LANGUAGE OverloadedStrings #-}

module Boxctl.Tailscale
  ( loadTailscaleEndpointStatus,
  )
where

import Boxctl.Domain
  ( TailscaleEndpointStatus (..),
    TailscalePeer (..)
  )
import qualified Boxctl.Instance as Instance
import Control.Applicative ((<|>))
import Control.Exception (IOException, try)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Aeson ((.:?), (.!=), eitherDecodeStrict')
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (catMaybes, fromMaybe, isNothing, listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time (UTCTime)
import System.Directory (doesDirectoryExist, doesFileExist, getModificationTime, listDirectory)
import System.FilePath ((</>))

loadTailscaleEndpointStatus :: Instance.TailscaleEndpointConfig -> FilePath -> IO TailscaleEndpointStatus
loadTailscaleEndpointStatus endpoint stateDirectory = do
  stateStoreResult <- loadStateStore (stateDirectory </> "tailscaled.state")
  case stateStoreResult of
    Left reason ->
      pure (unavailableStatus endpoint stateDirectory reason)
    Right stateStore ->
      case selectActiveProfile (stateStoreSnapshotValues stateStore) of
        Left reason ->
          pure
            ( (unavailableStatus endpoint stateDirectory reason)
                { tailscaleEndpointSnapshotTime = stateStoreSnapshotTime stateStore
                }
            )
        Right profile -> do
          netmapResult <- loadNetmapCache stateDirectory profile
          case netmapResult of
            Left reason ->
              pure
                ( (unavailableProfileStatus endpoint stateDirectory profile reason)
                    { tailscaleEndpointSnapshotTime = stateStoreSnapshotTime stateStore
                    }
                )
            Right netmapSnapshot ->
              pure
                TailscaleEndpointStatus
                  { tailscaleEndpointLabel = Instance.tailscaleEndpointLabel endpoint,
                    tailscaleEndpointTag = Instance.tailscaleEndpointTag endpoint,
                    tailscaleEndpointStateDirectory = stateDirectory,
                    tailscaleEndpointSource = "state-cache",
                    tailscaleEndpointTailnetName = networkDisplayNameOrDefault (activeProfileNetwork profile),
                    tailscaleEndpointMagicDNSSuffix = cachedNetworkMagicDNSName (activeProfileNetwork profile),
                    tailscaleEndpointSnapshotTime = netmapSnapshotTime netmapSnapshot,
                    tailscaleEndpointAvailable = True,
                    tailscaleEndpointReason = Nothing,
                    tailscaleEndpointSelf =
                      cachedNodeToPeer
                        (cachedNetworkMagicDNSName (activeProfileNetwork profile))
                        <$> netmapSnapshotSelf netmapSnapshot,
                    tailscaleEndpointPeers =
                      map
                        (cachedNodeToPeer (cachedNetworkMagicDNSName (activeProfileNetwork profile)))
                        (netmapSnapshotPeers netmapSnapshot)
                  }

data ActiveProfile = ActiveProfile
  { activeProfileID :: Text,
    activeProfileNetwork :: CachedNetworkProfile
  }

data CachedNetworkProfile = CachedNetworkProfile
  { cachedNetworkMagicDNSName :: Maybe Text,
    cachedNetworkDomainName :: Maybe Text,
    cachedNetworkDisplayName :: Maybe Text
  }

data NetmapSnapshot = NetmapSnapshot
  { netmapSnapshotTime :: Maybe UTCTime,
    netmapSnapshotSelf :: Maybe CachedNode,
    netmapSnapshotPeers :: [CachedNode]
  }

data StateStoreSnapshot = StateStoreSnapshot
  { stateStoreSnapshotTime :: Maybe UTCTime,
    stateStoreSnapshotValues :: Map Text BS.ByteString
  }

data CachedLoginProfile = CachedLoginProfile
  { cachedLoginProfileKey :: Maybe Text,
    cachedLoginProfileNetworkProfile :: Maybe CachedNetworkProfile
  }

data CachedNodeEnvelope = CachedNodeEnvelope
  { cachedNodeEnvelopeNode :: Maybe CachedNode
  }

data CachedNode = CachedNode
  { cachedNodeName :: Maybe Text,
    cachedNodeHostinfo :: Maybe CachedHostinfo,
    cachedNodeAddresses :: [Text],
    cachedNodeOnline :: Maybe Bool,
    cachedNodeLastSeen :: Maybe UTCTime,
    cachedNodeKeyExpiry :: Maybe UTCTime
  }

data CachedHostinfo = CachedHostinfo
  { cachedHostinfoHostname :: Maybe Text,
    cachedHostinfoOS :: Maybe Text
  }

instance Aeson.FromJSON CachedLoginProfile where
  parseJSON = Aeson.withObject "CachedLoginProfile" $ \obj ->
    CachedLoginProfile
      <$> obj .:? "Key"
      <*> obj .:? "NetworkProfile"

instance Aeson.FromJSON CachedNetworkProfile where
  parseJSON = Aeson.withObject "CachedNetworkProfile" $ \obj ->
    CachedNetworkProfile
      <$> obj .:? "MagicDNSName"
      <*> obj .:? "DomainName"
      <*> obj .:? "DisplayName"

instance Aeson.FromJSON CachedNodeEnvelope where
  parseJSON = Aeson.withObject "CachedNodeEnvelope" $ \obj ->
    CachedNodeEnvelope <$> obj .:? "Node"

instance Aeson.FromJSON CachedNode where
  parseJSON = Aeson.withObject "CachedNode" $ \obj ->
    CachedNode
      <$> obj .:? "Name"
      <*> obj .:? "Hostinfo"
      <*> (obj .:? "Addresses" .!= [])
      <*> obj .:? "Online"
      <*> obj .:? "LastSeen"
      <*> obj .:? "KeyExpiry"

instance Aeson.FromJSON CachedHostinfo where
  parseJSON = Aeson.withObject "CachedHostinfo" $ \obj ->
    CachedHostinfo
      <$> obj .:? "Hostname"
      <*> obj .:? "OS"

unavailableStatus :: Instance.TailscaleEndpointConfig -> FilePath -> Text -> TailscaleEndpointStatus
unavailableStatus endpoint stateDirectory reason =
  TailscaleEndpointStatus
    { tailscaleEndpointLabel = Instance.tailscaleEndpointLabel endpoint,
      tailscaleEndpointTag = Instance.tailscaleEndpointTag endpoint,
      tailscaleEndpointStateDirectory = stateDirectory,
      tailscaleEndpointSource = "state-cache",
      tailscaleEndpointTailnetName = Nothing,
      tailscaleEndpointMagicDNSSuffix = Nothing,
      tailscaleEndpointSnapshotTime = Nothing,
      tailscaleEndpointAvailable = False,
      tailscaleEndpointReason = Just reason,
      tailscaleEndpointSelf = Nothing,
      tailscaleEndpointPeers = []
    }

unavailableProfileStatus :: Instance.TailscaleEndpointConfig -> FilePath -> ActiveProfile -> Text -> TailscaleEndpointStatus
unavailableProfileStatus endpoint stateDirectory profile reason =
  (unavailableStatus endpoint stateDirectory reason)
    { tailscaleEndpointTailnetName = networkDisplayNameOrDefault (activeProfileNetwork profile),
      tailscaleEndpointMagicDNSSuffix = cachedNetworkMagicDNSName (activeProfileNetwork profile)
    }

loadStateStore :: FilePath -> IO (Either Text StateStoreSnapshot)
loadStateStore stateFile = do
  fileExists <- doesFileExist stateFile
  if not fileExists
    then pure (Left ("state file not found: " <> T.pack stateFile))
    else do
      stateTimeResult <- try (getModificationTime stateFile) :: IO (Either IOException UTCTime)
      bytesResult <- try (BS.readFile stateFile) :: IO (Either IOException BS.ByteString)
      let stateTime =
            case stateTimeResult of
              Left _ -> Nothing
              Right value -> Just value
      case bytesResult of
        Left err ->
          pure (Left ("failed to read state file: " <> T.pack (show err)))
        Right bytes ->
          pure $
            case eitherDecodeStrict' bytes of
              Left err ->
                Left ("failed to decode state file: " <> T.pack err)
              Right store ->
                fmap
                  (\values ->
                     StateStoreSnapshot
                       { stateStoreSnapshotTime = stateTime,
                         stateStoreSnapshotValues = values
                       }
                  )
                  (traverse decodeBase64TextMapValue (store :: Map Text Text))

selectActiveProfile :: Map Text BS.ByteString -> Either Text ActiveProfile
selectActiveProfile stateStore = do
  knownProfilesBytes <-
    maybe
      (Left "state store is missing _profiles")
      Right
      (Map.lookup "_profiles" stateStore)
  knownProfiles <-
    case eitherDecodeStrict' knownProfilesBytes of
      Left err ->
        Left ("failed to decode _profiles: " <> T.pack err)
      Right profiles ->
        Right (profiles :: Map Text CachedLoginProfile)
  let currentKey = fmap decodeStateValue (Map.lookup "_current-profile" stateStore)
      profilesWithIds =
        [ (profileID, profile)
        | (profileID, profile) <- Map.toList knownProfiles,
          maybe False (const True) (cachedLoginProfileNetworkProfile profile)
        ]
      selectedProfiles =
        case currentKey of
          Just keyValue ->
            [ (profileID, profile)
            | (profileID, profile) <- profilesWithIds,
              cachedLoginProfileKey profile == Just keyValue
            ]
          Nothing ->
            []
  case selectedProfiles of
    [(profileID, profile)] ->
      buildActiveProfile profileID profile
    []
      | [(profileID, profile)] <- profilesWithIds ->
          buildActiveProfile profileID profile
      | null profilesWithIds ->
          Left "state store does not contain any persisted tailscale profiles"
      | otherwise ->
          Left "current profile could not be inferred from state store"
    _ ->
      Left "multiple tailscale profiles match the current state"
  where
    decodeStateValue = TE.decodeUtf8With lenientDecode
    buildActiveProfile profileID profile =
      case cachedLoginProfileNetworkProfile profile of
        Just networkProfile ->
          Right
            ActiveProfile
              { activeProfileID = profileID,
                activeProfileNetwork = networkProfile
              }
        Nothing ->
          Left "current profile does not contain cached tailnet metadata"

loadNetmapCache :: FilePath -> ActiveProfile -> IO (Either Text NetmapSnapshot)
loadNetmapCache stateDirectory profile = do
  let cacheDirectory = stateDirectory </> "profile-data" </> T.unpack (activeProfileID profile) </> "netmap-cache"
  directoryExists <- doesDirectoryExist cacheDirectory
  if not directoryExists
    then pure (Left ("netmap cache not found: " <> T.pack cacheDirectory))
    else do
      entryNamesResult <- try (listDirectory cacheDirectory) :: IO (Either IOException [FilePath])
      case entryNamesResult of
        Left err ->
          pure (Left ("failed to list netmap cache: " <> T.pack (show err)))
        Right entryNames -> do
          entryResults <- mapM (loadNetmapEntry cacheDirectory) entryNames
          let snapshotTime =
                case catMaybes (map fst entryResults) of
                  [] -> Nothing
                  times -> Just (maximum times)
              entries = catMaybes (map snd entryResults)
              selfNode =
                listToMaybe
                  [ node
                  | NetmapEntry key node <- entries,
                    key == "self"
                  ]
              peerNodes =
                [ node
                | NetmapEntry key node <- entries,
                  "peer-" `T.isPrefixOf` key
                ]
          if isNothing selfNode && null peerNodes
            then pure (Left "netmap cache is empty")
            else
              pure
                ( Right
                    NetmapSnapshot
                      { netmapSnapshotTime = snapshotTime,
                        netmapSnapshotSelf = selfNode,
                        netmapSnapshotPeers = sortPeers peerNodes
                      }
                )

data NetmapEntry = NetmapEntry Text CachedNode

loadNetmapEntry :: FilePath -> FilePath -> IO (Maybe UTCTime, Maybe NetmapEntry)
loadNetmapEntry cacheDirectory entryName =
  case decodeHexText (T.pack entryName) of
    Nothing ->
      pure (Nothing, Nothing)
    Just key -> do
      let entryPath = cacheDirectory </> entryName
      timeResult <- try (getModificationTime entryPath) :: IO (Either IOException UTCTime)
      bytesResult <- try (BS.readFile entryPath) :: IO (Either IOException BS.ByteString)
      let maybeTime =
            case timeResult of
              Left _ -> Nothing
              Right timeValue -> Just timeValue
      case bytesResult of
        Left _ ->
          pure (maybeTime, Nothing)
        Right bytes ->
          case eitherDecodeStrict' bytes of
            Left _ ->
              pure (maybeTime, Nothing)
            Right envelope ->
              pure (maybeTime, fmap (NetmapEntry key) (cachedNodeEnvelopeNode (envelope :: CachedNodeEnvelope)))

sortPeers :: [CachedNode] -> [CachedNode]
sortPeers =
  sortOn
    (\node -> (Down (fromMaybe False (cachedNodeOnline node)), T.toCaseFold (cachedNodeDisplayName Nothing node)))

cachedNodeToPeer :: Maybe Text -> CachedNode -> TailscalePeer
cachedNodeToPeer magicDNSSuffix node =
  TailscalePeer
    { tailscalePeerName = cachedNodeDisplayName magicDNSSuffix node,
      tailscalePeerHostName = normalizeCachedText (cachedHostinfoHostname =<< cachedNodeHostinfo node),
      tailscalePeerDNSName = normalizeDNSName =<< normalizeCachedText (cachedNodeName node),
      tailscalePeerOS = normalizeCachedText (cachedHostinfoOS =<< cachedNodeHostinfo node),
      tailscalePeerIPs = map stripPrefixLength (cachedNodeAddresses node),
      tailscalePeerOnline = cachedNodeOnline node,
      tailscalePeerLastSeen = cachedNodeLastSeen node,
      tailscalePeerKeyExpiry = cachedNodeKeyExpiry node
    }

cachedNodeDisplayName :: Maybe Text -> CachedNode -> Text
cachedNodeDisplayName magicDNSSuffix node =
  fromMaybe "<unknown>" $
    shortMagicDNSName magicDNSSuffix =<< (normalizeDNSName =<< normalizeCachedText (cachedNodeName node))
      <|> normalizeCachedText (cachedHostinfoHostname =<< cachedNodeHostinfo node)
      <|> (normalizeDNSName =<< normalizeCachedText (cachedNodeName node))
      <|> listToMaybe (map stripPrefixLength (cachedNodeAddresses node))

shortMagicDNSName :: Maybe Text -> Text -> Maybe Text
shortMagicDNSName maybeSuffix dnsName =
  case maybeSuffix of
    Just suffix ->
      case T.stripSuffix ("." <> suffix) dnsName of
        Just shortName
          | not (T.null shortName) ->
              Just shortName
        _ ->
          Nothing
    Nothing ->
      Nothing

normalizeCachedText :: Maybe Text -> Maybe Text
normalizeCachedText maybeText =
  case fmap T.strip maybeText of
    Just trimmed
      | not (T.null trimmed) ->
          Just trimmed
    _ ->
      Nothing

normalizeDNSName :: Text -> Maybe Text
normalizeDNSName rawName =
  let normalized = T.dropWhileEnd (== '.') (T.strip rawName)
   in if T.null normalized then Nothing else Just normalized

stripPrefixLength :: Text -> Text
stripPrefixLength =
  T.takeWhile (/= '/')

decodeHexText :: Text -> Maybe Text
decodeHexText encoded =
  T.pack <$> go (T.unpack encoded)
  where
    go [] = Just []
    go [_] = Nothing
    go (left : right : rest) = do
      high <- hexDigitValue left
      low <- hexDigitValue right
      (toEnum (high * 16 + low) :) <$> go rest

    hexDigitValue char
      | '0' <= char && char <= '9' = Just (fromEnum char - fromEnum '0')
      | 'a' <= char && char <= 'f' = Just (10 + fromEnum char - fromEnum 'a')
      | 'A' <= char && char <= 'F' = Just (10 + fromEnum char - fromEnum 'A')
      | otherwise = Nothing

networkDisplayNameOrDefault :: CachedNetworkProfile -> Maybe Text
networkDisplayNameOrDefault networkProfile =
  cachedNetworkDisplayName networkProfile <|> cachedNetworkDomainName networkProfile

decodeBase64TextMapValue :: Text -> Either Text BS.ByteString
decodeBase64TextMapValue encodedValue =
  maybe
    (Left "failed to decode base64 state payload")
    Right
    (decodeBase64Text encodedValue)

decodeBase64Text :: Text -> Maybe BS.ByteString
decodeBase64Text =
  fmap BS.pack . go . T.unpack
  where
    go [] = Just []
    go [a, b, '=', '='] = do
      x1 <- base64Value a
      x2 <- base64Value b
      pure [fromIntegral ((x1 `shiftL` 2) .|. (x2 `shiftR` 4))]
    go [a, b, c, '='] = do
      x1 <- base64Value a
      x2 <- base64Value b
      x3 <- base64Value c
      pure
        [ fromIntegral ((x1 `shiftL` 2) .|. (x2 `shiftR` 4)),
          fromIntegral (((x2 .&. 0x0F) `shiftL` 4) .|. (x3 `shiftR` 2))
        ]
    go (a : b : c : d : rest) = do
      x1 <- base64Value a
      x2 <- base64Value b
      x3 <- base64Value c
      x4 <- base64Value d
      remaining <- go rest
      pure
        ( [ fromIntegral ((x1 `shiftL` 2) .|. (x2 `shiftR` 4)),
            fromIntegral (((x2 .&. 0x0F) `shiftL` 4) .|. (x3 `shiftR` 2)),
            fromIntegral (((x3 .&. 0x03) `shiftL` 6) .|. x4)
          ]
            <> remaining
        )
    go [_] = Nothing
    go [_, _] = Nothing
    go [_, _, _] = Nothing

    base64Value char
      | 'A' <= char && char <= 'Z' = Just (fromEnum char - fromEnum 'A')
      | 'a' <= char && char <= 'z' = Just (26 + fromEnum char - fromEnum 'a')
      | '0' <= char && char <= '9' = Just (52 + fromEnum char - fromEnum '0')
      | char == '+' = Just 62
      | char == '/' = Just 63
      | otherwise = Nothing
