{-# LANGUAGE OverloadedStrings #-}

module Boxctl.Instance
  ( normalizeControllerAddress,
    resolveInstance,
    stripJsonComments,
  )
where

import Boxctl.API (ResolvedInstance (..))
import Control.Applicative ((<|>))
import Data.Aeson ((.:?))
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Environment (lookupEnv)

defaultControllerAddress :: Text
defaultControllerAddress = "127.0.0.1:9090"

data ConfigFile = ConfigFile
  { configExperimental :: Maybe ExperimentalBlock
  }

data ExperimentalBlock = ExperimentalBlock
  { experimentalClashApi :: Maybe ClashAPIBlock
  }

data ClashAPIBlock = ClashAPIBlock
  { clashApiExternalController :: Maybe Text,
    clashApiSecret :: Maybe Text
  }

instance Aeson.FromJSON ConfigFile where
  parseJSON = Aeson.withObject "ConfigFile" $ \obj ->
    ConfigFile <$> obj .:? "experimental"

instance Aeson.FromJSON ExperimentalBlock where
  parseJSON = Aeson.withObject "ExperimentalBlock" $ \obj ->
    ExperimentalBlock <$> obj .:? "clash_api"

instance Aeson.FromJSON ClashAPIBlock where
  parseJSON = Aeson.withObject "ClashAPIBlock" $ \obj ->
    ClashAPIBlock
      <$> obj .:? "external_controller"
      <*> obj .:? "secret"

resolveInstance :: Maybe FilePath -> IO (Either Text ResolvedInstance)
resolveInstance cliInstance = do
  envInstance <- lookupEnv "BOXCTL_INSTANCE"
  envSecret <- fmap T.pack <$> lookupEnv "BOXCTL_SECRET"
  let instanceInput = cliInstance <|> envInstance <|> Just (T.unpack defaultControllerAddress)
  case instanceInput of
    Nothing ->
      pure (Left "failed to resolve target instance")
    Just target -> do
      directoryExists <- doesDirectoryExist target
      if directoryExists
        then pure (Left "instance path must be a config file, not a directory")
        else do
          fileExists <- doesFileExist target
          targetResult <-
            if fileExists
              then resolveFromConfig target
              else pure (resolveFromAddress (T.pack target))
          pure $
            fmap
              (\resolved -> resolved {resolvedSecret = envSecret <|> resolvedSecret resolved})
              targetResult

resolveFromConfig :: FilePath -> IO (Either Text ResolvedInstance)
resolveFromConfig configPath = do
  configBytes <- BS.readFile configPath
  let sanitized = stripJsonComments configBytes
  pure $ do
    config <- first (("failed to parse config file: " <>) . T.pack) (Aeson.eitherDecodeStrict' sanitized)
    clashApi <- maybe (Left "config file does not contain experimental.clash_api") Right (configExperimental config >>= experimentalClashApi)
    externalController <-
      maybe
        (Left "config file does not contain experimental.clash_api.external_controller")
        Right
        (clashApiExternalController clashApi)
    normalized <- resolveFromAddress externalController
    Right normalized {resolvedSecret = clashApiSecret clashApi}

resolveFromAddress :: Text -> Either Text ResolvedInstance
resolveFromAddress rawAddress = do
  baseUrl <- normalizeControllerAddress rawAddress
  Right ResolvedInstance {resolvedBaseUrl = baseUrl, resolvedSecret = Nothing}

normalizeControllerAddress :: Text -> Either Text Text
normalizeControllerAddress rawValue
  | T.null trimmed = Left "instance address is empty"
  | "://" `T.isInfixOf` trimmed = Right (T.dropWhileEnd (== '/') trimmed)
  | T.isPrefixOf "[" trimmed = normalizeBracketedAddress trimmed
  | otherwise = normalizeSimpleAddress trimmed
  where
    trimmed = T.strip rawValue

normalizeSimpleAddress :: Text -> Either Text Text
normalizeSimpleAddress rawAddress =
  case T.splitOn ":" rawAddress of
    [host] ->
      buildBaseUrl (normalizeHost False host) "9090"
    ["", port] ->
      buildBaseUrl "127.0.0.1" port
    [host, port] ->
      buildBaseUrl (normalizeHost False host) port
    _ ->
      Left "invalid instance address; bracket IPv6 literals like [::1]:9090"

normalizeBracketedAddress :: Text -> Either Text Text
normalizeBracketedAddress rawAddress =
  let (rawHost, rest) = T.breakOn "]" (T.drop 1 rawAddress)
   in if T.null rest
        then Left "invalid bracketed instance address"
        else
          let portPart = T.drop 1 rest
              host = normalizeHost True rawHost
           in case portPart of
                "" -> buildBaseUrl host "9090"
                p ->
                  case T.stripPrefix ":" p of
                    Just port -> buildBaseUrl host port
                    Nothing -> Left "invalid bracketed instance address"

buildBaseUrl :: Text -> Text -> Either Text Text
buildBaseUrl host port
  | T.null host = Left "instance host is empty"
  | not (validPort port) = Left "instance port must be in the range 1-65535"
  | needsBrackets host = Right ("http://[" <> host <> "]:" <> port)
  | otherwise = Right ("http://" <> host <> ":" <> port)

validPort :: Text -> Bool
validPort port
  | T.null port = False
  | T.all isDigit port =
      let value = read (T.unpack port) :: Int
       in value >= 1 && value <= 65535
  | otherwise = False

normalizeHost :: Bool -> Text -> Text
normalizeHost bracketed rawHost =
  case T.strip rawHost of
    "" -> "127.0.0.1"
    "*" -> loopback
    "0.0.0.0" -> "127.0.0.1"
    "::" -> "::1"
    "[::]" -> "::1"
    host -> host
  where
    loopback
      | bracketed = "::1"
      | otherwise = "127.0.0.1"

needsBrackets :: Text -> Bool
needsBrackets host = T.count ":" host > 1

stripJsonComments :: BS.ByteString -> BS.ByteString
stripJsonComments = BS.pack . goNormal . BS.unpack
  where
    goNormal [] = []
    goNormal (34 : rest) = 34 : goString False rest
    goNormal (47 : 47 : rest) = goLineComment rest
    goNormal (47 : 42 : rest) = goBlockComment rest
    goNormal (char : rest) = char : goNormal rest

    goString _ [] = []
    goString escaped (char : rest)
      | escaped = char : goString False rest
      | char == 92 = char : goString True rest
      | char == 34 = char : goNormal rest
      | otherwise = char : goString False rest

    goLineComment [] = []
    goLineComment (char : rest)
      | char == 10 = 10 : goNormal rest
      | char == 13 = 13 : goNormal rest
      | otherwise = goLineComment rest

    goBlockComment [] = []
    goBlockComment (42 : 47 : rest) = goNormal rest
    goBlockComment (char : rest)
      | char == 10 = 10 : goBlockComment rest
      | char == 13 = 13 : goBlockComment rest
      | otherwise = goBlockComment rest
