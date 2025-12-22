{-# LANGUAGE OverloadedStrings #-}

module Boxctl.Instance
  ( normalizeControllerAddress,
    resolveInstance,
    stripJsonComments,
  )
where

import Boxctl.API (ResolvedInstance (..))
import Boxctl.Error (InstanceError (..))
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..))
import Data.Aeson ((.:?))
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.Char (isDigit, toLower)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath (hasDrive, isPathSeparator, takeExtension)
import Text.Read (readMaybe)

defaultControllerAddress :: Text
defaultControllerAddress = "127.0.0.1:9090"

newtype ConfigFile = ConfigFile
  { configExperimental :: Maybe ExperimentalBlock
  }

newtype ExperimentalBlock = ExperimentalBlock
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

resolveInstance :: Maybe FilePath -> ExceptT InstanceError IO ResolvedInstance
resolveInstance cliInstance = do
  envInstance <- liftIO (lookupEnv "BOXCTL_INSTANCE")
  envSecret <- liftIO (fmap T.pack <$> lookupEnv "BOXCTL_SECRET")
  let target = fromMaybe (T.unpack defaultControllerAddress) (cliInstance <|> envInstance)
  directoryExists <- liftIO (doesDirectoryExist target)
  if directoryExists
    then ExceptT (pure (Left InstancePathIsDirectory))
    else do
      fileExists <- liftIO (doesFileExist target)
      resolved <-
        if fileExists
          then resolveFromConfig target
          else
            exceptEither $
              if looksLikeConfigPath target
                then Left (InstanceConfigFileNotFound target)
                else resolveFromAddress (T.pack target)
      pure resolved {resolvedSecret = envSecret <|> resolvedSecret resolved}

looksLikeConfigPath :: FilePath -> Bool
looksLikeConfigPath target =
  not ("://" `isInfixOf` target)
    && ( hasDrive target
           || any isPathSeparator target
           || map toLower (takeExtension target) `elem` [".json", ".jsonc"]
       )

resolveFromConfig :: FilePath -> ExceptT InstanceError IO ResolvedInstance
resolveFromConfig configPath = do
  configBytes <- liftIO (BS.readFile configPath)
  let sanitized = stripJsonComments configBytes
  exceptEither $ do
    config <- first (InstanceConfigParseError . T.pack) (Aeson.eitherDecodeStrict' sanitized)
    clashApi <- maybe (Left MissingClashApiConfig) Right (configExperimental config >>= experimentalClashApi)
    externalController <- maybe (Left MissingExternalController) Right (clashApiExternalController clashApi)
    normalized <- resolveFromAddress externalController
    Right normalized {resolvedSecret = clashApiSecret clashApi}

resolveFromAddress :: Text -> Either InstanceError ResolvedInstance
resolveFromAddress rawAddress = do
  baseUrl <- normalizeControllerAddress rawAddress
  Right ResolvedInstance {resolvedBaseUrl = baseUrl, resolvedSecret = Nothing}

normalizeControllerAddress :: Text -> Either InstanceError Text
normalizeControllerAddress rawValue
  | T.null trimmed = Left (InvalidInstanceAddress "instance address is empty")
  | "://" `T.isInfixOf` trimmed = Right (T.dropWhileEnd (== '/') trimmed)
  | T.isPrefixOf "[" trimmed = normalizeBracketedAddress trimmed
  | otherwise = normalizeSimpleAddress trimmed
  where
    trimmed = T.strip rawValue

normalizeSimpleAddress :: Text -> Either InstanceError Text
normalizeSimpleAddress rawAddress =
  case T.splitOn ":" rawAddress of
    [host] ->
      buildBaseUrl (normalizeHost False host) "9090"
    ["", port] ->
      buildBaseUrl "127.0.0.1" port
    [host, port] ->
      buildBaseUrl (normalizeHost False host) port
    _ ->
      Left (InvalidInstanceAddress "invalid instance address; bracket IPv6 literals like [::1]:9090")

normalizeBracketedAddress :: Text -> Either InstanceError Text
normalizeBracketedAddress rawAddress =
  let (rawHost, rest) = T.breakOn "]" (T.drop 1 rawAddress)
   in if T.null rest
        then Left (InvalidInstanceAddress "invalid bracketed instance address")
        else
          let portPart = T.drop 1 rest
              host = normalizeHost True rawHost
           in case portPart of
                "" -> buildBaseUrl host "9090"
                p ->
                  case T.stripPrefix ":" p of
                    Just port -> buildBaseUrl host port
                    Nothing -> Left (InvalidInstanceAddress "invalid bracketed instance address")

buildBaseUrl :: Text -> Text -> Either InstanceError Text
buildBaseUrl host port
  | T.null host = Left (InvalidInstanceAddress "instance host is empty")
  | not (validPort port) = Left (InvalidInstanceAddress "instance port must be in the range 1-65535")
  | needsBrackets host = Right ("http://[" <> host <> "]:" <> port)
  | otherwise = Right ("http://" <> host <> ":" <> port)

validPort :: Text -> Bool
validPort port
  | T.null port = False
  | T.all isDigit port =
      case readMaybe (T.unpack port) of
        Just value -> value >= (1 :: Int) && value <= 65535
        Nothing -> False
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

exceptEither :: Monad m => Either e a -> ExceptT e m a
exceptEither =
  ExceptT . pure
