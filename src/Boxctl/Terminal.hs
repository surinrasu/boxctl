{-# LANGUAGE OverloadedStrings #-}

module Boxctl.Terminal
  ( resolveRenderStyle,
  )
where

import Boxctl.CLI
import Boxctl.Output (BackgroundTone (..), RenderStyle (..))
import Control.Applicative ((<|>))
import Control.Exception (bracket)
import Control.Monad (unless)
import qualified Data.ByteString.Char8 as BS8
import Data.Char (digitToInt, isHexDigit, toLower)
import Data.List (stripPrefix)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import System.IO (hIsTerminalDevice, stdout)
import System.IO.Error (catchIOError)
import System.Posix.IO (OpenMode (ReadWrite), closeFd, defaultFileFlags, fdWrite, openFd)
import qualified System.Posix.IO.ByteString as PosixBS
import System.Posix.Terminal
  ( TerminalAttributes,
    TerminalMode (EnableEcho, ExtendedFunctions, KeyboardInterrupts, ProcessInput),
    TerminalState (Immediately),
    getControllingTerminalName,
    getTerminalAttributes,
    queryTerminal,
    setTerminalAttributes,
    withMinInput,
    withTime,
    withoutMode
  )
import System.Posix.Types (Fd)

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
        (List.foldl' withoutMode original [ProcessInput, EnableEcho, KeyboardInterrupts, ExtendedFunctions])
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
      let value = fromIntegral (List.foldl' step 0 digits) :: Double
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
      case (reads lastToken :: [(Int, String)]) of
        [(code, "")]
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
