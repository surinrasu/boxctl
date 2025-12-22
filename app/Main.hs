{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Boxctl.CLI as CLI
import qualified Boxctl.Command as Command
import Boxctl.Error (renderBoxctlError)
import Control.Monad.Trans.Except (runExceptT)
import qualified Data.Text.IO as TIO
import System.Exit (exitFailure)
import System.IO (stderr)

main :: IO ()
main = do
  options <- CLI.parseOptions
  result <- runExceptT (Command.run options)
  case result of
    Left err -> do
      TIO.hPutStrLn stderr ("boxctl: " <> renderBoxctlError err)
      exitFailure
    Right () ->
      pure ()
