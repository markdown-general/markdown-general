{-# LANGUAGE OverloadedStrings #-}

module GhciMcp.Process
  ( GhciProcess
  , startGhci
  , stopGhci
  , sendCommand
  , isRunning
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (try, SomeException)
import System.Process
import System.IO
import qualified Data.Text as T
import Data.Text (Text)

-- | Ghci process handle
data GhciProcess = GhciProcess
  { ghciStdin :: Handle
  , ghciStdout :: Handle
  , ghciStderr :: Handle
  , ghciProcessHandle :: ProcessHandle
  , ghciRunning :: TVar Bool
  }

-- | Start a ghci process
startGhci :: IO (Maybe GhciProcess)
startGhci = do
  result <- try $ createProcess (proc "ghci" [])
    { std_in = CreatePipe
    , std_out = CreatePipe
    , std_err = CreatePipe
    }
  case result of
    Left (e :: SomeException) -> do
      hPutStrLn stderr $ "Failed to start ghci: " ++ show e
      return Nothing
    Right (Just hIn, Just hOut, Just hErr, ph) -> do
      -- Set buffering
      hSetBuffering hIn LineBuffering
      hSetBuffering hOut LineBuffering
      hSetBuffering hErr LineBuffering
      
      -- Create process handle
      running <- newTVarIO True
      let ghci = GhciProcess hIn hOut hErr ph running
      
      return $ Just ghci
    Right _ -> do
      hPutStrLn stderr "Failed to create ghci pipes"
      return Nothing

-- | Stop a ghci process
stopGhci :: GhciProcess -> IO ()
stopGhci ghci = do
  atomically $ writeTVar (ghciRunning ghci) False
  terminateProcess (ghciProcessHandle ghci)
  hClose (ghciStdin ghci)
  hClose (ghciStdout ghci)
  hClose (ghciStderr ghci)

-- | Send a command to ghci
sendCommand :: GhciProcess -> Text -> IO ()
sendCommand ghci cmd = do
  hPutStrLn (ghciStdin ghci) (T.unpack cmd)
  hFlush (ghciStdin ghci)

-- | Check if ghci process is still running
isRunning :: GhciProcess -> IO Bool
isRunning ghci = do
  running <- readTVarIO (ghciRunning ghci)
  if not running
    then return False
    else do
      exitCode <- getProcessExitCode (ghciProcessHandle ghci)
      case exitCode of
        Nothing -> return True
        Just _ -> do
          atomically $ writeTVar (ghciRunning ghci) False
          return False
