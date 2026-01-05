{-# LANGUAGE OverloadedStrings #-}

module GhciMcp.Tools
  ( toolsList
  , handleToolCall
  ) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import Control.Concurrent.STM
import Control.Concurrent (forkIO, threadDelay)
import System.IO
import System.IO.Unsafe (unsafePerformIO)

import MCP.Async
import GhciMcp.Process

-- | Helper to create MCP text content response
mcpTextContent :: Text -> Value
mcpTextContent msg = object
  [ "content" .= 
      [ object
          [ "type" .= ("text" :: Text)
          , "text" .= msg
          ]
      ]
  ]

-- | Global ghci process state
-- In a real implementation, this would be in ReaderT or passed explicitly
{-# NOINLINE ghciProcessVar #-}
ghciProcessVar :: TVar (Maybe GhciProcess)
ghciProcessVar = unsafePerformIO $ newTVarIO Nothing

-- | List of available tools
toolsList :: [Tool]
toolsList =
  [ Tool
      { toolName = "start"
      , toolDescription = Just "Start a ghci REPL process"
      , toolInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object []
          ]
      }
  , Tool
      { toolName = "send"
      , toolDescription = Just "Send a command to ghci"
      , toolInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "command" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("GHCi command to execute" :: Text)
                  ]
              ]
          , "required" .= (["command"] :: [Text])
          ]
      }
  , Tool
      { toolName = "listen"
      , toolDescription = Just "Listen for output from ghci (returns immediately, sends notifications)"
      , toolInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object []
          ]
      }
  , Tool
      { toolName = "stop"
      , toolDescription = Just "Stop the ghci process"
      , toolInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object []
          ]
      }
  , Tool
      { toolName = "status"
      , toolDescription = Just "Check if ghci process is running"
      , toolInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object []
          ]
      }
  ]

-- | Handle a tool call
handleToolCall :: ServerContext -> RequestId -> Value -> IO ()
handleToolCall ctx reqId params = do
  case fromJSON params of
    Error err -> sendInvalidParams ctx reqId (T.pack err)
    Success toolCall -> dispatchTool ctx reqId toolCall

-- | Dispatch to specific tool handler
dispatchTool :: ServerContext -> RequestId -> ToolCall -> IO ()
dispatchTool ctx reqId toolCall =
  case callName toolCall of
    "start" -> handleStart ctx reqId
    "send" -> handleSend ctx reqId (callArguments toolCall)
    "listen" -> handleListen ctx reqId
    "stop" -> handleStop ctx reqId
    "status" -> handleStatus ctx reqId
    _ -> sendMethodNotFound ctx reqId

-- | Handle 'start' tool
handleStart :: ServerContext -> RequestId -> IO ()
handleStart ctx reqId = do
  maybeGhci <- readTVarIO ghciProcessVar
  case maybeGhci of
    Just _ -> do
      -- Already running
      sendResponse ctx reqId $ mcpTextContent "GHCi is already running"
    Nothing -> do
      -- Start ghci
      result <- startGhci
      case result of
        Nothing -> 
          sendError ctx reqId internalError "Failed to start ghci" Nothing
        Just ghci -> do
          atomically $ writeTVar ghciProcessVar (Just ghci)
          sendNotification ctx "ghci/started" Nothing
          sendResponse ctx reqId $ mcpTextContent "Started GHCi"

-- | Handle 'send' tool
handleSend :: ServerContext -> RequestId -> Maybe Value -> IO ()
handleSend ctx reqId args = do
  case args >>= parseMaybe (.: "command") of
    Nothing -> sendInvalidParams ctx reqId "Missing 'command' parameter"
    Just cmd -> do
      maybeGhci <- readTVarIO ghciProcessVar
      case maybeGhci of
        Nothing -> 
          sendError ctx reqId internalError "Ghci not running" Nothing
        Just ghci -> do
          sendCommand ghci cmd
          sendNotification ctx "ghci/command_sent" $ Just $ object ["command" .= cmd]
          sendResponse ctx reqId $ mcpTextContent $ "Sent command: " <> cmd

-- | Handle 'listen' tool - reads available output and sends as notifications
handleListen :: ServerContext -> RequestId -> IO ()
handleListen ctx reqId = do
  maybeGhci <- readTVarIO ghciProcessVar
  case maybeGhci of
    Nothing ->
      sendError ctx reqId internalError "Ghci not running" Nothing
    Just ghci -> do
      -- Respond immediately - we'll send output via notifications
      sendResponse ctx reqId $ mcpTextContent "Listening for GHCi output..."
      
      -- Spawn thread to read output
      forkIO $ readOutput ctx ghci
      return ()

-- | Read output from ghci and send as notifications
readOutput :: ServerContext -> GhciProcess -> IO ()
readOutput ctx ghci = do
  -- Try to read line (with timeout would be better in production)
  ready <- hReady (ghciStdout ghci)
  if ready
    then do
      line <- hGetLine (ghciStdout ghci)
      sendNotification ctx "ghci/output" $ Just $ object 
        [ "text" .= T.pack line
        , "stream" .= ("stdout" :: Text)
        ]
      -- Continue reading
      readOutput ctx ghci
    else do
      -- No data available, sleep briefly
      threadDelay 100000  -- 100ms
      running <- isRunning ghci
      if running
        then readOutput ctx ghci
        else sendNotification ctx "ghci/stopped" Nothing

-- | Handle 'stop' tool
handleStop :: ServerContext -> RequestId -> IO ()
handleStop ctx reqId = do
  maybeGhci <- readTVarIO ghciProcessVar
  case maybeGhci of
    Nothing ->
      sendResponse ctx reqId $ mcpTextContent "GHCi is not running"
    Just ghci -> do
      stopGhci ghci
      atomically $ writeTVar ghciProcessVar Nothing
      sendNotification ctx "ghci/stopped" Nothing
      sendResponse ctx reqId $ mcpTextContent "Stopped GHCi"

-- | Handle 'status' tool
handleStatus :: ServerContext -> RequestId -> IO ()
handleStatus ctx reqId = do
  maybeGhci <- readTVarIO ghciProcessVar
  case maybeGhci of
    Nothing ->
      sendResponse ctx reqId $ mcpTextContent "GHCi is not running"
    Just ghci -> do
      running <- isRunning ghci
      if running
        then sendResponse ctx reqId $ mcpTextContent "GHCi is running"
        else sendResponse ctx reqId $ mcpTextContent "GHCi process has terminated"

parseMaybe :: (Value -> Parser a) -> Value -> Maybe a
parseMaybe p v = case parse p v of
  Success a -> Just a
  Error _ -> Nothing
