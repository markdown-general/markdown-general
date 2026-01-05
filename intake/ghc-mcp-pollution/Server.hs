{-# LANGUAGE OverloadedStrings #-}

module GhciMcp.Server
  ( runGhciMcpServer
  ) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import System.IO (hPutStrLn, stderr)

import MCP.Async
import GhciMcp.Tools

-- | Server information
serverInfo :: Implementation
serverInfo = Implementation "ghci-mcp" "0.1.0"

-- | Server capabilities
serverCapabilities :: ServerCapabilities
serverCapabilities = ServerCapabilities 
  { capTools = Just $ ToolsCapability Nothing
  }

-- | Run the ghci MCP server
runGhciMcpServer :: IO ()
runGhciMcpServer =
  runMcpServer serverInfo serverCapabilities handleRequest handleNotification

-- | Handle MCP requests
handleRequest :: ServerContext -> RequestId -> Text -> Maybe Value -> IO ()
handleRequest ctx reqId method params =
  case method of
    "tools/list" -> handleToolsList ctx reqId
    "tools/call" -> case params of
      Nothing -> sendInvalidParams ctx reqId "Missing params"
      Just p -> handleToolCall ctx reqId p
    "ping" -> sendResponse ctx reqId Null
    _ -> sendMethodNotFound ctx reqId

-- | Handle tools/list request
handleToolsList :: ServerContext -> RequestId -> IO ()
handleToolsList ctx reqId = do
  let result = object
        [ "tools" .= toolsList
        ]
  sendResponse ctx reqId result

-- | Handle notifications from client
handleNotification :: ServerContext -> Text -> Maybe Value -> IO ()
handleNotification ctx method params = do
  -- Log to stderr (stdout is reserved for JSON-RPC)
  hPutStrLn stderr $ "Received notification: " ++ T.unpack method
  return ()
