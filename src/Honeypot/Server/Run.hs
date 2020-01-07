{-# LANGUAGE OverloadedStrings #-}
module Honeypot.Server.Run
  ( runServer
  ) where


import           Control.Concurrent                (threadDelay)
import           Data.ByteString.Builder           (string8)
import           Honeypot.Prelude
import           Network.HTTP.Types                (status200, status404)
import           Network.Wai                       (Application, Middleware,
                                                    pathInfo, responseFile,
                                                    responseLBS)
import           Network.Wai.EventSource           (ServerEvent (..),
                                                    eventSourceAppIO)

import           Network.Wai.Handler.Warp          (run)
import           Network.Wai.Middleware.AddHeaders (addHeaders)

import           Network.Wai.Middleware.Static     (addBase, noDots, only,
                                                    staticPolicy, (>->))

import           System.Directory                  (getCurrentDirectory)

runServer :: IO ()
runServer = -- do
  --dir <- getCurrentDirectory
  --let staticPath = dir ++ "/static"
  run 3000 . static2 . headers $ app

-- for proxy servers
headers :: Middleware
headers = addHeaders [ ("X-Accel-Buffering", "no")
                     , ("Cache-Control", "no-cache")
                     ]

-- add static path handler
static :: Middleware
static = staticPolicy $ noDots >-> addBase "static"

static2 :: Middleware
static2 = staticPolicy (only [("display.js", "static/display.js"), ("index.html", "static/index.html")])



app :: Application
app req res =
  case pathInfo req of
    ["start"] -> do
      putStrLn "started"
      eventSourceAppIO eventIO req res
    _         ->
      res $ responseLBS status404 [("Content-Type", "text/html")] "Not Found"
      --putStrLn "served"
      --res $ responseFile status200 [("Content-Type", "text/html")] "./static/index.html" Nothing

eventIO = do
  threadDelay 1000000
  return $ ServerEvent Nothing Nothing [string8 "test"]
