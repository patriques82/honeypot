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
runServer = run 3000 $ static (headers app)

-- for proxy servers
headers :: Middleware
headers = addHeaders [ ("X-Accel-Buffering", "no")
                     , ("Cache-Control", "no-cache")
                     ]

-- add static path handler
static :: Middleware
static = staticPolicy $ noDots >-> addBase "web/dist"

--static :: Middleware
--static = staticPolicy $ only [("main.js", "web/dist/main.js"), ("index.html", "web/index.html")]

app :: Application
app req res = do
	print (pathInfo req)
	case pathInfo req of
		["start"] ->
			eventSourceAppIO eventIO req res
		_         ->
			res $ responseLBS status404 [("Content-Type", "text/html")] "Not Found"

eventIO = do
  threadDelay 1000000
  return $ ServerEvent Nothing Nothing [string8 "test"]
