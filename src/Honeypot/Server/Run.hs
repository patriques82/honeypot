{-# LANGUAGE OverloadedStrings #-}
module Honeypot.Server.Run
  ( runServer
  ) where


import           Control.Concurrent                (threadDelay)
--import           Control.Monad
import           Data.ByteString.Builder           (string8)
import           Honeypot.Prelude
import           Network.HTTP.Types                (status200)
import           Network.Wai                       (Application, Middleware,
                                                    pathInfo, responseFile)
import           Network.Wai.EventSource           (ServerEvent (..),
                                                    eventSourceAppIO)

import           Network.Wai.Handler.Warp          (run)
import           Network.Wai.Middleware.AddHeaders (addHeaders)


runServer :: IO ()
runServer = run 3000 (headers app)

headers :: Middleware
headers = addHeaders [ ("X-Accel-Buffering", "no")
                     , ("Cache-Control", "no-cache")
                     ]

app :: Application
app req res =
  case pathInfo req of
    ["start"] -> eventSourceAppIO eventIO req res
    _         -> res $ responseFile status200 [("Content-Type", "text/html")] "web/index.html" Nothing

eventIO = do
  threadDelay 1000000
  putStrLn "sending"
  return $ ServerEvent Nothing Nothing [string8 "test"]
