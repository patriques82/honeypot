{-# LANGUAGE OverloadedStrings #-}

module Honeypot.Server.Run
  ( runServer
  ) where

import           Control.Concurrent                (forkIO, threadDelay)
import           Control.Concurrent.Chan           (Chan, newChan, readChan,
                                                    writeChan)
import           Data.Binary.Builder               (Builder, fromLazyByteString,
                                                    putStringUtf8)
import qualified Honeypot.Config.Config            as C
import           Honeypot.Core.Api                 (exec)
import           Honeypot.Prelude
import           Honeypot.Types
import           Network.HTTP.Types                (status200, status404)
import           Network.Wai                       (Application, Middleware,
                                                    pathInfo, responseFile,
                                                    responseLBS)
import           Network.Wai.EventSource           (ServerEvent (..),
                                                    eventSourceAppChan)

import qualified Network.Wai.Handler.Warp          as W (run)
import           Network.Wai.Middleware.AddHeaders (addHeaders)

import           Network.Wai.Middleware.Static     (addBase, noDots, only,
                                                    staticPolicy, (>->))

import           Data.Aeson                        (ToJSON, encode)

runServer :: Step Event -> C.Config -> IO ()
runServer step conf =
  case C.runConfig conf of
    Right state -> run state step
    Left err    -> print err

run :: GameState -> Step Event -> IO ()
run state step = do
  chan <- newChan
  forkIO $ producer chan state step
  putStrLn "Running on 3000"
  W.run 3000 . static . headers $ consumer chan
  where
    -- for proxy servers
    headers :: Middleware
    headers = addHeaders [ ("X-Accel-Buffering", "no")
                         , ("Cache-Control", "no-cache")
                         ]
    -- add static path handler
    static :: Middleware
    static = staticPolicy $ noDots >-> addBase "web/dist"

type EventName = String

gameEvent :: ToJSON a => a -> EventName -> ServerEvent
gameEvent x name =
  let payload = [fromLazyByteString (encode x)]
      tag = Just . putStringUtf8
   in ServerEvent (tag name) Nothing payload

producer :: Chan ServerEvent -> GameState -> Step Event -> IO ()
producer chan state step = loop state
  where
    loop s =
      case exec s step of
        GameOver status -> do
          putStrLn "Game over"
          writeChan chan $ gameEvent status "gameover"
          writeChan chan CloseEvent
        s'@(Continue env) -> do
          putStrLn "Sending state"
          writeChan chan $ gameEvent env "data"
          threadDelay 1000000
          loop s'

consumer :: Chan ServerEvent -> Application
consumer chan req res =
  case pathInfo req of
    ["start"] ->
      eventSourceAppChan chan req res
    _         ->
      res $ responseLBS status404 [("Content-Type", "text/html")] "Not Found"

