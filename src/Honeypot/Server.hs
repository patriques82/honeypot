{-# LANGUAGE OverloadedStrings #-}

module Honeypot.Server
  ( runServer
  ) where

import           Control.Concurrent                (forkIO, threadDelay)
import           Control.Concurrent.Chan           (Chan, newChan, readChan,
                                                    writeChan)
import           Control.Monad                     (mapM_)
import           Data.Binary.Builder               (Builder, fromLazyByteString,
                                                    putStringUtf8)
import           Data.Text                         (unpack)
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
  putStrLn "Running on 3000"
  W.run 3000 . static . headers $ app state step
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
      tag = Just $ putStringUtf8 name
   in ServerEvent tag Nothing payload

producer :: Chan ServerEvent -> GameState -> Step Event -> IO ()
producer chan state step = loop state
  where
    loop s =
      case exec s step of
        s'@(GameOver _ _) -> do
          putStrLn "Game over"
          writeChan chan $ gameEvent s' "gameover"
          writeChan chan CloseEvent
        s -> do
          putStrLn "Sending state"
          writeChan chan $ gameEvent s "data"
          threadDelay 1000000
          loop s

app :: GameState -> Step Event -> Application
app state step req res =
  case pathInfo req of
    []         ->
      res $ responseFile status200 [("Content-Type", "text/html")] "web/dist/index.html" Nothing
    ["init"] -> do
      putStrLn "init"
      res $ responseLBS status200 [("Content-Type", "application/json")] (encode $ getEnv state)
    ["start"] -> do
      putStrLn "start"
      chan <- newChan
      forkIO $ producer chan state step
      eventSourceAppChan chan req res
    _         ->
      res $ responseLBS status404 [("Content-Type", "text/html")] "Not Found"

