{-# LANGUAGE OverloadedStrings     #-}

module Main where

import qualified HoogleApi as H

import Control.Monad
import Control.Monad.STM
import Control.Concurrent.Async (async, wait)
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM.TChan
import qualified Data.Text.IO as T
import Data.Text (Text, unpack, pack)
import Data.Maybe
import Data.Monoid ((<>))
import Web.Telegram.API.Bot

token :: Token
token = Token ""

pollTimeout :: Int
pollTimeout = 300 -- seconds

main :: IO ()
main = do
  broadcastChan <- newTChanIO
  _ <- forkIO $ retrieveMessages broadcastChan Nothing
  _ <- forkIO $ answerMessages broadcastChan
  _ <- getLine
  return ()

answerMessages :: TChan Message -> IO ()
answerMessages broadcastChan = do
  chan <- atomically $ dupTChan broadcastChan -- dupTChan for every recursive call ?
  forever $ do
    msg <- atomically $ readTChan chan
    respondToMsg msg

-- |Retrieve messages via Telegram API
retrieveMessages :: TChan Message -> Maybe Int -> IO ()
retrieveMessages broadcastChan offset = do
  putStrLn $ "Retrieving with max offset " <> show (maybe 0 id offset)
  resp <- getUpdates token offset Nothing (Just pollTimeout) -- Longpoll
  case resp of
    Left err -> do
      putStrLn $ show err
      retrieveMessages broadcastChan offset
    Right r -> do
      putStrLn $ show r
      mapM_ writeMsg (filterMessages r)
      retrieveMessages broadcastChan (nextOffset r)
  where  writeMsg m = atomically $ writeTChan broadcastChan m
         nextOffset r = Just $ (+1) (maxOffset r)

-- |Filter for non empty messages
filterMessages :: UpdatesResponse -> [Message]
filterMessages response = catMaybes $ map message (update_result response)

-- |Determine highest offset in update.
maxOffset :: UpdatesResponse -> Int
maxOffset response = maximum $ map update_id (update_result response)

-- |Format bot response
formatHoogleResponse :: H.HoogleQuery -> Text
formatHoogleResponse qry =
  let formatRes r = "\n[" <> (H.self r) <> "]("
                 <> (H.location r) <> ")"
                 <> "```" <> (H.docs r) <> "```"
    in mconcat $ map formatRes $ H.results qry

-- |Respond to a msg send by a client
respondToMsg :: Message-> IO ()
respondToMsg req = do
  h <- async $ H.query (fromJust $ text req) Nothing Nothing -- avoid fromJust
  hoogleResp <- wait h
  sr <- async $ sendMessage token (messageReq $ answer hoogleResp)
  sendResp <- wait sr
  case sendResp of
      Left err -> putStrLn $ show err
      Right m-> putStrLn $ show $ message_id $ message_result m
  where chatId = pack $ show $ chat_id $ chat req
        replyToMsgId = Just (message_id req)
        markdown = Just Markdown
        answer hr = either (\_ -> "Can not reach hoogle") formatHoogleResponse hr
        messageReq a = SendMessageRequest chatId a markdown Nothing replyToMsgId Nothing

