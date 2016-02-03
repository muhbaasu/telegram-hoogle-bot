{-# LANGUAGE OverloadedStrings     #-}

module Main where

import qualified HoogleApi as H

import Control.Monad
import Control.Monad.STM
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
    hoogleRes <- H.query (fromJust $ text msg) Nothing Nothing -- avoid fromJust
    putStrLn $ show hoogleRes
    respond msg (either err formatHoogleResponse hoogleRes)
    where err = \_ -> "Failed to contact hoogle"

retrieveMessages :: TChan Message -> Maybe Int -> IO ()
retrieveMessages broadcastChan offset = do
  putStrLn $ "Retrieving with max offset " <> show (maybe 0 id offset)
  resp <- getUpdates token offset Nothing (Just pollTimeout) -- Longpoll
  case resp of
    Left err -> do
      putStrLn $ show err
      threadDelay $ 5 * (10 ^ 6) -- 5 seconds in microseconds
      retrieveMessages broadcastChan offset
    Right r -> do
      putStrLn $ show r
      let msgs = catMaybes $ map message (update_result r)
          writeMsg m = atomically $ writeTChan broadcastChan m
        in mapM_ writeMsg msgs
      let maxOffset = maximum $ map update_id (update_result r)
        in retrieveMessages broadcastChan $ Just (maxOffset + 1)

formatHoogleResponse :: H.HoogleQuery -> Text
formatHoogleResponse qry =
  let formatRes r = "\n[" <> (H.self r) <> "]("
                 <> (H.location r) <> ")"
                 <> "```" <> (H.docs r) <> "```"
    in mconcat $ map formatRes $ H.results qry

respond :: Message -> Text-> IO ()
respond req answer = do
  resp <- sendMessage token messageReq
  case resp of
      Left err -> putStrLn $ show err
      Right m-> putStrLn $ show $ message_id $ message_result m
   where chatId = pack $ show $ chat_id $ chat req
         replyToMsgId = Just (message_id req)
         markdown = Just Markdown
         messageReq = SendMessageRequest chatId answer markdown Nothing replyToMsgId Nothing

