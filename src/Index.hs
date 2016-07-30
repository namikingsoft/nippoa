{-# LANGUAGE CPP #-}
module Index
  ( execute
  ) where

import System.IO (hSetEncoding, stdout, utf8)
import System.Environment (getEnv)
import Data.Maybe
import Data.Time.Format

import Slack.Attachment
import Slack.GroupsList
import Slack.History
import Slack.Channel
import Slack.Message
import Slack.Network

execute :: IO ()
execute = do
    hSetEncoding stdout utf8
    token <- getEnv "SLACK_API_TOKEN"
    channelName <- getEnv "SLACK_CHANNEL_NAME"
    json <- getJsonFromGroupsList token
    let groupsList = parseGroupsList json
        maybeChannel = fromGroupsName groupsList channelName
        channel = fromMaybe (error "Channel Not Found") maybeChannel
    json <- getJsonFromGroupsHistory token (channelId channel)
    let messages = historyMessages $ parseHistory json
    mapM_ echoMessage messages

echoMessage :: Message -> IO ()
echoMessage x = do
    echoTime x
    putStr "  "
    echoText x
    putStrLn ""
    echoAttachments x
  where
    format = formatTime defaultTimeLocale "%F %T"
    echoTime = putStr . format . messageDateTime
    echoText = putStr . fromMaybe "" . messageText
    justAttachmentText = fromMaybe "" . attachmentText
    justAttachments = fromMaybe [] . messageAttachments
    echoAttachments x = mapM_ echoAttachment $ justAttachments x
    echoAttachment x = do
      putStr "> "
      putStrLn $ attachmentFallback x
      putStrLn . pre $ justAttachmentText x
      putStrLn ""
    pre x
      | x == "" = x
      | otherwise = "```\n" ++ x ++ "\n```"
