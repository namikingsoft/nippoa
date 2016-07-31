{-# LANGUAGE CPP #-}
module Index
  ( execute
  ) where

import System.IO (hSetEncoding, stdout, utf8)
import System.Environment (getEnv, getArgs)
import Data.Maybe
import Data.Time.LocalTime
import Data.Time.Format
import Data.Time.Clock

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
    (date, isToday) <- getDate
    json <- getJsonFromGroupsHistory token (channelId channel) date isToday
    let messages = historyMessages $ parseHistory json
    mapM_ echoMessage messages

getDate :: IO (String, Bool)
getDate = do
    args <- getArgs
    currentTime <- getZonedTime
    return $ case length args of
      1 -> (args !! 0, False)
      otherwise -> (zonedToDate currentTime, True)

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
    echoText = putStr . toMarkdown . fromMaybe "" . messageText
    justAttachmentText = fromMaybe "" . attachmentText
    justAttachments = fromMaybe [] . messageAttachments
    echoAttachments x = mapM_ echoAttachment $ justAttachments x
    echoAttachment x = do
      putStr "> "
      putStrLn $ attachmentFallback x
      putStr "> "
      putStrLn $ toMarkdown . attachmentFallback $ x
      putStrLn . pre $ justAttachmentText x
      putStrLn ""
    pre x
      | x == "" = x
      | otherwise = "```\n" ++ x ++ "\n```"
