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

import Nippoa.Record
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
    let channel = channelByNameFromJson channelName json
    (date, isToday) <- getDate
    json <- getJsonFromGroupsHistory token channel date isToday
    putStrLn . messagesTextFrom $ json
  where
    channelByNameFromJson name =
      channelId . fromMaybe (error "Channel Not Found") .
      fromGroupsName name . parseGroupsList
    messagesTextFrom =
      concat . reverse . map record . historyMessages . parseHistory
    record = newline . recordRender . recordByMessage
    newline x = x ++ "\n"

getDate :: IO (String, Bool)
getDate = do
    args <- getArgs
    currentTime <- getZonedTime
    return $ case length args of
      1 -> (args !! 0, False)
      otherwise -> (zonedToDate currentTime, True)
