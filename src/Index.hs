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
import Nippoa.RecordFactory
import Slack.UsersList
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
    factory <- getFactory token
    json <- getJsonFromGroupsList token
    let channel = channelByNameFromJson channelName json
    (date, isToday) <- getDate
    json <- getJsonFromGroupsHistory token channel date isToday
    putStrLn . messagesTextFrom factory $ json
  where
    channelByNameFromJson name =
      channelId . fromMaybe (error "Channel Not Found") .
      fromGroupsName name . parseGroupsList
    messagesTextFrom factory =
      concat . reverse . map (record factory) . historyMessages . parseHistory
    record factory = newline . recordRender . recordByMessage factory
    newline x = x ++ "\n"

getDate :: IO (String, Bool)
getDate = do
    args <- getArgs
    currentTime <- getZonedTime
    return $ case length args of
      1 -> (args !! 0, False)
      otherwise -> (zonedToDate currentTime, True)

getFactory :: String -> IO RecordFactory
getFactory token = do
    json1 <- getJsonFromUsersList token
    return RecordFactory
      { usersList = parseUsersList json1
      }
