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
import Slack.UsersList
import Slack.GroupsList
import Slack.ChannelsList
import Slack.History
import Slack.Channel
import Slack.Message
import Slack.Network
import Slack.Organizer
import Utility.Time

execute :: IO ()
execute = do
    hSetEncoding stdout utf8
    token <- getEnv "SLACK_API_TOKEN"
    channelName <- getEnv "SLACK_CHANNEL_NAME"
    organizer <- getOrganizer token
    (date1, date2) <- getDate
    let channel = returnChannel $ channelByName organizer channelName
    json <- getJsonFromGroupsHistory token channel date1 date2
    putStrLn . messagesTextFrom organizer $ json
  where
    returnChannel = fromMaybe (error "Channel Not Found")
    messagesTextFrom organizer =
      concat . reverse . map (record organizer) . historyMessages . parseHistory
    record organizer = newline . recordRender . recordByMessage organizer
    newline x = x ++ "\n"

getDate :: IO (UTCTime, UTCTime)
getDate = do
    args <- getArgs
    currentTime <- getCurrentTime
    zonedTime <- getZonedTime
    return $ case length args of
      2 ->
        ( dateToUtc $ args !! 0
        , dateToUtc $ args !! 1
        )
      1 -> case dateToUtc $ args !! 0 of
        n | addUTCTime oneDay n < currentTime ->
          ( n, addUTCTime oneDay n)
        n | otherwise ->
          ( n, currentTime )
      _ ->
        ( dateToUtc . zonedToDate $ zonedTime
        , currentTime
        )
  where
    oneDay = diffSec 86400

getOrganizer :: String -> IO Organizer
getOrganizer token = do
    json1 <- getJsonFromUsersList token
    json2 <- getJsonFromGroupsList token
    json3 <- getJsonFromChannelsList token
    return Organizer
      { usersList = parseUsersList json1
      , groupsList = parseGroupsList json2
      , channelsList = parseChannelsList json3
      }
