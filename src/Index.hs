{-# LANGUAGE CPP #-}
module Index
  ( execute
  ) where

import System.IO (hSetEncoding, stdout, utf8)
import System.Environment (getEnv, getArgs)
import Control.Concurrent
import Data.Maybe
import Data.Time.LocalTime
import Data.Time.Format
import Data.Time.Clock
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Internal
import Nippoa.Record
import Slack.UsersList
import Slack.GroupsList
import Slack.ChannelsList
import Slack.History
import Slack.Channel
import Slack.Message
import Slack.Agent
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
      concat . reverse . map (newline . recordRender) .
      concatMap (recordsByMessage organizer) . historyMessages . parseHistory
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
    mjson1 <- newMVar B.empty
    mjson2 <- newMVar B.empty
    mjson3 <- newMVar B.empty
    forkIO $ forkJson getJsonFromUsersList mjson1
    forkIO $ forkJson getJsonFromGroupsList mjson2
    forkIO $ forkJson getJsonFromChannelsList mjson3
    waitForOrganizer mjson1 mjson2 mjson3
  where
    forkJson getJson mjson = do
        json <- getJson token
        putMVar mjson json
    waitForOrganizer mjson1 mjson2 mjson3 = do
        json1 <- takeMVar mjson1
        json2 <- takeMVar mjson2
        json3 <- takeMVar mjson3
        case isDone json1 && isDone json2 && isDone json3 of
            True ->
                return Organizer
                  { usersList = parseUsersList json1
                  , groupsList = parseGroupsList json2
                  , channelsList = parseChannelsList json3
                  }
            False -> waitForOrganizer mjson1 mjson2 mjson3
      where
        isDone json = B.length json > 0

