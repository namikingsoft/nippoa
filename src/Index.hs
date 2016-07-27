module Index
  ( execute
  ) where

import System.IO
import System.Environment
import Control.Applicative
import Network.HTTP.Conduit
import Text.Printf
import Data.Maybe
import Data.Aeson
import Data.ByteString.Lazy.Char8 (unpack)

import Slack.GroupsList
import Slack.ChannelHistory
import Slack.Channel
import Slack.Message

execute :: IO ()
execute = do
    hSetEncoding stdout utf8
    token <- getEnv "SLACK_API_TOKEN"
    channelName <- getEnv "SLACK_CHANNEL_NAME"
    json <- simpleHttp . printf urlGroupsList $ token
    putStrLn ""
    putStrLn $ unpack json
    let groupsList = parseGroupsList json
        maybeChannel = fromGroupsName groupsList channelName
        channel = fromMaybe (error "Channel Not Found") maybeChannel
    print channel
    json <- simpleHttp $ printf urlGroupsHistory token (channelId channel)
    putStrLn $ unpack json
    let messages = channelHistoryMessages $ parseChannelHistory json
    mapM_ (\x -> putStrLn $ fromMaybe "" $ messageText x) messages
  where
    urlGroupsList = "https://slack.com/api/groups.list?token=%s"
    urlGroupsHistory = "https://slack.com/api/groups.history?token=%s&channel=%s"
