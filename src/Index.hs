module Index
  ( execute
  ) where

import System.Environment
import Control.Applicative
import Network.HTTP.Conduit
import Text.Printf
import Data.Maybe
import Data.Aeson
import Data.ByteString.Lazy.Char8 (unpack)

import Slack.ChannelHistory
import Slack.ChannelList
import Slack.Channel

execute :: IO ()
execute = do
    token <- getEnv "SLACK_API_TOKEN"
    json <- simpleHttp . printf urlChannelList $ token
    putStrLn ""
    putStrLn $ unpack json
    let channelList = parse json
        maybeChannel = fromName channelList "general"
        channel = fromMaybe (error "Channel Not Found") maybeChannel
    print channel
    json <- simpleHttp $ printf urlChannelHistory token (channelId channel)
    putStrLn $ unpack json
    let messages = channelHistoryMessages $ parseChannelHistory json
    putStrLn ""
    print messages
  where
    urlChannelList = "https://slack.com/api/channels.list?token=%s"
    urlChannelHistory = "https://slack.com/api/channels.history?token=%s&channel=%s"
