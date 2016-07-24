module Index
  ( execute
  ) where

import Prelude hiding (id)
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
        channelId = id channel
    print channel
    json <- simpleHttp $ printf urlChannelHistory token channelId
    putStrLn $ unpack json
    let messages = channelHistoryMessages $ parseChannelHistory json
    putStrLn ""
    print messages
  where
    urlChannelList = "https://slack.com/api/channels.list?token=%s"
    urlChannelHistory = "https://slack.com/api/channels.history?token=%s&channel=%s"
