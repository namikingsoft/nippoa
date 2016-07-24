module Index
  ( execute
  ) where

import System.Environment
import Control.Applicative
import Text.Printf
import Data.Aeson
import Network.HTTP.Conduit
import Data.ByteString.Lazy.Char8 (unpack)

import Slack.ChannelList
import Slack.Channel

execute :: IO ()
execute = do
    token <- getEnv "SLACK_API_TOKEN"
    json <- simpleHttp . printf urlChannelList $ token
    putStrLn ""
    putStrLn $ unpack json
    let channelList = parse json
        channel = fromName channelList "general"
    putStrLn ""
    print channel
  where
    urlChannelList = "https://slack.com/api/channels.list?token=%s"
    urlChannelHistory = "https://slack.com/api/channels.history?token=%s&channel=%s"
