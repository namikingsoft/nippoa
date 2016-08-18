{-# LANGUAGE CPP #-}
module Slack.Organizer
  ( Organizer(..)
  , getOrganizer
  , recordsByMessage
  , channelByName
  ) where

import Control.Concurrent
  ( MVar(..)
  , newMVar
  , putMVar
  , takeMVar
  , forkIO
  )
import Text.Printf
  ( printf
  )
import Data.Maybe
  ( fromMaybe
  )
import qualified Data.ByteString.Lazy as B
  ( length
  , empty
  )
import Nippoa.Record
  ( Record(..)
  )
import Nippoa.Record.TimeStamp
  ( TimeStamp(..)
  , timeStampFromTs
  , timeStampToText
  )
import Nippoa.Record.Author
  ( Author(..)
  )
import Slack.Agent
  ( getJsonFromUsersList
  , getJsonFromGroupsList
  , getJsonFromChannelsList
  )
import Slack.UsersList
  ( UsersList(..)
  , userById
  , parseUsersList
  )
import Slack.GroupsList
  ( GroupsList(..)
  , channelByGroupsName
  , parseGroupsList
  )
import Slack.ChannelsList
  ( ChannelsList(..)
  , channelByChannelsName
  , parseChannelsList
  )
import Slack.Message
  ( Message(..)
  , toMarkdown
  )
import Slack.Channel
  ( Channel(..)
  )
import Slack.User
  ( User(..)
  )
import Slack.Profile
  ( Profile(..)
  )
import Slack.Attachment
  ( Attachment(..)
  )
import Utility.Base
  ( liftJustList
  , filterJust
  , isMaybe
  )
import Utility.Regex
  ( match
  )

data Organizer
  = Organizer
  { organizerUsersList :: UsersList
  , organizerGroupsList :: GroupsList
  , organizerChannelsList :: ChannelsList
  }

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
            True -> return
                Organizer
              { organizerUsersList = parseUsersList json1
              , organizerGroupsList = parseGroupsList json2
              , organizerChannelsList = parseChannelsList json3
              }
            False -> waitForOrganizer mjson1 mjson2 mjson3
      where
        isDone json = B.length json > 0


recordsByMessage :: Organizer -> Message -> [Record]
recordsByMessage this x =
    recordsMain x ++ recordsAttaches x
  where
    recordsMain =
      liftJustList . recordByMessage
    recordsAttaches =
      filterJust . map recordByAttachment . fromMaybe [] . messageAttachments
    recordByMessage x =
      case messageUser x of
        Just id -> Just
            Plain
          { plainTimeStamp = timeStampFromTs . messageTs $ x
          , plainAuthor
              = Author
              { authorName = userName user
              , authorImage24 = profileImage24 . userProfile $ user
              , authorImage48 = profileImage48 . userProfile $ user
              }
          , plainText = toMarkdown . fromMaybe "" . messageText $ x
          }
          where
            user = fromMaybeUser . userById (organizerUsersList this) $ id
        otherwise -> Nothing
    recordByAttachment y =
      case attachmentTitleLink y of
        Just n -> Just
            Link
          { linkTimeStamp = timeStampFromTs . messageTs $ x
          , linkAuthor = None
          , linkText = fromMaybe "" . attachmentTitle $ y
          , linkHref = n
          }
        otherwise ->
          case attachmentFallback y of
            n | isMaybe . matchGitHubComment $ n -> Just
                GitHubComment
              { githubCommentTimeStamp = timeStampFromTs . messageTs $ x
              , githubCommentAuthor = None
              , githubCommentLink = (!!0) . fromMaybe [] . matchGitHubComment $ n
              , githubCommentTitle = (!!1) . fromMaybe [] . matchGitHubComment $ n
              , githubCommentText = fromMaybe "" . attachmentText $ y
              }
            otherwise -> Nothing
      where
        matchGitHubComment = match "New comment by .* <(.*)\\|(.*)>"
    fromMaybeUser = fromMaybe (error "User Not Found")

channelByName :: Organizer -> String -> Maybe Channel
channelByName this name = case maybeChannel1 of
    Just channel1 -> maybeChannel1
    otherwise -> case maybeChannel2 of
      Just channel2 -> maybeChannel2
      othersize -> Nothing
  where
    maybeChannel1 = channelByGroupsName (organizerGroupsList this) name
    maybeChannel2 = channelByChannelsName (organizerChannelsList this) name
