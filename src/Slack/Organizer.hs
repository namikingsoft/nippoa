{-# LANGUAGE CPP #-}
module Slack.Organizer
  ( Organizer(..)
  , recordsByMessage
  , channelByName
  ) where

import Text.Printf
  ( printf
  )
import Data.Maybe
  ( fromMaybe
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
import Slack.UsersList
  ( UsersList(..)
  , userById
  )
import Slack.GroupsList
  ( GroupsList(..)
  , channelByGroupsName
  )
import Slack.ChannelsList
  ( ChannelsList(..)
  , channelByChannelsName
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
  { usersList :: UsersList
  , groupsList :: GroupsList
  , channelsList :: ChannelsList
  }

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
            user = returnUser . userById (usersList this) $ id
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
    returnUserById = returnUser . userById (usersList this)
    returnUser = fromMaybe (error "User Not Found")

channelByName :: Organizer -> String -> Maybe Channel
channelByName this name = case maybeChannel1 of
    Just channel1 -> maybeChannel1
    otherwise -> case maybeChannel2 of
      Just channel2 -> maybeChannel2
      othersize -> Nothing
  where
    maybeChannel1 = channelByGroupsName (groupsList this) name
    maybeChannel2 = channelByChannelsName (channelsList this) name
