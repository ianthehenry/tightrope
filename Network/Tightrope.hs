{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.Tightrope (
  Slack,
  Command, Message,
  User(..), Channel(..), Icon(..), Room(..),
  say, bot, message, defaultMessage,
  Account(..),
  source, user, name,
  text,
  iconEmoji, username,
  liftIO
) where
import qualified Network.Wai as Wai
import qualified Network.Wreq as Wreq
import           Control.Lens (makeFields, (^.), (&), (.~))
import qualified Data.Aeson as Aeson
import           Data.Aeson ((.=))
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Text.Lazy (fromStrict)
import qualified Data.Text.Lazy.Encoding as LazyEncoding
import qualified Data.Text.Encoding as StrictEncoding
import           Data.ByteString.Lazy (ByteString)
import           Network.HTTP.Types.Status (status200)
import           Network.Wai.Parse as WaiParse
import           Data.Map ((!))
import qualified Data.Map as Map
import           Control.Monad.Reader (MonadReader, MonadIO, ReaderT, liftIO, ask, runReaderT)
import           Control.Applicative (Applicative)
import           Data.Monoid (mconcat)

newtype User = User Text deriving (Show, Eq, Ord)
newtype Channel = Channel Text deriving (Show, Eq, Ord)
newtype Icon = Icon Text deriving (Show, Eq, Ord)

data Room = Public Channel | Private User deriving (Show, Eq, Ord)

data Command = Command { _commandSource :: Room
                       , _commandUser :: User
                       , _commandName :: Text
                       , _commandText :: Text
                       }
$(makeFields ''Command)

data Message = Message { _messageIconEmoji :: Icon
                       , _messageUsername :: Text
                       , _messageText :: Text
                       }
$(makeFields ''Message)

defaultMessage :: Message
defaultMessage = message
                 (Icon "ghost")
                 "Tightrope Bot"
                 "I love Tightrope"
                 
message :: Icon -> Text -> Text -> Message
message i u t =
  Message { _messageIconEmoji = i
          , _messageUsername = u
          , _messageText = t
          }

type Token = String
type Url = String
data Account = Account Token Url

instance Aeson.ToJSON Room where
  toJSON (Private u) = Aeson.toJSON u
  toJSON (Public c) = Aeson.toJSON c

instance Aeson.ToJSON User where
  toJSON (User u) = Aeson.String (Text.append "@" u)

instance Aeson.ToJSON Channel where
  toJSON (Channel c) = Aeson.String (Text.append "#" c)

instance Aeson.ToJSON Icon where
  toJSON (Icon i) = Aeson.String (mconcat [":", i, ":"])

newtype Slack m = Slack (ReaderT Account IO m)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Account)

say :: Message -> Room -> Slack (Wreq.Response ByteString)
say msg room = do
  Account token path <- ask
  let opts = Wreq.defaults & Wreq.param "token" .~ [Text.pack token]
  liftIO $ Wreq.postWith opts path (Aeson.toJSON payload)
  where payload = Aeson.object [ "channel" .= room
                               , "icon_emoji" .= (msg ^. iconEmoji)
                               , "username" .= (msg ^. username)
                               , "text" .= (msg ^. text)
                               ]

roomFromText :: User -> Text -> Room
roomFromText u "directmessage" = Private u
roomFromText _ channelName = Public (Channel channelName)

parseCommand :: Wai.Request -> IO Command
parseCommand req = do
  (paramList, _) <- WaiParse.parseRequestBody WaiParse.lbsBackEnd req
  let params = Map.fromList paramList
      decode = StrictEncoding.decodeUtf8 . (params !)
      requestor = User (decode "user_name")

  return Command { _commandSource = roomFromText requestor (decode "channel_name")
                 , _commandUser = requestor
                 , _commandName = decode "command"
                 , _commandText = decode "text"
                 }

bot :: Account -> (Command -> Slack Text) -> Wai.Application
bot acc handler req res = do
  command <- parseCommand req
  let (Slack m) = handler command
  responseText <- liftIO $ runReaderT m acc
  let responseBytes = LazyEncoding.encodeUtf8 (fromStrict responseText)
  res $ Wai.responseLBS status200 [contentType] responseBytes
  where contentType = ("Content-Type", "text/plain; charset=utf-8")
