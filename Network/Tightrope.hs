{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.Tightrope where
import qualified Network.Wai as Wai
import qualified Network.Wreq as Wreq
import           Control.Lens hiding ((.=))
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

newtype User = User Text
newtype Channel = Channel Text
newtype Icon = Icon Text
newtype Token = Token String

type Destination = Either Channel User

data Command = Command { _commandChannel :: Channel
                       , _commandUser :: User
                       , _commandName :: Text
                       , _commandText :: Text
                       }
$(makeFields ''Command)

data Message = Message { _messageIconEmoji :: Icon
                       , _messageDestination :: Destination
                       , _messageUsername :: Text
                       , _messageText :: Text
                       }
$(makeFields ''Message)

data Account = Account String String

instance Aeson.ToJSON User where
  toJSON (User u) = Aeson.String (Text.append "@" u)

instance Aeson.ToJSON Channel where
  toJSON (Channel c) = Aeson.String (Text.append "#" c)

instance Aeson.ToJSON Icon where
  toJSON (Icon i) = Aeson.String (mconcat [":", i, ":"])

instance Aeson.ToJSON Message where
  toJSON m = Aeson.object [ "channel" .= (m ^. destination)
                          , "icon_emoji" .= (m ^. iconEmoji)
                          , "username" .= (m ^. username)
                          , "text" .= (m ^. text)
                          ]

newtype Slack m = Slack { runSlack :: ReaderT Account IO m }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Account)

say :: Message -> Slack (Wreq.Response ByteString)
say message = do
  Account token path <- ask
  let opts = Wreq.defaults & Wreq.param "token" .~ [Text.pack token]
  liftIO $ Wreq.postWith opts path (Aeson.toJSON message)

parseCommand :: Wai.Request -> IO Command
parseCommand req = do
  (paramList, _) <- WaiParse.parseRequestBody WaiParse.lbsBackEnd req
  let params = Map.fromList paramList
      decode = StrictEncoding.decodeUtf8 . (params !)

  return Command { _commandChannel = Channel (decode "channel_name")
                 , _commandUser = User (decode "user_name")
                 , _commandName = decode "command"
                 , _commandText = decode "text"
                 }

bot :: String -> (Command -> Slack Text) -> Wai.Application
bot token handler req res = do
  command <- parseCommand req
  let (Slack m) = handler command
  responseText <- liftIO $ runReaderT m acc
  let responseBytes = LazyEncoding.encodeUtf8 (fromStrict responseText)
  res $ Wai.responseLBS status200 [contentType] responseBytes
  where contentType = ("Content-Type", "text/plain; charset=utf-8")
        acc = Account token "bar"
