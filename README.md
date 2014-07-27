Tightrope is a library that makes *writing Slash commands easier*.

You give it a function that turns a `Command` into a `Text` response, and it gives you a [WAI application](http://hackage.haskell.org/package/wai) that you can hand to [Warp](https://hackage.haskell.org/package/warp) (or any other WAI-compatible server).

You need to give it an incoming-webhook token in order to use the `say` command. You can pass in the empty string if you're writing a bot that only needs to communicate with the user who initiated the slash command.

# Usage

Here's a simple echo bot:

```haskell
import Network.Tightrope
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do

  -- we don't want to keep our token in source control
  token <- readFile "token"

  -- change this, of course
  let url = "https://company.slack.com/services/hooks/incoming-webhook"
      port = 4000
      echobot = bot (Account token url) echo

  putStrLn $ "Running on port " ++ show port
  Warp.run port echobot

echo :: Command -> Slack Text
echo command = do
    -- `say` will broadcast a message to everyone in the specified room,
    -- or to one person (if you you `say` to a private room).

    -- You can `say` as many times as you want, to whatever room you like.

    -- But here we're just gonna post a message to the room the request
    -- came in on.

    say $ message (Icon "ghost")
                  "Echobot"
                  (command ^. text)
                  (command ^. source)

    return "echoing, be patient..."

    -- Only the user who typed "/echo" will see the return value, and
    -- it isn't persistent. It'll disappear the next time they refresh
    -- slack. Use `say` if you want something persistent -- when given
    -- a `Private` Room, `say` will send messages from slackbot to the
    -- specified user.
```

`Slack` is a `MonadIO`, so you can use `liftIO` to do any IO operation in the process of handling the request.

What? A monad??

Yes. Here there be monads. Don't be afraid. Monads can smell fear. We can get through this together. I'm there for you.

# But what if I don't know Haskell

That's okay! I don't either. But I've been having a lot of fun making bots *anyway*, and I've picked up a little bit of Haskell along the way.

# Examples

- [jpg2bot](https://github.com/ianthehenry/jpg2bot)

# Contributors

- [Ian Henry](http://ianthehenry.com), author
- [Hao Lian](http://hao.codes), inspiration and [nemesis](https://github.com/hlian/linklater)
- You??
