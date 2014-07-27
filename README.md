Tightrope is a library that makes *writing Slash commands easier*.

You give it a function that turns a `Command` into a `Text` response, and it gives you a WAI application.

Here's a simple echo bot:

```haskell
import Network.Tightrope
import Network.Wai.Handler.Warp (run)

-- Setup is annoying right now. I'm gonna make it simpler.
main :: IO ()
main = do
  token <- readFile "token"
  let url = "https://company.slack.com/services/hooks/incoming-webhook"
      port = 4000
      echobot = bot (Account token url) echo

  putStrLn $ "Running on port " ++ show port
  Warp.run port echobot

echo :: Tightrope.Command -> Slack Text
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

    -- Only the user who typed "/echo" will see the return value, and
    -- it isn't persistent. It'll disappear the next time they refresh
    -- slack.

    return "echoing, be patient..."
```

`Slack` is a `MonadIO`, so you can `liftIO` to do more interesting things (like outside network requests) in the process of handling a command.

# But what if I don't know Haskell

That's okay! I don't either. But I've been having a lot of fun making bots *anyway*, and I've picked up a little bit of Haskell along the way.

# Examples

- [jpg2bot](https://github.com/ianthehenry/jpg2bot)

# Contributing

Tell me the way that Tightrope makes you feel, if you've used it, etc.
