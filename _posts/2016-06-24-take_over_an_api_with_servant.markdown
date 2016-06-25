---
title: "Incremental API Takeover with Haskell Servant"
date: 2016-06-24
layout: post
categories: programming
---

Haskell's [`servant`](https://haskell-servant.github.io/) library is a compelling choice for a web API.
By providing a specification of the API as a type, you ensure at compile time that your application correctly implements the specification.
You also get automatically generated or derived clients for [Haskell](https://haskell-servant.readthedocs.io/en/stable/tutorial/Client.html), [JavaScript](https://haskell-servant.readthedocs.io/en/stable/tutorial/Javascript.html), and [Ruby](https://github.com/tfausak/lackey#readme).
Using [servant-swagger](https://haskell-servant.github.io/posts/2016-02-06-servant-swagger.html), you can automatically generate `swagger` API specification, with all the goodies that come from that.

"Fine, fine, you've convinced me, I'll start my next project with Servant. But I still have all these old APIs in Ruby and JavaScript and Java that I need to support!"

What if I told you that you could incrementally take over an existing API, and gradually reap the benefits of a `servant` application?
Oh yes.
Let's do this!
We'll put the old API behind a reverse proxy that's handled in Haskell by Servant, and take over an endpoint at a time.

The code for this blog post is located in [this repository](http://www.github.com/parsonsmatt/incremental-servant).
Each section has it's own git branch.
The first section is the master branch.

# The Initial API

Here's the super important business logic legacy API that we need to preserve while we're replacing it:

```ruby
# rubby/api.rb
require 'sinatra'
require 'json'

get '/' do
  'You can get either <a href="cat">cat</a> or <a href="dog">dog</a>.'
end

get '/cat' do
  { cat: "meow" }.to_json
end

get '/dog' do
  { dog: "woof" }.to_json
end
```

Pretty hairy, right? _groan_ 
Sinatra is a nice Ruby DSL for writing APIs and web apps.
When you make a `GET` request to `/`, it responds with a bit of text linking you to either `cat` or `dog`.
Ruby implicitly returns the last line in a block, so this just returns the corresponding hashes converted to JSON.

Ordinarily, we'd run this with `ruby api.rb` and it'd be on `http://localhost:4567`.
Our first step is creating the reverse proxy in Haskell that is handled by Servant.
We'll use the [`http-reverse-proxy`](https://www.stackage.org/lts-6.4/package/http-reverse-proxy-0.4.3) package to simplify the process.

Here's the full source code of the initial reverse proxy.

```haskell
-- src/Api.hs

{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators   #-}

module Api where

import Network.HTTP.ReverseProxy
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
```

As usual, we start with the language extensions and imports.

```haskell
forwardRequest :: Request -> IO WaiProxyResponse
forwardRequest _ = 
    pure . WPRProxyDest . ProxyDest "127.0.0.1" $ 4567
```

`forwardRequest` is a function we'll use to return a [`WaiProxyResponse`](https://www.stackage.org/haddock/lts-6.4/http-reverse-proxy-0.4.3/Network-HTTP-ReverseProxy.html#t:WaiProxyResponse).
This function inspects the request, and then gets to decide what to do with it.
In this case, we just want to forward the request to our Sinatra app running on `localhost:4567`.

```haskell
app :: Manager -> Application
app manager =
    waiProxyTo forwardRequest defaultOnExc manager

startApp :: IO ()
startApp = do
    manager <- newManager defaultManagerSettings
    run 8080 (app manager)
```

`app` is going to be how we define our application, and `startApp` is a convenience function we'll use to run the app in GHCi.

Let's verify that this is working like we want it to!

```bash
$ git clone https://www.github.com/parsonsmatt/incremental-servant
$ cd incremental-servant/rubby
$ bundle install
$ ruby api.rb
```

This runs our Ruby API. When you go to `http://localhost:4567`, you *should* see the text defined in the `get '/' do ... end` block above.
Clicking either of the links will return a JSON object.

Now, if this works, then we *should* be able to just run the Haskell thing in GHCi and access it through `localhost:8080`.
Let's give that a shot. Leave the `ruby api.rb` task running, and in another terminal, do:

```bash
$ cd incremental-servant
$ stack init
$ stack ghci
# GHCi loads...
Main Api> runApp
-- silence . . .
```

Now, when I navigate to `localhost:8080` in Chrome, I see the original application we defined.
Nice!

# Commandeering a Route

Next up, we'll take over the `cat` route.
The code for this section is in the [`cat-takeover` branch on GitHub](https://github.com/parsonsmatt/incremental-servant/tree/cat-takeover).
Our first step is to define our API type for Servant:

```haskell
type API 
    = "cat" :> Get '[JSON] Cat
```

For the string literal `"cat"`, we'll return a `JSON` representation of a `Cat`.
We could alternatively specify other encodings, like HTML or plain text, but for now we're just returning JSON.
What is a `Cat` exactly? We have to define it!

```haskell
newtype Cat = Cat { cat :: String }
```

And how do we convert it to JSON? We'll use the [Aeson](https://www.stackage.org/lts-6.4/package/aeson-0.11.2.0) library to do the conversion!
Here's a manual instance that mirrors the API we have on the Ruby:

```haskell
instance ToJSON Cat where
    toJSON (Cat mew) = 
        object [ "cat" .= mew ]
```

Serialization and deserialization are often some of the more annoying parts of writing a web app.
Aeson provides both Template Haskell functions for deriving compile time instances, as well as generic implementations for derived instances.
If we enable the `DeriveGeneric` and `DeriveAnyClass` extensions and `import GHC.Generics`, then we can change our `newtype` declaration above to the following and get the JSON instance without having to write it:

```haskell
newtype Cat = Cat { cat :: String }
    deriving (Generic, ToJSON)
```

Nice! Alright, let's get it hooked up to our application.
First, we need a Servant server function:

```haskell
server :: Server API
server = pure (Cat { cat = "mrowl" })
```

Now we need to turn our `Server API` into a `WAI` `Application`. 
We use the `serve` function and the funny `:<|>` constructor.

```haskell
api :: Proxy (API :<|> Raw)
api = Proxy

app :: Manager -> Application
app manager = serve api $ server :<|> 
    waiProxyTo forwardRequest defaultOnExc manager
```

The `api` proxy is required to tell Servant what our API type is supposed to look like.
Otherwise, it can't figure out whether or not the function we're `serve`ing conforms to it!

The second argument of `serve` is a pair of handlers.
The handlers must line up with the types.
Note that `server` in this code has the type `Server API`, while `waiProxyTo ...` has the type `Application`.
If we have Servant server types (like `"cat" :> Get '[JSON] Cat`), then we need to have a `Server` for them.
For the `Raw` endpoint, we just need any `WAI` application.

This is all the changes we need! Let's test this out.
You haven't closed the `ruby api.rb` process right?
That's been running the whole time, right?
Open it up in `localhost:4567` and verify that it's still doing the thing we want it to.

Close out the current `startApp` call in GHCi, hit `:reload` to reload the code, and run `startApp` again.
Open `localhost:8080` in the browser, and you *should* see the same text from Sinatra.
However, when you click `cat`, instead of the cat saying `meow`, you get `mrowl`!

EXCITING

We just took over a route without any loss of service or touching the underlying application at all!

# DOGS

Let's knock out the dogs route now.
The code for that is [here](https://github.com/parsonsmatt/incremental-servant/tree/dog-takeover).
Then the original app will only be serving as an entry point!

It's actually a really minimal change! We'll add two language pragmas so we can derive generic instances, import the generic machinery, and that's almost the whole of it!

```haskell
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

-- ... 

import GHC.Generics

-- ...

newtype Dog = Dog { dog :: String }
    deriving (Generic, ToJSON)

type API 
    = "cat" :> Get '[JSON] Cat
    :<|> "dog" :> Get '[JSON] Dog

server :: Server API
server = cats :<|> dogs
  where
    cats = pure (Cat { cat = "mrowl" })
    dogs = pure (Dog { dog = "zzzzzz" })
```

And that's all! We'll reload the code in GHCi and see that the `dogs` route has been successfully captured.

# Intro Text

The second to last remaining bit is to grab the index page.
Let's do it!

(this code is in the [`index` branch on GitHub](https://github.com/parsonsmatt/incremental-servant/tree/index))

We'll use the excellent [`lucid`](https://www.stackage.org/lts-6.4/package/lucid-2.9.5) library for HTML templating rather than a bare string.
This means we'll need to add `lucid` and `servant-lucid` to the cabal file, and add the relevant imports.

```haskell
import Lucid
import Servant.HTML.Lucid

type API
    = Get '[HTML] (Html ())
    :<|> "cat" :> Get '[JSON] Cat
    :<|> "dog" :> Get '[JSON] Dog
```

Our API definition adds a first route, returning a content type of HTML and a value of `Html ()`.

```haskell
server :: Server API
server = pure index :<|> cats :<|> dogs
  where
    cats = pure (Cat { cat = "mrowl" })
    dogs = pure (Dog { dog = "zzzzzzzz" })
    index = p_ $ do
        "You can get either a "
        a_ [href_ "cat"] "cat"
        " or a "
        a_ [href_ "dog"] "dog"
        "."
```

I really like `lucid`. It's a great DSL for HTML.

At this point, all the API endpoints are being routed to Haskell.
We still have the reverse proxy setup, though, and a `Raw` endpoint never fails to match.
This means that any missing route will go back to the Sinatra application, and will be handled there.
If we remove the proxy and `Raw` endpoint, then we'll be able to handle those errors in Servant.

Anyway, that's all we had to do.
If we quit GHCi, restart it, and then rerun `startApp`, the application will be serving up our index page rather than the Ruby app.
We have successfully ousted a Ruby API with one based on Haskell's Servant, from which we'll reap tremendous benefits in terms of generated documentation, clients, and improved performance.
