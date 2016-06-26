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
If you're not familiar, I'd highly recommend checking out the links -- [this article in particular](https://haskell-servant.github.io/posts/2015-08-05-content-types.html) is a bit of a mindblower.

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
    pure (WPRProxyDest (ProxyDest "127.0.0.1" 4567))
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
Main Api> startApp
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
app manager = serve api 
    (server :<|> waiProxyTo forwardRequest defaultOnExc manager)
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

# Performance

So what is the overhead on this reverse proxy?
Let's run a shady benchmark with `httperf`.
Keep in mind that this is done locally, and the Ruby server is `WEBrick`, which is notoriously slow.

Here's the output from `httperf` on the Ruby server:

```
$ httperf --port=4567 --num-calls=500

httperf --client=0/1 --server=localhost --port=4567 --uri=/ --send-buffer=4096 --recv-buffer=163
84 --ssl-protocol=auto --num-conns=1 --num-calls=500
Maximum connect burst length: 0

Total: connections 1 requests 500 replies 500 test-duration 19.961 s

Connection rate: 0.1 conn/s (19960.9 ms/conn, <=1 concurrent connections)
Connection time [ms]: min 19960.9 avg 19960.9 max 19960.9 median 19960.5 stddev 0.0
Connection time [ms]: connect 0.1
Connection length [replies/conn]: 500.000

Request rate: 25.0 req/s (39.9 ms/req)
Request size [B]: 62.0

Reply rate [replies/s]: min 25.0 avg 25.0 max 25.0 stddev 0.0 (3 samples)
Reply time [ms]: response 1.4 transfer 38.5
Reply size [B]: header 282.0 content 66.0 footer 0.0 (total 348.0)
Reply status: 1xx=0 2xx=500 3xx=0 4xx=0 5xx=0

CPU time [s]: user 6.18 system 13.78 (user 30.9% system 69.0% total 100.0%)
Net I/O: 10.0 KB/s (0.1*10^6 bps)

Errors: total 0 client-timo 0 socket-timo 0 connrefused 0 connreset 0
Errors: fd-unavail 0 addrunavail 0 ftab-full 0 other 0
```

We're getting about 25 requests per second, with a test duration of about 20 seconds.
Here's output from just running the reverse proxy.
I built the binary using `-O2` to enable optimizations.

```
$ httperf --port=8080 --num-calls=500
httperf --client=0/1 --server=localhost --port=8080 --uri=/ --send-buffer=4096 --recv-buffer=163
84 --ssl-protocol=auto --num-conns=1 --num-calls=500
Maximum connect burst length: 0

Total: connections 1 requests 500 replies 500 test-duration 19.984 s

Connection rate: 0.1 conn/s (19984.3 ms/conn, <=1 concurrent connections)
Connection time [ms]: min 19984.4 avg 19984.4 max 19984.4 median 19984.5 stddev 0.0
Connection time [ms]: connect 0.1
Connection length [replies/conn]: 500.000

Request rate: 25.0 req/s (40.0 ms/req)
Request size [B]: 62.0

Reply rate [replies/s]: min 25.0 avg 25.0 max 25.0 stddev 0.0 (3 samples)
Reply time [ms]: response 39.9 transfer 0.0
Reply size [B]: header 290.0 content 66.0 footer 2.0 (total 358.0)
Reply status: 1xx=0 2xx=500 3xx=0 4xx=0 5xx=0

CPU time [s]: user 5.76 system 14.20 (user 28.8% system 71.1% total 99.9%)
Net I/O: 10.2 KB/s (0.1*10^6 bps)

Errors: total 0 client-timo 0 socket-timo 0 connrefused 0 connreset 0
Errors: fd-unavail 0 addrunavail 0 ftab-full 0 other 0
```

This is essentially the exact same. About 20 seconds test duration and 25 requests per second.
There doesn't seem to be any overhead detectable in this test, which makes me want a better test!

For reference, here's the Haskell `index` branch, where it's doing the `lucid` HTML templating:

```
λ ~/Projects/incremental-servant/ index* httperf --port=8080 --num-calls=500
httperf --client=0/1 --server=localhost --port=8080 --uri=/ --send-buffer=4096 --recv-buffer=163
84 --ssl-protocol=auto --num-conns=1 --num-calls=500
Maximum connect burst length: 0

Total: connections 1 requests 500 replies 500 test-duration 0.061 s

Connection rate: 16.3 conn/s (61.5 ms/conn, <=1 concurrent connections)
Connection time [ms]: min 61.5 avg 61.5 max 61.5 median 61.5 stddev 0.0
Connection time [ms]: connect 0.1
Connection length [replies/conn]: 500.000

Request rate: 8136.6 req/s (0.1 ms/req)
Request size [B]: 62.0

Reply rate [replies/s]: min 0.0 avg 0.0 max 0.0 stddev 0.0 (0 samples)
Reply time [ms]: response 0.1 transfer 0.0
Reply size [B]: header 143.0 content 77.0 footer 2.0 (total 222.0)
Reply status: 1xx=0 2xx=500 3xx=0 4xx=0 5xx=0

CPU time [s]: user 0.02 system 0.04 (user 32.5% system 65.1% total 97.6%)
Net I/O: 2240.7 KB/s (18.4*10^6 bps)

Errors: total 0 client-timo 0 socket-timo 0 connrefused 0 connreset 0
Errors: fd-unavail 0 addrunavail 0 ftab-full 0 other 0
```

8000 requests per second, with a test duration of 60 milliseconds.

I like that pretty well!
