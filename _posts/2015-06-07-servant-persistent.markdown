---
title: "servant-persistent"
date: 2015-06-07
layout: post
categories: programming
---

## A Brief Example

When people talk about why they like Haskell, the type system always comes out as a big win. Everything from inference, to refactorability, to compile-time checking. The more you can program into the type system, the more benefit you gain from it -- and there have been some compelling demonstrations lately.

[Servant](https://haskell-servant.github.io/) is a fantastic example of this. Servant provides a type-level DSL for defining a webservice API. You describe the endpoints, named captures, query parameters, headers, response types, etc. The compiler is then able to verify the correctness of your code in fulfilling the API you described. Define a new route? The compiler immediately lets you know what's missing. Change the parameters that an endpoint accepts? The compiler lets you know which endpoint function needs to change, and how.

Servant is still rather new, and there wasn't yet an example on connecting a Servant API with a database. [Persistent](http://www.yesodweb.com/book/persistent) leverages the type system similarly, and the combination of the two makes for a compelling example on the power of Haskell.

The code for this post is available in the following github repository: [parsonsmatt/servant-persistent](http://www.github.com/parsonsmatt/servant-persistent)

# Main.hs

Main is brief -- it gathers some configuration information from the environment, initializes the database resources, and runs the server.

```haskell
module Main where

import Network.Wai.Handler.Warp    (run)
import System.Environment          (lookupEnv)
import Database.Persist.Postgresql (runSqlPool)

import Config (defaultConfig, Config(..), Environment(..), setLogger, makePool)
import Api    (app)
import Models (doMigrations)

main :: IO ()
main = do
    env  <- lookupSetting "ENV" Development
    port <- lookupSetting "PORT" 8081
    pool <- makePool env
    let cfg = defaultConfig { getPool = pool, getEnv = env }
        logger = setLogger env
    runSqlPool doMigrations pool
    run port $ logger $ app cfg

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    p <- lookupEnv env
    return $ case p of Nothing -> def
                       Just a  -> read a
```

And that's it for the Main module!

# Api.hs

Now, let's dig into the neat stuff -- the API definition! This is located in `src/Api.hs`. As usual, language pragmas and imports bring up the beginning:

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Control.Monad.Reader         (ReaderT, runReaderT, lift)
import Control.Monad.Trans.Either   (EitherT, left)
import Network.Wai                  (Application)
import Database.Persist.Postgresql  (selectList, Entity(..), (==.)
                                    , fromSqlKey)
import Data.Int                     (Int64)
import Servant

import Config    (Config(..))
import Models -- (Person, runDb, userToPerson, EntityField(UserName))
```

The Servant import isn't qualified, as it brings about 20 terms into scope, included type operators, and GHC was complaining about trying to explicitly import them. `Config` and `Models` are both internal modules to the application. The qualified imports for the Model are commented out, as the module doesn't export the EntityField(UserName) definition and wouldn't compile. I left the comment there to show what is being imported.

The API we'll be describing is pretty simple: we want to be able to get a list of users, create a user, and retreive a single user by name. We'll return them as JSON. We'll go over each line individually to describe what's going on here.

```haskell
type PersonAPI = 
         "users" :> Get   '[JSON]   [Person]
      {- scope   /  verb  encoding  return value -}
```

`"users"` indicates that this route will start with `/users`. `:>` acts to combine parts of the route. The final part of the route specifies the HTTP verb, the return content encodings, and what the request will return. So "return all users as JSON" -- not too bad!

```haskell
    :<|> "users" :> Capture "name" String :> Get '[JSON] Person
```

The `:<|>` is an `Alternative` type operator. It can be read as "or": The type of this route is either 'get all users at `/users` **or** get a single user at `/users/:name`'. `Capture "name" String` is how we specify that we want this to be a named capture, and we'll `read` it as a String. Servant has a lot of types built in, and you're able to define your own.

```haskell
    :<|> "users" :> ReqBody '[JSON] Person :> Post '[JSON] Int64
```

Lastly, this one specifies that we'll accept a `POST` request at `/users`. The `ReqBody` will accept JSON encoding of a Person, and will return an `Int64`.

## Implementing the Server

Now that we've defined the API, we need to serve it. By default, Servant uses an `EitherT ServantErr IO`. We'd like to extend this with the `Reader` monad to make the database configuration available without manually threading it through all the functions that require it. A detailed description on this is available [in this guide](https://haskell-servant.github.io/tutorial/server.html#using-another-monad-for-your-handlers).

```haskell
type AppM = ReaderT Config (EitherT ServantErr IO)

userAPI :: Proxy PersonAPI
userAPI = Proxy

readerToEither :: Config -> AppM :~> EitherT ServantErr IO
readerToEither cfg = Nat $ \x -> runReaderT x cfg

readerServer :: Config -> Server PersonAPI
readerServer cfg = enter (readerToEither cfg) server

app :: Config -> Application
app cfg = serve userAPI (readerServer cfg)
```

Now we've got something that returns a WAI `Application`! We're just about ready to run this.

```haskell
server :: ServerT PersonAPI AppM
server = allPersons :<|> singlePerson :<|> createPerson
```

We defined three routes, so we need three handler functions. And the handler functions need to be of the right type!

```haskell
allPersons :: AppM [Person]
allPersons = do
    users <- runDb $ selectList [] []
    let people = map (\(Entity _ y) -> userToPerson y) users
    return people
```

Persistent's `selectList` here is pretty astounding at first -- it's capable of inferring the database table to query on based on the type. The first list is a list of filters, so we're getting all of the Users out of the database. Persistent knows this because we're mapping the function `userToPerson :: User -> Person` over the array. In any case, we've got a fairly standard Persistent query, and a `return` to raise it into the `AppM` monad.

```haskell
singlePerson :: String -> AppM Person
singlePerson str = do
    users <- runDb $ selectList [UserName ==. str] []
    let list = map (\(Entity _ y) -> userToPerson y) users
    case list of
         []     -> lift $ left err404
         (x:xs) -> return x
```

At the top, we defined the route to take a single named capture. This function uses that named capture as a parameter. We'll select all users from the database with the same name as the capture. If the resulting list is empty, we'll error out with a 404. Otherwise, we'll return the first one.

This isn't particularly elegant, but it shows how to error out of a servant request. Since our overall monad is `ReaderT Config (EitherT ServantErr IO)`, we have to lift the call to left.

```haskell
createPerson :: Person -> AppM Int64
createPerson p = do
    newPerson <- runDb $ insert $ User (name p) (email p)
    return $ fromSqlKey newPerson
```

In all of these examples, I've used an indirect type. `User` is the database model, and `Person` is what we're interfacing with. If we were directly dealing with `User`s, then this function could be expressed as the point-free one liner:

```haskell
createUser = liftM fromSqlKey . runDb . insert
```

And that's it. The API is defined and ready to be served!

# Config.hs

Config contains many of the functions used to configure the application, as well as the Config datatype that the ReaderT monad uses. I'll skip the imports in the interest of brevity:

```haskell
data Config = Config 
    { getPool :: ConnectionPool
    , getEnv  :: Environment
    }

data Environment = 
    Development
  | Test
  | Production
  deriving (Eq, Show, Read)

defaultConfig :: Config
defaultConfig = Config
    { getPool = undefined
    , getEnv  = Development
    }
```

Config is a simple record that we stuff into the Reader monad so we can read information from it with `asks`. We'll use this with the `runDb` function, which is defined in Models.

# Models.hs

Models holds the data definitions for our data types, and a few functions for running queries against the database. Persistent brings a ton of language extensions into play and uses Template Haskell extensively. For a good introduction to Persistent, see [the chapter](http://www.yesodweb.com/book/persistent) from the Yesod book. Our database model `User` is here:

```haskell
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name String
    email String
    deriving Show
|]
```

and our API data type `Person` is here:

```haskell
data Person = Person
    { name :: String
    , email :: String
    } deriving (Eq, Show, Generic)

instance ToJSON Person
instance FromJSON Person

userToPerson :: User -> Person
userToPerson User{..} = Person { name = userName, email = userEmail }
```

Nothing too fancy! Now we just need to query the database, and we'll be all set. The monad that our application uses is the `ReaderT Config (EitherT ServantErr IO)` monad stack. `runSqlPool` operates in IO, so we'll need to `liftIO` to get it where we want it. Our connection pool is stored in the `Config`, so we'll need to `asks getPool` to access it. `runDb` therefore looks like:

```haskell
runDb query = do
   pool <- asks getPool
   liftIO $ runSqlPool query pool
```

This can also be written as `runDb query = asks getPool >>= liftIO . runSqlPool query` if you're into one liners.

# Try it out!

All the files are available on the [Github repository](https://github.com/parsonsmatt/servant-persistent). Clone the repository, get it running, and play with it. If you'd like some practice, try the following exercises:

## Add another data model and route!

1. Perhaps this app is a blogging service. Add a `Post` model to the database definition, with a title, body, and a reference to a `User` that authored it. Use Persistent's convenient `json` annotation to automatically derive FromJSON and ToJSON instances.
2. Add a route to the API to get a user's posts. It should look something like: `users/:id/posts`. After adding the route, read the type error, and see what function you need to add to the server.
3. Create the function that will retrieve a users posts.
4. Of course, accessing isn't enough. Create a route/function that will allow someone to create a blog post at `users/:id/posts`. You'll need to access the `ReqBody` and a `Capture` for this.

## Create a join model!

1. People now want to follow other people on the blog. Create a join model `FollowRelation` that stores a `Follower` user reference and a `Follows` user reference.
2. Add a route to get a user's followers: `users/:id/followers`
3. Now add a route to follow a user: POST a UserID to `users/:id/follow` that creates a `FollowRelation`
