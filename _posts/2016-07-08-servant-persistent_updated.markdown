---
title: "servant-persistent updated"
date: 2016-07-08
layout: post
categories: programming
---

Previously, I [wrote a blog post on using `servant` and `persistent` together]({% post_url 2015-06-07-servant-persistent %}).
`servant` has updated to the new 0.7 version, and I felt like it was a good idea to bring my tutorial up to date.
I'd also noticed that some folks were using the repository as a starter scaffold for their own apps, which is great!
To accommodate that, I've beefed up the application a bit to demonstrate some of the features of Servant, including a primitive client, as well as configuration for easy deployment with the [`keter`](https://hackage.haskell.org/package/keter) package.
Let's dive in!

The code for all of this is on the [GitHub repository](https://github.com/parsonsmatt/servant-persistent/tree/0.7).
I'll be keeping the 0.7 branch up to date with any edits to this post.

Take note: This is less of a tutorial on `servant` specifically, and more of an exposition on a `servant` base package that has some convenient defaults for running applications.

# Application Structure

The application has three sub-components:

- `src` : contains all the library code
- `app` : contains the code for the executable
- `test` : contains the test code

It's a good idea to extract as much code as you can in the library.
This makes it easier to test the code, as you can import it into the tests without having to recompile it every time.
Additionally, you can make the library functions available for all kinds of potential executables down the line.
We'll start with `Main` and dig into the rest.

# `app/Main.hs`

```haskell
-- | The 'main' function gathers the required environment information and
-- initializes the application.
main :: IO ()
main = do
    putStrLn "servant-persistent booting up"
    env  <- lookupSetting "ENV" Development
    port <- lookupSetting "PORT" 8081
    pool <- makePool env
    let cfg = Config { getPool = pool, getEnv = env }
        logger = setLogger env
    runSqlPool doMigrations pool
    generateJavaScript
    run port $ logger $ app cfg
```

`main` grabs some settings from the environment, creates the database pool, runs migrations, generates JavaScript for querying the API, and finally runs the app.
We define `lookupSetting` a little below:

```haskell
-- | Looks up a setting in the environment, with a provided default, and
-- 'read's that information into the inferred type.
lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    maybeValue <- lookupEnv env
    case maybeValue of
        Nothing ->
            return def
        Just str ->
            maybe (handleFailedRead str) return (readMay str))
  where
    handleFailedRead str =
        error $ mconcat 
            [ "Failed to read [["
            , str
            , "]] for environment variable "
            , env
            ]
```

First, we lookup the environment variable. If it's not present, then we just return the default value.
If it is present, then we use the function `readMay` which we've imported from the `Safe` module.
If `readMay` fails to read the variable, then we throw an error.
Consider that `readMay "PRoduction" :: Maybe Environment` will return `Nothing`, silently putting us in `Development` mode.
We definitely don't want that!

Next up is `makePool`, so let's check that out. We've imported it from `Config`.

# `src/Config.hs`

For `Development` and `Test` environments, the `makePool` function is relatively simple:

```haskell
-- | This function creates a 'ConnectionPool' for the given environment.
-- For 'Development' and 'Test' environments, we use a stock and highly
-- insecure connection string. The 'Production' environment acquires the
-- information from environment variables that are set by the keter
-- deployment application.
makePool :: Environment -> IO ConnectionPool
makePool Test =
    runNoLoggingT (createPostgresqlPool (connStr "_test") (envPool Test))
makePool Development =
    runStdoutLoggingT (createPostgresqlPool (connStr "") (envPool Development))
```

In `Testing`, we don't want it to print anything out, so we use the `runNoLoggingT` function from `Control.Monad.Logger` to tell `createPostgresqlPool` which instance of the `MonadLogger` type class it'll use.
Likewise, `Development` will be printing all of the logs to standard out.
We create a `connStr` with a database name suffix of "\_test" for testing and no suffix for development.

For production, it gets a bit trickier. We need to get the database environment
from `keter`, so we have to read each bit of the connection string in as
environment variables. This part of the function makes heavy use of the `MaybeT` monad
transformer, which might be confusing if you're not familiar with it. It allows
us to combine the effects from 'IO' and the effect of `Maybe` into a single
"big effect", so that when we bind out of `MaybeT IO a`, we get an `a`. If we
just had `IO (Maybe a)`, then binding out of the IO would give us a `Maybe a`,
which would make the code quite a bit more verbose.

```haskell
makePool Production = do
    pool <- runMaybeT $ do
        let keys = [ "host="
                   , "port="
                   , "user="
                   , "password="
                   , "dbname="
                   ]
            envs = [ "PGHOST"
                   , "PGPORT"
                   , "PGUSER"
                   , "PGPASS"
                   , "PGDATABASE"
                   ]
        envVars <- traverse (MaybeT . lookupEnv) envs
        let prodStr = mconcat . zipWith (<>) keys . fmap BS.pack $ envVars
        runStdoutLoggingT $ createPostgresqlPool prodStr (envPool Production)
```

`traverse` is *tons* of fun. If you know `map :: (a -> b) -> [a] -> [b]`, then `mapM` shouldn't be too scary: it's just `mapM :: (a -> m b) -> [a] -> m [b]`.
As it happens, the `m` in `mapM` doesn't have to a `Monad`, just `Applicative`, and it works for more things than just lists. 
In this case, `traverse` is taking each `String` in the `envs` list, looking it up in the environment and wrapping it in `MaybeT`, and finally evaluating a value of type `MaybeT IO [String]`.

We now have a list of keys, and a list of values. We zip them together with `<>` and concatenate them all into a big connection string, with which we create a pool.
Finally, we `runMaybeT` to convert the `MaybeT IO ConnectionPool` to an `IO (Maybe ConnectionPool`) and bind that value out.

```haskell
    case pool of
         Nothing -> error "Database Configuration not present in environment."
         Just a -> return a
```

If the database configuration isn't there, then we error out. Otherwise, we return it.
This *shouldn't* happen, as `keter` automatically manages the PostgreSQL database information for us on the deployment server.

# `src/Models.hs`

That covers making the pool. Running migrations was next. This step is neatly handled for us by the [`persistent`](http://www.yesodweb.com/book/persistent) library.
For further reading on that, check the chapter out. It's a great resource.

```haskell
doMigrations :: SqlPersistM ()
doMigrations = runMigration migrateAll
```

The `migrateAll` function is generated by the following Persistent Entity Definitions:

```haskell
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

User json
    name  String
    email String
    deriving Show

|]
```

The `json` keyword there means "please generate `FromJSON` and `ToJSON` instances for this entity," which is a really handy tool.

Persistent is smart enough to know if the current database schema is in line with what the entity definitions say.
If it is, then it doesn't do anything. If it can safely make the migrations, then it does so.
If it can't, then it helpfully prints the SQL necessary to the console for you to do yourself.

You'll probably want to move to something like [dbmigrations](https://hackage.haskell.org/package/dbmigrations) when your database is a bit more complicated, but Persistent's migrations are still really useful to verify that your data looks like you expect.
You can run `printMigration` to just print out what Persistent would do.

Easy! Let's see how we're generating the JavaScript now. That function was imported from `Api.User`, which we'll check out next.

# `src/Api/User.hs`

In classic `servant` manner, we've got a little API we've defined:

```haskell
type UserAPI =
         "users" :> Get '[JSON] [Entity User]
    :<|> "users" :> Capture "name" String :> Get '[JSON] (Entity User)
    :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64
```

Along with our handlers for the server:

```haskell
-- | The server that runs the UserAPI
userServer :: ServerT UserAPI App
userServer = allUsers :<|> singleUser :<|> createUser

-- | Returns all users in the database.
allUsers :: App [Entity User]
allUsers =
    runDb (selectList [] [])
```

It still blows my mind how good Haskell's type inference is. `selectList` is a function that accepts a list of filters and a list of options, and returns a list of matching records.
Here, we provide nothing other than the inferred return type of `Entity User` and it knows how to run the query.

```haskell
-- | Returns a user by name or throws a 404 error.
singleUser :: String -> App (Entity User)
singleUser str = do
    maybeUser <- runDb (selectFirst [UserName ==. str] [])
    case maybeUser of
         Nothing ->
            throwError err404
         Just person ->
            return person

-- | Creates a user in the database.
createUser :: User -> App Int64
createUser p = do
    newUser <- runDb (insert (User (userName p) (userEmail p)))
    return $ fromSqlKey newUser
```

Here's a neat trick: `App (Entity User)` is *just a function*.
We can easily reuse that handler code in the rest of the codebase if we wanted to, and it'd do the right thing.

Finally, the JavaScript generation:

```haskell
-- | Generates JavaScript to query the User API.
generateJavaScript :: IO ()
generateJavaScript =
    writeJSForAPI (Proxy :: Proxy UserAPI) vanillaJS "./assets/api.js"
```

Well, what does that look like?

# `assets/api.js`

The generated code isn't super pretty, but it gets the job done.

```javascript
var getUsers = function(onSuccess, onError)
{
  var xhr = new XMLHttpRequest();
  xhr.open('GET', '/users', true);
  xhr.setRequestHeader("Accept","application/json");
  xhr.onreadystatechange = function (e) {
    if (xhr.readyState == 4) {
      if (xhr.status == 204 || xhr.status == 205) {
        onSuccess();
      } else if (xhr.status >= 200 && xhr.status < 300) {
        var value = JSON.parse(xhr.responseText);
        onSuccess(value);
      } else {
        var value = JSON.parse(xhr.responseText);
        onError(value);
      }
    }
  }
  xhr.send(null);
}
```

This is just the `vanillaJS` option. There's also jQuery and AngularJS options available.

The same machinery that generates client JavaScript code can also be used to generate [Ruby](https://hackage.haskell.org/package/lackey) clients, if you need them.

Now, we still need to serve up some static files. We do that in the `app` function, imported from `Api`.

# `src/Api.hs`

This is the function we export to run our `UserAPI`. Given
a `Config`, we return a WAI `Application` which any WAI compliant server
can run.

```haskell
userApp :: Config -> Application
userApp cfg = serve (Proxy :: Proxy UserAPI) (appToServer cfg)
```

This functions tells Servant how to run the `App` monad with the Servant provided `server` function.

```haskell
appToServer :: Config -> Server UserAPI
appToServer cfg = enter (convertApp cfg) userServer
```

This function converts our `App` monad into the `ExceptT ServantErr IO` monad that Servant`s `enter' function needs in order to run the
application. The `:~>` type is a natural transformation, or, in
non-category theory terms, a function that converts two type
constructors without looking at the values in the types.

```haskell
convertApp :: Config -> App :~> ExceptT ServantErr IO
convertApp cfg = Nat (flip runReaderT cfg . runApp)
```

Since we also want to provide a minimal front end, we need to give Servant a way to serve a directory with HTML and JavaScript.
This function creates a WAI application that just serves the files out of the given directory.

```haskell
files :: Application
files = serveDirectory "assets"
```

Just like a normal API type, we can use the `:<|>` combinator to unify two different APIs and applications.
This is a powerful tool for code reuse and abstraction! We need to put the 'Raw' endpoint last, since it always succeeds.

```haskell
type AppAPI = UserAPI :<|> Raw

appApi :: Proxy AppAPI
appApi = Proxy
```

Finally, this function takes a configuration and runs our `UserAPI` alongside the `Raw` endpoint that serves all of our files.

```haskell
app :: Config -> Application
app cfg =
    serve appApi (readerServer cfg :<|> files)
```

Now, we can do:

```sh
$ stack build
$ stack exec perservant
```

and open `localhost:8081` to see our primitive little UI.

We're done, right? Well, sort of! There's also deployment with `keter`!

# Deployment

`keter` is a *very* nice little utility for deploying Haskell applications. Here's the configuration required for the app:

```yaml
# config/keter.yaml

exec: ../perservant
host: your.host.name.com

plugins:
    postgres: true
```

And the deployment script:

```sh
#! /bin/bash

set -e
echo "Building Perservant..."
stack build
strip `stack exec -- which perservant`
echo "Creating bundle..."
cp `stack exec -- which perservant` perservant
tar -czvf perservant.keter perservant config ql-ui/assets
rm perservant
scp ./perservant.keter user@host:/opt/keter/incoming/perservant.keter
rm perservant.keter
```

And that's all you need to get going!
