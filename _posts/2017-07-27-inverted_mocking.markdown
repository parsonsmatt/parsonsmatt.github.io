---
title: "Inverted Mocking"
date: 2017-07-27
layout: post
categories: programming
---

Mocking comes up a lot in discussions of testing effectful code in Haskell.
One of the advantages for `mtl` type classes or `Eff` freer monads is that you can swap implementations and run the same program on different underlying interpretations.
This is cool!
However, it's an extremely heavy weight technique, with a ton of complexity.
I've recently gravitated to mostly doing everything in this sort of transformer:

```haskell
newtype App a = App { unApp :: ReaderT AppCtx IO a }
```

It's simple, has great error messages, and is easy to hook into existing libraries and frameworks by writing instances for either `AppCtx` or `App`.
There's a small cost: I have to call `lift` manually if I use an `App a` function inside of a Conduit or `MaybeT` block or similar.
This is a fairly small cost to pay, all told, and the benefits in getting new developers up to speed on our projects is a big sell.

Now, how would I go about testing this sort of function?

```haskell
doWork :: App ()
doWork = do
  query <- runHTTP getUserQuery
  users <- runDB (usersSatisfying query)
  for_ users $ \user -> do
    thing <- getSomething user
    let result = compute thing
    runRedis (writeKey (userRedisKey user) result)
```

If we have our `mtl` or `Eff` or OOP mocking hats on, we might think:

> I know! We need to mock our HTTP, database, and Redis effects. Then we can control the environment using mock implementations, and verify that the results are sound!

Hmm. Let's step back and apply some more elementary techniques to this problem.
I bet we can simplify our solution to testing.

# Decomposing Effects

The first thing we need to do is recognize that *effects* and *values* are separate, and try to keep them as separate as possible.
Generally speaking, functions that look like:

```haskell
doWork :: App ()
```

are not functional.
The only point to this is to run it for the effect it has on the outside world.
We can tell just by looking at the type signature!
So, let's look at what it does, and how we might test it:

```haskell
doWork :: App ()
doWork = do
  query <- runHTTP getUserQuery
  users <- runDB (usersSatisfying query)
  for_ users $ \user -> do
    thing <- getSomething user
    let result = compute thing
    runRedis (writeKey (userRedisKey user) result)
```

We get a bunch of stuff -- inputs -- that are acquired as an *effect*.
We can make this a lot easier to test by simply taking those things as inputs.

```haskell
doWork :: App ()
doWork = do
  query <- runHTTP getUserQuery
  users <- runDB (usersSatisfying query)
  doWorkHelper users

doWorkHelper :: [User] -> App ()
doWorkHelper users =
  for_ users $ \user -> do
    thing <- getSomething user
    let result = compute thing
    runRedis (writeKey (userRedisKey user) result)
```

Now, the only effect we need to mock for the `doWorkHelper` is `getSomething` and `runRedis`.
But I'm not satisfied.
We can get rid of the `getSomething` by factoring another helper out.

```haskell
doWorkHelper :: [User] -> App ()
doWorkHelper users = do
  things'users <- for users $ \user -> do
    thing <- getSomething user
    pure (thing, user)
  lookMaNoInputs thing'users

lookMaNoInputs :: [(Thing, User)] -> App ()
lookMaNoInputs things'users =
  for_ things'users $ \(thing, user) -> do
    let result = compute thing
    runRedis (writeKey (userRedisKey user) result)
```

Ah, can we decompose this further?
We can!
Let's inspect our output effect:

```haskell
runRedis (writeKey (userRedisKey user) result)
```

It expects two things:

1. The user's Redis key
2. The computed result from the `thing`.

We can prepare the redis key and computed result fairly easily:

```haskell
businessLogic :: (Thing, User) -> (RedisKey, Result)
businessLogic (thing, user) = (userRedisKey user, compute result)

lookMaNoInputs :: [(Thing, User)] -> App ()
lookMaNoInputs users = do
  for_ (map businessLogic users) $ \(key, result) -> do
    runRedis (writeKey key result)
```

neat! We've isolated the core business logic out and now we can write nice unit tests on that business logic.

# Decomposition: Conduit-style

Streaming libraries like `Pipes` and `Conduit` are a great way to handle large data sets and interleave effects.
They're *also* a great way to decompose functions and provide "inverted mocking" facilities to your programs.

Most conduits look like this:

```haskell
import Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.List as CL

streamSomeStuff :: IO ()
streamSomeStuff = do
  runConduit
     $ conduitThatGetsStuff
    .| conduitThatProcessesStuff
    .| conduitThatConsumesStuff
```

You have some `Source` or `Producer` that initially provides things.
This can be from a database action, an HTTP request, or from a file handle.
Now, each part of this conduit can itself have many conduits inside of it:


```haskell
conduitThatGetsStuff :: Producer IO ByteString
conduitThatGetsStuff = ...

conduitThatProcessesStuff :: Conduit ByteString IO RealThing
conduitThatProcessesStuff =
  CL.mapM (\bs -> 
    case parseFromByteString bs of
      Left err ->
        throwIO err
      Right yesss ->
        pure yesss
    )
  .| CL.map convertSomeThing
  .| CL.filter someFilterCondition

passThrough :: (a -> IO ()) -> Conduit a IO a
passThrough action = CL.mapM (\a -> do
  action a
  pure a)

conduitThatConsumesStuff :: Consumer RealThing IO ()
conduitThatConsumesStuff =
  passThrough print
  .| passThrough makeHttpPost
  .| CL.mapM_ saveToDatabase
```

We have a bunch of small, decomposed things.
Our `conduitThatProcessesStuff` doesn't care where it gets the `ByteString`s that it parses -- you can hook it up to *anything*.
Databases, HTTP calls, file IO, or even just `CL.sourceList [example1, example2, example3]`.

Likewise, the `conduitThatConsumesStuff` doesn't care where the `RealThing`s come from.
You can use `CL.sourceList` to provide a bunch of fake input.

We're not usually working directly with `Conduit`s here, either -- most of the functions are provided to `CL.mapM_` or `CL.filter` or `CL.map`.
That allows us to write functions that are simple `a -> m b` or `a -> Bool` or `a -> b`, and these are really easy to test.

# Plain ol' abstraction

Always keep in mind the lightest and most general techniques in functional programming:

1. Make it a function
2. Abstract a parameter

These will get you very, very far.

Let's revisit the `doWork` business up top:

```haskell
doWork :: App ()
doWork = do
  query <- runHTTP getUserQuery
  users <- runDB (usersSatisfying query)
  for_ users $ \user -> do
    thing <- getSomething user
    let result = compute thing
    runRedis (writeKey (userRedisKey user) result)
```

We can make this *abstract* by taking concrete terms and making them function parameters.
The literal definition of lambda abstraction!

```haskell
doWorkAbstract
    :: Monad m
    => m Query
    -- ^ The HTTP getUserQuery
    -> (Query -> m [User])
    -- ^ The database action
    -> (User -> m Thing)
    -- ^ The getSomething function
    -> (RedisKey -> Result -> m ())
    -- ^ finally, the redis action
    -> m ()
doWorkAbstract getUserQuery getUsers getSomething redisAction = do
  query <- getUserQuery
  users <- getUsers
  for_ users $ \user -> do
    thing <- getSomething user
    let result = compute thing
    redisAction (userRedisKey user) result
```

There are some interesting things to note about this abstract definition:

1. It's parameterized over *any* monad. `Identity`, `State`, `IO`, whatever. You choose!
2. We have a pure specification of the effect logic. This can't *do* anything. It just describes what to do, when given the right tools.
3. This is basically dependency injection on steroids.

Given the above abstract definition, we can easily recover the concrete `doWork` by providing the necessary functions:

```haskell
doWork :: App ()
doWork = 
  doWorkAbstract 
    (runHTTP getUserQuery)
    (\query -> runDB (usersSatisfying query))
    (\user -> getSomething user)
    (\key result -> runRedis (writeKey key result))
```

We can also easily get a testing variant that logs the actions taken:

```haskell
doWorkScribe :: Writer [String] ()
doWorkScribe = 
  doWorkAbstract getQ getUsers getSomething redis
  where
    getQ = do
      tell ["getting users query"]
      pure AnyUserQuery
    getUsers _ = do
      tell ["getting users"]
      pure [exampleUser1, exampleUser2]
    getSomething u = do
      tell ["getting something for " <> show u]
      pure (fakeSomethingFor u)
    redis k v = do
      tell ["wrote k: " <> show k]
      tell ["wrote v: " <> show v]
```

All without having to fuss about with monad tranformers, type classes, or anything else that's terribly complicated.

# Decompose!!!

Ultimately, this is all about decomposition of programs into their smallest, most easily testable parts.
You then unit or property test these tiny parts to ensure they work together.
If all the parts work independently, then they should work together when composed.

Your effects should ideally not be anywhere near your business logic.
Pure functions from `a` to `b` are ridiculously easy to test, especially if you can express properties.

If your business logic really needs to perform effects, then try the simplest possible techniques first: functions and abstractions.
