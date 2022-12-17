---
title: "Three Layer Haskell Cake"
date: 2018-03-22
layout: post
categories: programming
---

The question of "How do I design my application in Haskell?" comes up a lot.
There's a bunch of perspectives and choices, so it makes sense that it's difficult to choose just one.
Do I use plain monad transformers, `mtl`, just pass the parameters manually and use `IO` for everything, the [`ReaderT` design pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern), [free monads](http://www.parsonsmatt.org/2017/09/22/what_does_free_buy_us.html), free*r* monads, some other kind of algebraic effect system?!

The answer is: why not both/all?
Each approach has pros and cons.
Instead of sticking with one technique for everything, let's instead leverage all of the techniques where they shine.
Lately, I've been centering on an application design architecture with roughly three layers.

# Layer 1: 

```haskell
newtype AppT m a 
    = AppT 
    { unAppT :: ReaderT YourStuff m a 
    } deriving (Functor, Applicative, Monad, etc)
```

The  [`ReaderT` Design Pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern), essentially.
This is what everything gets boiled down to, and what everything eventually gets interpreted in.
This type is the backbone of your app.
For some components, you carry around some info/state (consider [`MonadMetrics`](https://hackage.haskell.org/package/monad-metrics) or  [`katip`'s](https://hackage.haskell.org/package/katip-0.5.2.0/docs/Katip.html) logging state/data); for others, you can carry [an explicit effect interpreter](http://www.parsonsmatt.org/2016/07/14/rank_n_classy_limited_effects.html).
This layer is for defining how the upper layers work, and for handling operational concerns like performance, concurrency, etc.

At IOHK, we had a name for this kind of thing: a "capability".
We have a [big design doc on monads](https://github.com/parsonsmatt/cardano-sl/blob/10e55bde9a5c0d9d28bca25950a8811407c5fc8c/docs/monads.md), and the doc goes into what makes something a capability or not.
IOHK has since deleted this design document and decided that it wasn't good to follow.

This layer sucks to test.
So don't.
Shift all the business logic up into the next two layers as much as possible.
You want this layer to be tiny.
If you get a request to test something in this layer, don't - factor the logic out, test *that* function, and call it in IO.

How do you shift something out?
I wrote a post on [Inverting your Mocks](http://www.parsonsmatt.org/2017/07/27/inverted_mocking.html) that I believe covers it well, but the general routine is:

- Factor the *inputs* of code out
- Represent the *output* of effects as data returned from pure functions

If I had to give a name to this layer, I'd call it the "orchestration" layer.
All of the code has been composed, and now we're arranging it for a real performance.

# Layer 2

This layer provides a bridge between the first and third layer.
Here, we're mostly interested in mocking out external services and dependencies.
The most convenient way I've found to do this are `mtl` style classes, implemented in terms of domain resources or effects.
This is a trivial example:

```haskell
class MonadTime m where 
    getCurrentTime :: m UTCTime
```
    
`MonadTime` is a class that I might use to "purify" an action that uses IO only for the current time.
Doing so makes unit testing a time based function easier.
However -- this isn't a great use for this.
The best "pure" instance of this is 

```haskell
instance MonadTime ((->) UTCTime) where
    getCurrentTime = id
```

And, if you've factored your effects out, this will already be done for you.
Furthermore, it would actually be quite difficult to write a realistic `MonadTime` mock.
One law we might like to have with `getCurrentTime` is that:

```haskell
timeLessThan = do
  x <- getCurrentTime
  y <- getCurrentTime
  pure (x < y)
```

A pure implementation returning a constant time would fail this.
We could have a `State` with a random generator and a `UTCTime` and add a random amount of seconds for every call, but this wouldn't really make testing any easier than just getting the actual time.
Getting the current time is best kept as a Layer 1 concern - don't bother mocking it.

A more realistic example from a past codebase is this:

```haskell
class Monad m => MonadLock m where
    acquireLock 
        :: NominalDiffTime 
        -> Key 
        -> m (Maybe Lock)
    renewLock 
        :: NominalDiffTime 
        -> Lock 
        -> m (Maybe Lock)
    releaseLock 
        :: Lock 
        -> m ()
```

This class describes logic around implementing distributed locks.
The production instance talked to a Redis datastore.
Setting up redis for dev/test sounded annoying, so I implemented a testing mock that held an `IORef (Map ByteString ByteString)`.

Another good class is a simplified DSL for working with your data.
In OOP land, you'd call this your "Data Access Object."
It doesn't try to contain a full SQL interpreter, it only represents a small set of queries/data that you need.

```haskell
class (Monad m) => AcquireUser m where
    getUserBy :: UserQuery -> m [User]
    getUser :: UserId -> m (Maybe User)
    getUserWithDog :: UserId -> m (Maybe (User, Dog))

class AcquireUser m => UpdateUser m where
    deleteUser :: UserId -> m ()
    insertUser :: User -> m ()
```

We can use this class to provide a mock database for testing, without having to write an entire SQL database mocking system.
These classes also come in handy because you can swap out the underlying production implementations.
Suppose you have a microservices system going on, and `AcquireUser` is done through an HTTP API.
Suddenly, your boss is convinced that monoliths are king, and gives you One Large Server To Rule Them All.
Now your HTTP API has direct database access to the underlying data -- you can make SQL requests instead of HTTP!
How wonderful.

These are higher level than `App` and delimit the effects you use; but are ultimately lower level than real business logic.
You might see some `MonadIO` in this layer, but it should be avoided where possible.
This layer should be expanded on an as-needed (or as-convenient) basis.
As an example, implementing `MonadLock` as a class instead of directly in `AppT` was done because using Redis directly would require that every development and test environment would need a full Redis connection information.
That is wasteful so we avoid it.
Implementing `AcquireModel` as a class allows you to omit database calls in testing, and if you're real careful, you can isolate the database tests well.

*DO NOT* try to implement `MonadRedis` or `MonadDatabase` or `MonadFilesystem` here.
That is a fool's errand.
Instead, capture the tiny bits of your domain: `MonadLock`, `MonadModel`, or `MonadSpecificDataAcquisition`.
The smaller your domain, the easier it is to write mocks and tests for it.
You probably don't want to try and write a SQL database, so don't -- capture the queries you need as methods on the class so they can easily be mocked.
Alternatively, present a tiny query DSL that *is* easy to write an interpreter for.

This layer excels at providing swappable implementations of external services.
This technique is still quite heavy-weight: `mtl` classes require tons of `newtype`s and instance boilerplate.
This layer should be as thin as possible, preferring to instead push stuff into the Layer 3.

# Layer 3: 

Business logic.
This should be entirely pure, with no `IO` component at all.
This should almost always just be pure functions and relatively simple data types.
Reach for *only* as much power as you need -- and you need much less than you think!

All the effectful data should have been acquired beforehand, and all effectful post-processing should be handled afterwards.
My post on [inverting your mocks](http://www.parsonsmatt.org/2017/07/27/inverted_mocking.html) goes into detail on ways to handle this.
If you need streaming, then you can implement "pure" `conduit` or `pipe`s with a type signature like this:

```haskell
pureConduit 
    :: Monad m 
    => Conduit i m o
```

This expresses no dependency on *where* the data comes from, nor on how the output is handled.
We can easily run it with mock data, or put it in the real production pipeline.
It is *abstract* of such concerns.
As a result, it's lots of fun to test.

If the result of computation needs to perform an effect, then it is useful to encode that effect as a datatype.
Free monads are a technique to encode computation as data, but they're very complicated; you can usually get away with a much simpler datatype to express the behavior you want.
Often times, a simple non-recursive sum type "command" suffices as an interface between a pure function and an effectful one.
A list of commands adds a lot of flexibility without dramatically complicating things.

Before you jump to monads, consider: would a monoidal means of constructing/reading this data work?
If not, what about a Free Applicative?
What about a limited recursive sum type, perhaps a GADT, that can express what I want to do?

When you're testing pure functions emitting data structures, it's a dream.
This is the Haskell we know and love.
So try to put as much of your code into the pleasant, testable, QuickCheckable, type-verified bits as possible.
If you manage to isolate your application like this, then you won't need to test your IO stuff (aside from the usual integration testing).

May all of your tests be pleasant and your software correct.

### Addendum

When I initially wrote this blog post, it really bothered me that I couldn't come up with a good name for layers 2 and 3.
I published it anyway because it's useful enough with just the numbers, and names have the potential to mislead.
I've since realized that the layers *already* have names!

1. Imperative programming
2. Object Oriented programming
3. Functional programming

I've provided a new explanation of the ["functional core, imperative shell"](https://www.destroyallsoftware.com/talks/boundaries) model of programming!

### Examples

I get folks asking me for examples fairly regularly.
Unfortunately, I haven't had time to write an OSS app using this technique.
Fortunately, other folks have!

- [Holmusk/three-layer](https://github.com/Holmusk/three-layer)
- [thomashoneyman/purescript-halogen-realworld](https://github.com/thomashoneyman/purescript-halogen-realworld)
- [defect-process](https://github.com/incoherentsoftware/defect-process) is a 62kloc Haskell video game project
