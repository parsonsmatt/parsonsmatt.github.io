---
title: "Spooky Masks and Async Exceptions"
date: 2022-10-29
layout: post
categories: programming
---

Everyone loves Haskell because it makes concurrent programming so easy!
`forkIO` is great, and you've got `STM` and `MVar` and other fun tools that are pleasant to use.

Well, then you learn about asynchronous exceptions.
The world seems a little scarier - an exception could be lurking around any corner!
Anyone with your `ThreadId` could blast you with a `killThread` or `throwTo` and you would have no idea what happened.

The `async` library hides a lot of this from you by managing the `forkIO` and `throwTo` stuff for you.
It also makes it easy to wait on a thread to finish, and receive exceptions that the forked thread died with.
Consider how nice the implementation of `timeout` is here:

```haskell
timeout :: Int -> IO a -> IO (Maybe a)
timeout microseconds action = do
  withAsync (Just <$> action) $ \a0 ->
  withAsync (Nothing <$ threadDelay microseconds) $ \a1 ->
      either id id <$> waitEither a0 a1
```

The `async` library uses asynchronous exceptions to signal that a thread *must die*.
The `withAsync` function guarantees that the forked thread is killed off when the inner action is complete.
So `timeout` will fork a thread to run `Just <$> action`, and then fork another thread to `threadDelay`.
`waitEither` accepts an `Async a` and an `Async b` and returns an `IO (Either a b)` - whichever one finishes first determines the return type.
If `threadDelay` finishes first, then we get a `Right Nothing` as the return, and exits.
This spells doom for the `action` thread.

But if our brave hero is able to escape before the deadline, it's the `threadDelay` that gets killed!

Indeed, this is a specialization of `race :: IO a -> IO b -> IO (Either a b)`, which runs two `IO` actions in separate threads.
The first to complete returns the value, and the remaining thread is sacrificed to unspeakable horrors.

But, you really shouldn't `catch` or `handle` async exceptions yourself.
GHC uses them to indicate "you really need to shut down extremely quickly, please handle your shit *right now*."
`ThreadKilled` is used to end a thread's execution, and `UserInterrupt` means that you got a `SIGINT` signal and need to stop gracefully.
The `async` package uses `AsyncCancelled` to, well, cancel threads.
However, the `base` package's `Control.Exception` has a footgun: if you catch-all-exceptions by matching on `SomeException`, then you'll catch these async exceptions too!

Now, you should *pretty much never* be catching `SomeException`, unless you really *really* know what you're doing.
But I see it all the time:

```haskell
import Control.Exception (catch)

blah = 
    Just <$> coolThing 
        `catch` \(SomeException e) -> do
            reportException e
            pure Nothing
```

If `coolThing` receives a `ThreadKilled` or an `AsyncCancelled` or `UserInterrupt` or anything else from `throwTo`, it'll catch it, report it, and then your program will continue running.
Then the *second* `Ctrl-C` comes from the user, and your program halts immediately without running any cleanup.
This is pretty dang bad!
You really want your `finally` calls to run.

You search for a bit, and you find the [`safe-exceptions`](https://hackage.haskell.org/package/safe-exceptions) package.
It promises to make things a lot nicer by *not* catching async exceptions by default.
So our prior code block, with just a change in import, becomes much safer:

```haskell
import Control.Exception.Safe (catch)

blah = 
    Just <$> coolThing 
        `catch` \(SomeException e) -> do
            reportException e
            pure Nothing
```

This code will no longer catch and report an async exception.
However, the blocks in your `finally` and `bracket` for cleanup *will* run!

Unfortunately, the `safe-exceptions` library (and the `unliftio` package which uses the same behavior), have a dark secret...

`*thunder claps in the distance, as rain begins to fall*`

... they wear *spooky masks* while cleaning! WowowoOOOoOoOooOooOOooOooOOo

No, really, they do something like this:

```haskell
bracket provide cleanup action = 
    Control.Exception.bracket
        provide
        (\a -> 
            Control.Exception.uninterruptibleMask_ $ 
                cleanup a)
        action
```

This code looks pretty innocuous.
It even says that it's good! 
"Your cleanup function is guaranteed not to be interrupted by an asynchronous exception."
So if you're cleaning things up, and **BAMM** a vampire `ThreadKill`s you, you'll finish your cleanup before rethrowing.
This might just be all you need to make it out of the dungeon alive.

Behind the sweet smile and innocent demeanor of the `safe-exceptions` package, though, is a dark mystery - and a vendetta *for blood*.
Well, maybe not blood, but I guess "intercompatibility of default expectations"?

# A Nightmare Scenario: Night of the Living Deadlock

Once, a brave detective tried to understand how slow the database was.
But in her studies, she accidentally caused the *the entire app to deadlock and become an unkillable zombie?!* 

There are three actors in this horror mystery.
Mr DA, the prime suspect.
Alice, our detective.
And Bob, the unassuming janitor.

## Mr Database Acquisition

One of the suspected villains is Mr. Database Acquisition, a known rogue.
Usually, Mr. Database Acquisition works quickly and effectively, but sometimes everything stops and he's nowhere to be found.
We're already recording how long he takes by measuring the job *completion* time, but if the job never finishes, we don't know anything.

The database connection is provided from a `resource-pool` `Pool`, which is supposed to be thread safe and guarantee resource allocation.
But something seems shady about it...

## Alice 

Alice is a performance engineer and lead detective.
She's interested in making the codebase faster, and to do so, she sets up inspection points to log how long things are taking.

Alice cleverly sets up a phantom detective - a forked thread that occasionally checks in on Mr Database.

```haskell
withAcquisitionTimer
    :: (IO () -> IO r) -> IO r
withAcquisitionTimer action = do
    timeSpent <- newIORef 0
    let tracker = 
            forever $ do
                threadDelay 1000
                timeSpent <- atomicModifyIORef' timeSpent (\a -> (a+1000, a+1000))
                recordMetric runningWait timeSpent

        report = do
            elapsed <- readIORef timeSpent
            recordMetric totalWait elapsed

    withAsync (tracker `finally` report) $ \a ->
        action (cancel a)
```

The actual implementation is a bit more robust and sensible, but this gets the gist across.
Pretend we're in a campy low budget horror movie.

The `tracker` thread wakes up every millisecond to record how long we're waiting, and continues running until the thread is finally cancelled, or killed with an async exception, or the `action` finishes successfully, or if a regular exception causes `action` to exit early.
`withAsync` will `cancel` the tracker thread, ensuring that we don't leak threads.
Part of `cancel`'s API is that it doesn't return until the thread is totally, completely, certainly dead - so when `withAsync` returns, you're *guaranteed* that the thread is dead.

Alice sets the tracker up for every database acquisition, and waits to see what's really going on.

## Bob, the Janitor

```haskell
theSceneOfTheCrime =
    bracket 
        (runDB startProcess) 
        (\processId -> runDB (closeProcess processId)) 
        $ \processId -> do
            doWorkWith processId
            {- ... snip ... -}
```

There's a great big mess - it appears that someone was thrown from a high building!
Foul play is suspected from the initial detective work.
But after the excitement dies down, the janitor, Bob, is left to clean up the mess.

One of the perks of being a janitor is *protection from all sorts of evil*.
While you're cleaning stuff up, nothing spooky can harm you - no async exceptions are allowd.
You might expect there's a loophole here, but it's fool proof.
It's such a strong protection that the janitor is even able to bestow it upon anyone that works for him to help clean up.

Bob begins cleaning up by recording the work he's doing in the database.
To do this, he requests a database connection from Mr Database.
However, this provides Mr Database with the same protections: no one can kill him, *or anyone that works for him*!

Now, by the particular and odd rules of this protection magic, you don't have to *know* that someone is working for you.
So the *phantom tracker* that Alice set up is similarly extended this protection.

Mr Database provides the database connection to Bob in a prompt manner, and Bob completes his task.
However, when Bob attempts to *release* the database back, he can't!
The database connection is permanently stuck to his hand.
Mr Database can't accept it back and put it in the pool, and he can't continue to his next job.
The entire application comes grinding to a halt, as no one can access the database.

What kind of bizarre curse is this?

# The Gift of Safety

`withAsync` wants to be safe - it wants to guarantee that the forked thread is killed when the block exits.
It accomplishes this by effectively doing:

```haskell
withAsync thread action = 
    bracket
        (async thread)
        uninterruptibleCancel
        action
```

`async` forks the thread and prepares the `Async`:

```haskell
async action = do
   var <- newEmptyTMVarIO
   threadId <- mask $ \restore ->
          forkIO $ try (restore action) >>= atomically . putTMVar var
   return Async 
      { asyncThreadId = threadId 
      , _asyncWait = readTMVar var
      }
```

`async` is careful to `mask` the `forkIO` call, which ensures that the forked thread is `mask`ed.
That allows `action` to receive async exceptions, but *outside* of `action`, it's guaranteed that if `try` succeeds, then the `atomically . putTMVar var` also succeeds.
Since `try` will catch async exceptions, this means that the async exception will *definitely* be registered in the `putTMVar` call.

`uninterruptibleCancel` cancels the thread in an uninterruptible state.
`cancel` waits for the thread to complete - either with an exception or a real value.

Meanwhile, `bracket` is *also* cursed with safety:

```haskell
module UnliftIO.Exception where

bracket make clean action = 
    withRunInIO $ \runInIO ->
        Control.Exception.bracket
            (runInIO make)
            (\a -> uninterruptibleMask_ $ runInIO $ clean a)
            (\a -> runInIO $ action a)
```

# The Curse of Two Gifts

Unspeakable magical rules dictate that *two* gifts form a curse, under the usual laws for associativity and commutativity.

To understand what's going on, we start by inlining the bracket.

```haskell
crimeSceneCleanedUp =
    withRunInIO $ \runInIO ->
    bracket
        (runInIO $ runDB createProcess)
        (\pid -> 
            uninterruptibleMask_ $ do
                runInIO $ runDB $ do
                    closeProcess pid
        )
        _stuff
```

We know that the `make` and `action` managed to complete, so we're interested in the cleanup.
Let's expand `runDB` annd omit some noise:

```haskell
crimeSceneCleanedUp =
    withRunInIO $ \runInIO ->
           
                                       
                 
            uninterruptibleMask_ $ do
                runInIO $ do
                    sqlPool <- getSqlPool
                    withAcquisitionTimer $ \stop ->
                        flip runSqlPool sqlPool $ do
                            stop 
                            closeProcess pid
```

Hmm! That `withAcqusitionTimer` is new!
Enhance!!


```haskell
crimeSceneCleanedUp =
    withRunInIO $ \runInIO ->
           
                                       
                 
            uninterruptibleMask_ $ do
                runInIO $ do
                    sqlPool <- getSqlPool
                    
                    withAsync (task `finally` record) $ \async ->
                        flip runSqlPool sqlPool $ do
                        cancel async 
                        closeProcess pid
```

Uh oh.
Let's zoom in on `withAsync` (and get rid of some indentation):

```haskell
crimeSceneCleanedUp =
    uninterruptibleMask_ $ do
        sqlPool <- getSqlPool
        
        bracket 
            (async (task `finally` record))
            (uninterruptibleCancel)
            $ \async ->
            flip runSqlPool sqlPool $ do
                cancel async 
                closeProcess pid
```

One more level!

```haskell
crimeSceneCleanedUp =
    uninterruptibleMask_ $ do
        sqlPool <- getSqlPool
        
        bracket 
            (do
                var <- newEmptyTMVarIO
                threadId <- mask $ \restore ->
                    forkIO $ do
                        eres <- try $ restore $ 
                            task `finally` record 
                        atomically $ putTMVar var eres
                return Async 
                    { asyncThreadId = threadId 
                    , _asyncWait = readTMVar var
                    }
            uninterruptibleCancel
            $ \async ->
            flip runSqlPool sqlPool $ do
                cancel async 
                closeProcess pid
```

Uh oh.
`forkIO` inherits the masking state from the parent thread.
This means that `uninterruptibleMask_` state, set by `bracket`'s `cleanup`, is inherited by our `forkIO`.

Let's zoom back out on that `async` call and inline the `task`...

```haskell
crimeSceneCleanedUp =
    uninterruptibleMask_ $ do
        withAsync 
            (do
                forever $ do
                    threadDelay 1000
                    {- hmm -}
             `finally` record) $ \async ->
            {- snip -}
```

Ah!
That's the zombie.
Reducing it to it's most basic nature, we have:

```haskell
zombie :: IO (Async a)
zombie =
    uninterruptibleMask_ $
        async $ 
            forever $ 
                threadDelay 1000
```

`uninteruptibleMask_` means "I cannot be killed by async exceptions."
`async` allows the forked thread to inherit the masking state of the parent.
But about half of the API of `async` *requires* that the forked thread can be killed by async exceptions.
`race` is completely broken with unkillable `Async`s.

The solution is to use `withAsyncWithUnmask`:

```haskell
safeWithAsync thread action =
    withAsyncWithUnmask (\unmask -> unmask thread) action
```

This unmasks the child thread, revealing it to be an imposter all along.

> And I would have ~gotten away with it~ never exited and consumed all resources, if it weren't for you danged kids!!!

The unmasked phantom thread, free from it's curse of safety, was killed and returned to the phantom aether to be called upon in other sorcery.
