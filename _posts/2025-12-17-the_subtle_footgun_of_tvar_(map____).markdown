---
title: "The Subtle Footgun of TVar (Map _ _)"
date: 2025-12-17
layout: post
categories: programming
---

> How coarse-grained STM containers can livelock under load

Software Transactional Memory (STM) is one of Haskell's crown jewels.
The promise is easy, lock-free concurrency with guaranteed transactional semantics and great performance.
Used correctly, you get all of these benefits.
However, concurrency is fundamentally difficult, and STM has a failure mode: livelock.

Livelock is when the program is repeatedly retrying transactions - working furiously and making no progress.
Livelock is subtle and extremely difficult to diagnose.
The problem of livelock is often simply "performance is really bad and seems to get worse with more concurrency."
Avoiding livelock proactively will pay massive dividends in the performance of your concurrent systems, as well as the time spent diagnosing and fixing them later on.

# Avoiding Livelock on Shared Containers

A common causes of livelock is fortunately discoverable with simple text search: 

```haskell
uhoh :: TVar (Map k (TVar v)) -> STM ()

oops :: TVar (HashMap k v) -> STM ()

ohno :: TVar (Set v) -> STM ()
```

A `TVar` to a container data structure, like `Map k v` or `Set a` or `[a]` or `Seq a`, is a common source of livelock.
Any change to the `Map` invalidates any transaction that is reading from that `Map`, even if it is reading at a totally different key or value.
If the `Map` is being concurrently updated, you are nearly guaranteed to run into performance problems.

Fortunately, you can easily avoid this pattern, thanks to the `stm-containers` library.

# `stm-containers`

The [`stm-containers`](https://nikita-volkov.github.io/stm-containers/) package was released over ten years ago.
`stm-containers` allows you to have a structure *similar to* a `TVar (Map k v)` or `TVar (Map k (TVar v))` with the added benefit that it actually scales up with increased concurrency, avoiding the dreaded livelock.
Modifications to the map impact a significantly smaller portion of the map structure, so you're way less likely to run into livelock from concurrent updates.

Switching is really easy.
The API is mostly the same as the `containers` library, but modifications are effectful in `STM ()` instead of returning the new data structure.

```haskell
StmMap.lookup
    :: (Hashable k)
    => k
    -> StmMap.Map k v
    -> STM (Maybe v)

HashMap.lookup
    :: (Hashable k)
    => k
    -> HashMap k v
    -> Maybe v

StmMap.insert
    :: (Hashable k)
    => v -- note that the value is first, not the key
    -> k
    -> StmMap.Map k v
    -> STM ()

HashMap.insert
    :: (Hashable k)
    => k
    -> v
    -> HashMap k v
    -> HashMap k v
```

Here's an example of using `stm-containers` in practice, compared to a `TVar Map`.

```haskell
import qualified Data.Map as Map
import qualified StmContainers.Map as StmMap

-- old:
doStuff :: TVar (Map.Map String Int) -> String -> STM Int
doStuff tmap str = do
    map <- readTVar tmap
    let mval = Map.lookup str map
    let newVal = maybe 0 (+1) mval
    writeTVar (Map.insert str newVal map)
    pure newVal

-- new:
doStuff :: StmMap.Map String Int -> String -> STM Int
doStuff tmap str = do
    mval <- StmMap.lookup str tmap
    let newVal = maybe 0 (+1) mval
    StmMap.insert newVal str tmap
    pure newVal
```

The library also offers an interesting `focus` function, which allows you to combine operations at a single key into a single operation.

If you're sharing a `Map` or `Set`-like container across many threads, then you almost certainly want to just use `stm-containers`.
If you stop reading here, and merely use `stm-containers` by default, then you'll avoid a lot of pain without having to invest much time or energy.
`stm-containers` is faster and safer than a `TVar (Map k v)`.

[In this PR to the `hs-temporal-sdk` library](https://github.com/MercuryTechnologies/hs-temporal-sdk/pull/279), I mechanically translated a few `TVar (HashMap _ _)` to `stm-containers` datatypes.
We were observing degraded performance with increased concurrency, some of which were causing an ordinary 10 minute test suite run to time out after 50 minutes.
By switching from `TVar (HashMap _ _)` to `stm-containers`, the problem was completely fixed - performance remained consistent with increased concurrency.

# When should I not use `stm-containers`?

By default, you should use `stm-containers`.
It is very fast, simple to use, and has no downsides (aside from the downsides inherent to `STM` vs `MVar` or `IORef` based shared references).

A `TVar (Map k v)` isn't *always* going to be dangerous.
And that's part of the problem - there are some places where `TVar (Map k v)` won't be slow or break.
These rely on *non-local assumptions* about your code structure - you *cannot* enforce this easily without writing complicated wrappers that enforce encapsulation around how the shared reference is used.
Writing code that works great with a `TVar (Map k v)` in the small is quite easy, but guaranteeing that code won't break as it scales is challenging.

There are many situations where going from `TVar (Map k v)` to `StmMap.Map k v` will make a huge improvement.
There are no situations where going from `StmMap.Map k v` to `TVar (Map k v)` will make a significant improvement.

# What about `IORef (Map k v)`?

An `IORef (Map k v)` eliminates the possibility of livelock.
However, the `IORef` structure is not suitable for many writers.
To ensure a consistent view of the `Map`, you need to use `atomicModifyIORef`.
While a thread is doing an `atomicModifyIORef`, all other writes are blocked to the reference.
This means that an `IORef (Map k v)` is suitable if there is only one thread writing to the `IORef`.
If you have many writers, then threads will queue up and block on updating the entire `Map`.

`atomicModifyIORef` also requires the updating action to be *pure*.
If you need the modification to be effectful, *and* have a consistent view of the data, then you need to use either `stm-containers` or an `MVar`.

[In this PR to the `prometheus-haskell`](https://github.com/parsonsmatt/prometheus-haskell/pull/1) library, I demonstrate that an `IORef (Map k v)` has significantly worse performance than an `stm-containers` `Map k v` as concurrency scales up.
This change made a big difference to the performance of our metric collection code.

# What about `MVar (Map k v)`?

An `MVar (Map k v)` avoids the problem of livelock, but introduces another potential problem: deadlock.
An `MVar` is a *locking* concurrency mechanism, where you can block other threads by `takeMVar` and making the `MVar` empty.
This allows updating threads to lock the value, update the `MVar`, and unlock the value.
However, if your code fails to fill up an `MVar`, or if two threads are waiting on mutually held `MVar`s, then your code cannot make progress, and will be deadlocked.

One advantage that this has to a `TVar` is fairness.
Threads that attempt to read or take from an `MVar` are enqueued, and guaranteed to operate in a first-in-first-out manner.
`TVar`, on the other hand, will start every thread at once, and the first one to complete wins while everyone else must retry.

If you have multiple `MVar` variables that you want to coordinate on, then you are increasing the risk of deadlock.

# When is `TVar (Map k v)` safe?

The fundamental problem with `TVar (Map k v)` is that any write to the `TVar` will invalidate and retry every transaction that reads from it.

For this variable to be safe, you need a single thread performing updates, and other threads are only doing reads.
Additionally, those updates should ideally be relatively rare - if the thread is constantly updating single keys in the map, that will invalidate transactions frequently.
Instead, you'll want to batch updates to the `Map` and perform a whole replacement at once.

However, this is exactly the same limitation as `IORef (Map k v)` or an `MVar (Map k v)`.
If you only have a single `TVar (Map k v)` involved in your `STM` transactions, then you can simply switch to an `IORef` or `MVar` and enjoy increased performance.
If you have multiple `TVar` in your transaction, then you are at risk of livelock, and should use `stm-containers`.

# When is `TVar (Map k (TVar v))` safe?

`TVar (Map k (TVar v))` is slightly better than the above.
With this, any write to the top-level `TVar` will invalidate the transaction, but writes to the value `TVar`s will only cause contention on other transactions that are attempting to write to it.

For this to be safe, you must have a single thread that updates the `Map` *structure* (ie adding or removing keys).
Many threads can write on the map values and only experience the usual contention on a single variable.

However, this runs into the same problem as above: if you can safely refactor this to `IORef (Map k (TVar v))` (or `MVar`), then you are safe, otherwise, you are at risk of livelock and you should switch to `stm-containers`.

# The Semantics of a Reference Type

I love arguing semantics.
What else are we going to argue about, syntax?

If I say `IORef a`, I mean:

> This is a reference to an `a`.
> The reference may be read or written to from any thread.
> The reference is very fast for these operations.
> However, the only operation for atomic consistency is `atomicModifyIORef`.
> So do not expect to be able to do transactions or blocking with this!

We have a *very fast* reference with very few guarantees.
This is suitable for sharing information among many threads, where updates don't happen very often, and updates are fast and pure.

Meanwhile, if I say `MVar a`, I mean:

> This is a reference to an `a`, which may be full or empty.
> The reference may be read or written to from any thread.
> If the reference is empty, then other threads will queue up and wait for it to be full.
> This means I can implement control, atomic updates, and fair access.
> However, it also means that I can experience deadlock and blocking performance.

Now, when writing code against an `MVar`, we have to be more careful: we can cause deadlock.
But we also have significantly more power over how the data is accessed and updated.
Particularly *fairness* and *coordination* are powerful advantages for the `MVar`.
However, transactions with multiple `MVar` are difficult to do correctly.

Where `IORef`s `atomicModifyIORef` requires a pure computation to avoid data races, you can `takeMVar` on an `MVar` to do an update - this allows you to do effects while computing your new value, and guarantees that consumers receive a consistent view of the value inside the `MVar`.

If I say `TVar a`, I mean:

> This is a reference to an `a`.
> The reference may be read or written to from any thread.
> I want transactionality around *the entire structure of `a`* - that is, if anyone touches any part of the structure of `a`, I want my whole transaction to be invalidated.

This last bit is actually a very strong claim!
Are you sure you want transactionality around *the entire `a`*?

For a `TVar Int`, the answer is *yes*.
But for a `TVar (Map k v)`, your transaction *probably* is only concerned with specific *parts* of the `Map`.
`stm-containers` uses a strategy similar to `TChan` and `TQueue` - rather than a `TVar` containing a recursive data structure, the actual recursive steps are themselves `TVar`s.
This allows modifications to the data structure to only invalidate transactions that are actually relevant.

Consider a SQL transaction.
If you have a query which selects a row from a table, you want to have some sort of locking to ensure that the row remains the same throughout the transaction.
Locking the *entire table* would be disastrous for performance - no one could do any work on the table until your transaction was complete!
Locking the entire row can also be quite bad for performance if the row is large.
But if you only select a handful of columns, ideally only *those* columns are locked by the query.

`STM` is a wonderful mechanism for concurrency, but it isn't foolproof.
We are still responsible for selecting the right data structures for good performance.
The core issue here is *coarse-grained transactional state*.
Finer transactionality gives us better performance.
