---
title: "TChan vs TQueue: What's the difference?"
date: 2018-10-24
layout: post
categories: programming
---

I always forget the difference between a `TChan` and a `TQueue`.
They appear to have an almost identical API, so whenever I need a concurrent message thing, I spend some time working out what the difference is.
I've done this a few times now, and it's about time that I write it out so that I don't need to keep reconstructing it.

Aside: Please don't use `TChan` or `TQueue`.
These types are *unbounded*, which means that you can run into unbounded memory use if your producer is faster than your consumer.
Instead, use `TBChan` or `TBQueue`, which allow you to set a bound.
I have run into issues with livelock with the standard `stm` and `stm-chans` types, and have found that [`unagi-chan` package](https://hackage.haskell.org/package/unagi-chan) has better performance in all cases, so I usually reach for that when I have a need for a high performance concurrent channel.
Unfortunately, the `unagi-chan` variants don't operate in `STM`, which can be a dealbreaker depending on your workflow.

tl;dr: Use a channel when you want all readers to receive each message. 
Use a queue when you want only one reader to receive each message.

The [docs for a `TChan`](http://hackage.haskell.org/package/stm-2.5.0.0/docs/Control-Concurrent-STM-TChan.html) are concise:

> TChan is an abstract type representing an unbounded FIFO channel.

The [docs for a `TQueue` are a bit more verbose]():

> A `TQueue` is like a `TChan`, with two important differences:
>
> * it has faster throughput than both `TChan` and `Chan` (although the costs are amortised, so the cost of individual operations can vary a lot).
> * it does not provide equivalents of the `dupTChan` and `cloneTChan` operations.
>
> The implementation is based on the traditional purely-functional queue representation that uses two lists to obtain amortised $O(1)$ enqueue and dequeue operations.

So the docs say that `TQueue` is faster, but has fewer operations.
Presumably, we should use a `TQueue` unless we need these operations.
What do `dupTChan` and `cloneTChan` do?
Let's look at [the Haddocks](http://hackage.haskell.org/package/stm-2.5.0.0/docs/Control-Concurrent-STM-TChan.html#v:dupTChan):

> ```haskell
> dupTChan :: TChan a -> STM (TChan a)
> ```
> Duplicate a TChan: the duplicate channel begins empty, but data written to either channel from then on will be available from both. Hence this creates a kind of broadcast channel, where data written by anyone is seen by everyone else.
>
> ```haskell
> cloneTChan :: TChan a -> STM (TChan a)
> ```
>
> Clone a TChan: similar to dupTChan, but the cloned channel starts with the same content available as the original channel.

So, what's the point of these?
Let's write some code and see what happens.

```haskell
test2 :: IO ()
test2 = do
    c <- newTChanIO
    forkIO $ do
        for_ [1..5] $ \n -> do
            i <- atomically $ readTChan c
            putStrLn $ "First thread received: " ++ show i ++ "on #: " ++ show n

    forkIO $ do
        for_ [5..10] $ \n -> do
            i <- atomically $ readTChan c
            putStrLn $ "Second thread received: " ++ show i ++ "on #: " ++ show n

    for_ [1..10] $ \i -> do
       threadDelay 10000
       atomically $ writeTChan c i
```

This creates a new `TChan`, then forks two threads.
Each thread sits and waits on the `TChan` to have a value, and then it prints the value out.
Finally we stuff the numbers 1 through 100 into the channel, with a slight delay.

This is the output:

```
Beginning test...
First thread received: 1on #: 1
First thread received: 2on #: 2
Second thread received: 3on #: 6
First thread received: 4on #: 3
Second thread received: 5on #: 7
Second thread received: 6on #: 8
First thread received: 7on #: 4
Second thread received: 8on #: 9
First thread received: 9on #: 5
Second thread received: 10on #: 10
```

Alright, so the two threads mostly just interleave their work.
The values 1-100 are printed out by each thread.
Let's try using `dupTChan` and see what happens:

```haskell
test3 :: IO ()
test3 = do
    c <- newTChanIO
    forkIO $ do
        for_ [1..5] $ \n -> do
            i <- atomically $ readTChan c
            putStrLn $ "First thread received: " ++ show i ++ " on #: " ++ show n

    forkIO $ do
        c' <- atomically $ dupTChan c
        for_ [6..10] $ \n -> do
            i <- atomically $ readTChan c'
            putStrLn $ "Second thread received: " ++ show i ++ " on #: " ++ show n

    for_ [1..10] $ \i -> do
       threadDelay 10000
       atomically $ writeTChan c i
```

This is basically the same code, but we've duplicated the `TChan` in the second thread.
Here's the new output:

```
Beginning test...
First thread received: 1 on #: 1
Second thread received: 1 on #: 6
Second thread received: 2 on #: 7
First thread received: 2 on #: 2
First thread received: 3 on #: 3
Second thread received: 3 on #: 8
First thread received: 4 on #: 4
Second thread received: 4 on #: 9
First thread received: 5 on #: 5
Second thread received: 5 on #: 10
```

Interesting! So the duplicated channel is able to receive a `writeTChan`, and both threads are able to see the values.
So, a `TChan` with a `dupTChan` call is suitable for when you want all copies of the `TChan` to receive a value.
A `TQueue` will only permit a value to be seen once, by a single thread.

There's a variant of `newTChan` called `newBroadcastTChan`.
How does it differ?
The docs explain:

> Create a write-only TChan.
> More precisely, readTChan will retry even after items have been written to the channel.
> The only way to read a broadcast channel is to duplicate it with dupTChan.
> 
> Consider a server that broadcasts messages to clients:
> 
> ```haskell
> serve :: TChan Message -> Client -> IO loop
> serve broadcastChan client = do
>     myChan <- dupTChan broadcastChan
>     forever $ do
>         message <- readTChan myChan
>         send client message
> ```
>
> The problem with using newTChan to create the broadcast channel is that if it is only written to and never read, items will pile up in memory.
> By using newBroadcastTChan to create the broadcast channel, items can be garbage collected after clients have seen them.

The last paragraph is the important part.
A standard `TChan` will accumulate messages until *that copy* of the `TChan` is read.
A *broadcast* `TChan` will not accumulate messages on the write end of the channel.
This points to an important performance concern:

- If you `dupTChan` a channel, you must read from *every* duplicate of the `TChan` in order to avoid memory loss.
- If you intend on having a write-only end, you must use `newBroadcastTChan` for any channel that you won't read from.

`TQueue` avoids this problem as it cannot be duplicated.
