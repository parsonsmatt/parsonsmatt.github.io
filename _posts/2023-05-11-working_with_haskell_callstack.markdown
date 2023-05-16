---
title: "Working with Haskell CallStack"
date: 2023-05-11
layout: post
categories: programming
---

GHC Haskell provides a type [`CallStack`](https://www.stackage.org/haddock/lts-20.20/base-4.16.4.0/GHC-Exception.html#t:CallStack) with some magic built in properties.
Notably, there's a constraint you can write - `HasCallStack` - that GHC will automagically figure out for you.
Whenever you put that constraint on a top-level function, it will figure out the line and column, and either create a fresh `CallStack` for you, or it will append the source location to the pre-existing `CallStack` in scope.

# Getting the current `CallStack` 

To grab the current `CallStack`, you'll write `callStack` - a value-level term that summons a `CallStack` from GHC's magic.

```haskell
import GHC.Stack

emptyCallStack :: IO ()
emptyCallStack = putStrLn $ show callStack
```

If we evaluate this in a compiled executable, then GHC will print out `[]` - a `CallStack` list with no entries!
This isn't much use. Let's add a `HasCallStack` constraint.

```haskell
giveCallStack :: HasCallStack => IO ()
giveCallStack = putStrLn $ show callStack
```

Running this in our test binary gives us the following entry, lightly formatted:

```haskell
[ ( "giveCallStack"
  , SrcLoc 
    { srcLocPackage = "main"
    , srcLocModule = "Main"
    , srcLocFile = "test/Spec.hs"
    , srcLocStartLine = 18
    , srcLocStartCol = 9
    , srcLocEndLine = 18
    , srcLocEndCol = 22
    }
  )
]
```

We get a `[(String, SrcLoc)]`.
The `String` represnts the function that was called, and where `SrcLoc` tells us the package, module, file, and a begin and end to the source location of the call site - not the definition site.

Let's construct a helper that gets the current `SrcLoc`.

```haskell
getSrcLoc :: HasCallStack => SrcLoc
getSrcLoc = snd $ head $ getCallStack callStack
```

I'm going to call `print getSrcLoc` in my test binary, and this is the output (again, formatted for legibility):

```haskell
SrcLoc 
    { srcLocPackage = "main"
    , srcLocModule = "Main"
    , srcLocFile = "test/Spec.hs"
    , srcLocStartLine = 27
    , srcLocStartCol = 15
    , srcLocEndLine = 27
    , srcLocEndCol = 24
    }
```

We can use this to construct a link to a GitHub project - suppose that we called that inside the [`esqueleto` repository](https://github.com/bitemyapp/esqueleto), and we want to create a link that'll go to that line of code.
Normally, you'd want to shell out and grab the commit and branch information, but let's just bake that into the link for now.

```haskell
mkGithubLink :: HasCallStack => String
mkGithubLink =
    concat
        [ "https://www.github.com/bitemyapp/esqueleto/blob/master/"
        , srcLocFile srcLoc
        , "#L", show $ srcLocStartLine srcLoc
        , "-"
        , "L", show $ srcLocEndLine srcLoc
        ]
  where
    srcLoc = getSrcLoc
```

Let's call that from our test binary now:

```haskell
main = do
    -- snip...
    example "mkGithubLink" do
        putStrLn mkGithubLink
```

The output is given:

```
mkGithubLink
https://www.github.com/bitemyapp/esqueleto/blob/master/src/Lib.hs#L24-L24
```

But - that's not right! That's giving us the source location for `getSrcLoc` inside of `mkGithubLink`.
We want it to give us the location of the callsite of `mkGithubLink`.

Fortunately, we can freeze the current `CallStack`, which will prevent `getSrcLoc` from adding to the existing `CallStack`.

# Freezing the `CallStack`

`GHC.Stack` provides a function [`withFrozenCallStack`](https://www.stackage.org/haddock/lts-20.20/base-4.16.4.0/GHC-Stack.html#v:withFrozenCallStack), with a bit of a strange type signature:

```haskell
withFrozenCallStack :: HasCallStack => (HasCallStack => a) -> a
```

This function freezes the `CallStack` for the *argument* of the function.
This is useful if you want to provide a wrapper around a function that manipulates or reports on the `CallStack` in some way, but you don't want that polluting any other `CallStack`.

Let's call that before `getSrcLoc` and see what happens.

```haskell
mkGithubLinkFrozen :: HasCallStack => String
mkGithubLinkFrozen =
    concat
        [ "https://www.github.com/bitemyapp/esqueleto/blob/master/"
        , srcLocFile srcLoc
        , "#L", show $ srcLocStartLine srcLoc
        , "-"
        , "L", show $ srcLocEndLine srcLoc
        ]
  where
    srcLoc = withFrozenCallStack getSrcLoc

-- in test binary,
main = do
    -- snip
    example "mkGithubLinkFrozen" do putStrLn mkGithubLinkFrozen
```

Output:

```
mkGithubLinkFrozen
https://www.github.com/bitemyapp/esqueleto/blob/master/test/Spec.hs#L32-L32
```

Bingo!

## More real-world examples

As an example, the library [`annotated-exception`](https://hackage.haskell.org/package/annotated-exception) attaches `CallStack`s to thrown exceptions, and each function like `catch` or `onException` that touches exceptions will append the current source location to the existing `CallStack`.

However, `handle` is implemented in terms of `catch`, which is implemented in terms of `catches`, and we wouldn't want every single call-site of `handle` to mention `catch` and `catches`, and we wouldn't want every call site of `catch` to mention `catches` - that's just noise.
So, we can freeze the `CallStack`:

```haskell
handle 
    :: (HasCallStack, Exception e, MonadCatch m) 
    => (e -> m a) -> m a -> m a
handle handler action = 
    withFrozenCallStack catch action handler

catch 
    :: (HasCallStack, Exception e, MonadCatch m) 
    => m a -> (e -> m a) -> m a
catch action handler = 
    withFrozenCallStack catches action [Handler handler]

catches 
    :: (HasCallStack, MonadCatch m) 
    => m a -> [Handler m a] -> m a
catches action handlers = 
    Safe.catches action (withFrozenCallStack mkAnnotatedHandlers handlers)

mkAnnotatedHandlers :: (HasCallStack, MonadCatch m) => [Handler m a] -> [Handler m a]
mkAnnotatedHandlers xs =
    xs >>= \(Handler hndlr) ->
        [ Handler $ \e ->
            checkpointCallStack $ hndlr e
        , Handler $ \(AnnotatedException anns e) ->
            checkpointMany anns $ hndlr e
        ]
```

Now, there's something interesting going on here: consider these two possible definition of `handle`:

```haskell
handle handler action = 
    withFrozenCallStack catch action handler
handle handler action = 
    withFrozenCallStack $ catch action handler
```

It's a Haskell *instinct* to write `function $ argument`, and it seems a bit odd to see `withFrozenCallStack` - a function - applied without a dollar.
This is a subtle distinction - `withFrozenCallStack` applied to `catch` alone just freezes the `CallStack` for `catch`, but not for `handler` or `action`.
If we apply `withFrozenCallStack $ catch action handler`, then we'll freeze the `CallStack` for our *arguments*, too.
This is *usually* not what you want.

### Freezing Functions

Let's explore the above subtle distinction in more depth.

```haskell
wat :: HasCallStack => IO ()
wat = do
    wrap "unfrozen" (printSrcLoc getSrcLoc)
    withFrozenCallStack $ wrap "dolla" (printSrcLoc getSrcLoc)
    withFrozenCallStack wrap "undolla" (printSrcLoc getSrcLoc)

printSrcLoc :: SrcLoc -> IO ()
printSrcLoc = putStrLn . prettySrcLoc

wrap :: HasCallStack => String -> IO a -> IO a
wrap message action = do
    putStrLn $ concat
        [ "Beginning ", message
        , ", called at ", prettySrcLoc getSrcLoc
        ]
    a <- action
    putStrLn $ "Ending " <> message
    pure a
```

Before seeing the answer and discussion below, consider and predict what `SrcLoc` you expect to see printed out when `wat` is called.

Let's zoom in on that:

```haskell
    wrap "unfrozen" (printSrcLoc getSrcLoc)
    withFrozenCallStack $ wrap (print getSrcLoc)
    withFrozenCallStack wrap (print getSrcLoc)
```

Both lines type check just fine.
The difference is in *which* `CallStack`s are frozen.
The first line freezes the `CallStack` for the entire expression, `wrap (print getSrcLoc)`.
The second line only freezes the `CallStack` for the `wrap` function - the `CallStack` for the `(print getSrcLoc)` is unfrozen.

Let's see what happens when we run that:

```
wat
Beginning unfrozen, called at src/Lib.hs:51:40 in callstack-examples-0.1.0.0-9VnJFsvI3QO7TuvXNKcHBF:Lib
src/Lib.hs:40:34 in callstack-examples-0.1.0.0-9VnJFsvI3QO7TuvXNKcHBF:Lib
Ending unfrozen
Beginning dolla, called at test/Spec.hs:34:19 in main:Main
test/Spec.hs:34:19 in main:Main
Ending dolla
Beginning undolla, called at test/Spec.hs:34:19 in main:Main
src/Lib.hs:42:53 in callstack-examples-0.1.0.0-9VnJFsvI3QO7TuvXNKcHBF:Lib
Ending undolla
```

For `unfrozen`, `wrap` calls the `SrcLoc` that corresponds to it's `putStrLn $ concat [..., getSrcLoc]` call.
This always points to the `wrap` definition site - we'd want to freeze that `getSrcLoc` if we wanted the *call* site of `wrap` in that case.
The next line (`src/Lib.hs:40:34 ...`) is our `printSrcLoc getSrcLoc` function provided to `wrap`.
That `SrcLoc` points to the call site of `getSrcLoc` in the file for that function.

For `dolla`, we've frozen the `CallStack` for both `wrap` and the function argument.
That means the `SrcLoc` we get for *both* cases is the same - so we're not really returning the exact `SrcLoc`, but the most recent `SrcLoc` before the entire `CallStack` was frozen.
This `SrcLoc` corresponds to the call-site of `wat` in the test suite binary, not the library code that defined it.

For `undolla`, we've only frozen the `CallStack` for `wrap`, and we leave it untouched for `printSrcLoc getSrcLoc`.
The result is that `wrap` prints out the frozen `CallStack` pointing to the callsite of `wat` in the test binary, while the function argument `printSrcLoc getSrcLoc` is able to access the `CallStack` with new frames added.

It's easiest to see what's going on here with *explicit function parenthesization*.
Haskell uses whitespace for function application, which makes the parentheses implicit for multiple argument functions.
Let's write the above expressions with explicit parens around `withFrozenCallStack`:

```haskell
    (withFrozenCallStack (wrap "dolla" (printSrcLoc getSrcLoc)))
    (withFrozenCallStack wrap) "undolla" (printSrcLoc getSrcLoc)
```

I almost wish that `withFrozenCallStack` *always* required parentheses, just to make this clearer - but that's not possible to enforce.

## Freezing Broke `mkGithubLink`

Unfortunately, yeah, `mkGithubLinkFrozen` is broken if we've frozen the `CallStack` externally:

```haskell
-- test
main :: HasCallStack => IO ()         
main = do                             -- === line 16
    -- snip...

    example "frozen githublink" do
        putStrLn (withFrozenCallStack mkGithubLinkFrozen)
                                      -- ^^^ line 37
```

This outputs:

```
frozen githublink
https://www.github.com/bitemyapp/esqueleto/blob/master/test/Spec.hs#L16-L16
```

Line 16 points to `main`, where we've included our `HasCallStack` constraint.
What if we omit that constraint?

```haskell
main :: IO ()         
main = do                             -- === line 16
    -- snip...

    example "frozen githublink" do
        putStrLn (withFrozenCallStack mkGithubLinkFrozen)
                                      -- ^^^ line 37
```

This outputs:

```
frozen githublink
callstack-examples-test: Prelude.head: empty list
```

Uh oh!

Well, `GHC.Stack` doesn't provide a utility for us to *unfreeze* the `CallStack`, which makes sense - that would break whatever guarantee that `withFrozenCallStack` is providing.

If we look at the [internal definitions](https://www.stackage.org/haddock/lts-20.20/base-4.16.4.0/src/GHC.Stack.Types.html#CallStack) for `CallStack`, we'll see that it's a list-like type:

```haskell
data CallStack
  = EmptyCallStack
  | PushCallStack [Char] SrcLoc CallStack
  | FreezeCallStack CallStack
```

Then we can see [`withFrozenCallStack`'s implementation](https://www.stackage.org/haddock/lts-20.20/base-4.16.4.0/src/GHC.Stack.html#withFrozenCallStack):

```haskell
withFrozenCallStack :: HasCallStack
                    => ( HasCallStack => a )
                    -> a
withFrozenCallStack do_this =
  -- we pop the stack before freezing it to remove
  -- withFrozenCallStack's call-site
  let ?callStack = freezeCallStack (popCallStack callStack)
  in do_this
```

That `?callStack` syntax is GHC's `ImplicitParams` extension - it's an implementation detail that GHC may change at any point in the future.
Let's rely on that detail!
It has remained true for 10 major versions of `base`, and we can always try and upstream this officially...

```haskell
import GHC.Stack.Types

thawCallStack :: CallStack -> CallStack
thawCallStack stack =
    case stack of
        FreezeCallStack stk -> stk
        _ -> stack

withThawedCallStack :: HasCallStack => (HasCallStack => r) -> r
withThawedCallStack action =
    let ?callStack = thawCallStack (popCallStack callStack)
     in action
```

Unfortunately, we can't call this *within*  `mkGithubLink` - that unfreezes the `CallStack`, but at that point, it's too late.

Yet another "safe" use of `head` that turns out to be unsafe!
Only in Haskell might we have a totally empty stack trace...

# Propagating `CallStack`

When you write a top-level function, you can include a `CallStack`.
Any time you call `error`, the existing `CallStack` will be appended to the [`ErrorCall`](https://www.stackage.org/haddock/lts-20.20/base-4.16.4.0/Control-Exception.html#t:ErrorCall) thrown exception, which you can see by matching on `ErrorCallWithLocation` instead of plain `ErrorCall`.

`CallStack` propagation is fragile.
Any function which *does not* include a `HasCallStack` constraint will break the chain, and you'll only have the lowest level of the `CallStack`.
Consider `boom` and `boomStack`:

```haskell
boom :: Int
boom = error "oh no"

boomStack :: HasCallStack => Int
boomStack = error "oh no, but with a trace"
```

If we evaluate these, then we'll see very different information.
`error` will summon it's own `CallStack`, which will include the callsite of `error` itself:

```
callstack-examples-test: oh no
CallStack (from HasCallStack):
  error, called at src/Lib.hs:76:8 in callstack-examples-0.1.0.0-9VnJFsvI3Q
O7TuvXNKcHBF:Lib
```

Line 76 and column 8 point exactly to where `error` is called in the definition of `boom`.
Let's evaluate `boomStack` now:

```haskell
callstack-examples-test: oh no, but with a trace
CallStack (from HasCallStack):
  error, called at src/Lib.hs:79:13 in callstack-examples-0.1.0.0-9VnJFsvI3QO7TuvXNKcHBF:Lib
  boomStack, called at test/Spec.hs:40:15 in main:Main
  main, called at test/Spec.hs:16:1 in main:Main
```

Now, we see the entry for `error`'s call-site, as well as `boomStack`'s call site, and finally `main` - the entire chain!

Remembering to put `HasCallStack` constraints *everywhere* is a bit of a drag, which is another motivation for my [`annotated-exception`](https://hackage.haskell.org/package/annotated-exception-0.2.0.4/docs/Control-Exception-Annotated.html) library - all of the functions which touch exceptions in any way will push a stack frame onto any exception that has been thrown.
This means that any `catch` or `finally` or similar will do a much better job of keeping track of the stack frame.
Diagnosing problems becomes far easier.

We can do this for `ErrorCall`, but it's annoying, because the location is a `String`.

```haskell
mkStackFrameLines :: CallStack -> [String]
mkStackFrameLines =
    map formatFrame . getCallStack
  where
    formatFrame (fn, srcLoc) =
        fn <> ", called at " <> prettySrcLoc srcLoc

addStackFrame :: HasCallStack => IO a -> IO a
addStackFrame action = do
    let newLines =
            map ("  " <>) $ mkStackFrameLines callStack
        appendLoc locs =
            unlines
                (locs : newLines)
    action `catch` \(ErrorCallWithLocation err loc) ->
        throwIO $ ErrorCallWithLocation err (appendLoc loc)

-- These functions are used here .
-- Try and predict what their output will be!

moreContextPlease :: IO ()
moreContextPlease =
    addStackFrame $ do
        print boom

moreContextPleaseStacked :: HasCallStack => IO ()
moreContextPleaseStacked =
    addStackFrame $ do
        print boom
```

When we evaluate `moreContextPlease`, we'll see this:

```
callstack-examples-test: oh no
CallStack (from HasCallStack):
  error, called at src/Lib.hs:77:8 in callstack-examples-0.1.0.0
  addStackFrame, called at src/Lib.hs:84:5 in callstack-examples-0.1.0.0
```

This gives us a little more context - we at least have that `addStackFrame` call.
But `addStackFrame` happily adds everything in the trace, and `moreContextPleaseStacked` has an unbroken line:

```
callstack-examples-test: oh no
CallStack (from HasCallStack):
  error, called at src/Lib.hs:77:8 in callstack-examples-0.1.0.0-9VnJFsvI3Q
O7TuvXNKcHBF:Lib
  addStackFrame, called at src/Lib.hs:89:5 in callstack-examples-0.1.0.0-9V
nJFsvI3QO7TuvXNKcHBF:Lib
  moreContextPleaseStacked, called at test/Spec.hs:40:9 in main:Main
  main, called at test/Spec.hs:16:1 in main:Main
```

Wow! A complete stack trace, all the way from `error` to `main`. 
You never see that.

Unfortunately, the `String` makes deduplicating lines more challenging.
`boomStack` included the `HasCallStack`, which would be an unbroken chain too - let's see how that plays out...

```haskell
moreContextPleaseStacked :: HasCallStack => IO ()
moreContextPleaseStacked =
    addStackFrame $ do
        print boomStack
```

Evaluating this now gives us:

```
callstack-examples-test: oh no, but with a trace
CallStack (from HasCallStack):
  error, called at src/Lib.hs:80:13 in callstack-examples-0.1.0.0-9VnJFsvI3
QO7TuvXNKcHBF:Lib
  boomStack, called at src/Lib.hs:90:15 in callstack-examples-0.1.0.0-9VnJF
svI3QO7TuvXNKcHBF:Lib
  moreContextPleaseStacked, called at test/Spec.hs:40:9 in main:Main
  main, called at test/Spec.hs:16:1 in main:Main
  addStackFrame, called at src/Lib.hs:89:5 in callstack-examples-0.1.0.0-9V
nJFsvI3QO7TuvXNKcHBF:Lib
  moreContextPleaseStacked, called at test/Spec.hs:40:9 in main:Main
  main, called at test/Spec.hs:16:1 in main:Main
```

We get `error`, `boomStack`, `moreContextPleaseStacked`, `main` - the original stack trace.
Then we append to that `addStackFrame`, which also adds in `moreContextPleaseStacked` and `main` again.
So, clearly, this is noisier than it needs to be - ideally, we would not include duplicates.
This should be possible - `addStackFrame` could potentially parse the location `String` and if it finds a shared suffix (in this case, `moreContextPleaseStacked`), then it can only insert the `addStackFrame` call above it and drop the rest.

# `annotated-exception`

I've mentioned [`annotated-exception`](https://hackage.haskell.org/package/annotated-exception-0.2.0.4/docs/Control-Exception-Annotated.html) a few times.
This library extends the `CallStack` machinery to any exception that is thrown by the library or passes through an exception handler.
Additionally, you can provide additional metadata information on your exceptions, which makes debugging them much more useful.
You can now transparently add, say, the logged in user ID to every single exception that gets thrown in a code block.

The source code for this blog post is available [at this GitHub repository](https://github.com/parsonsmatt/callstack-examples).
