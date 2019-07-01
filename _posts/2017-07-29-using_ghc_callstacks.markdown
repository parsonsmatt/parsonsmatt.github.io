---
title: "Using GHC CallStacks"
date: 2017-07-29
layout: post
categories: programming
---

(Note: this blog post has code accessible on [this GitHub repository](https://github.com/parsonsmatt/callstacks-what-even). You can follow along there if you'd like.)

Haskell doesn't really have a callstack.
The evaluation strategy is more like a graph reduction.
If you don't understand that, that's okay -- I don't either!
All I know about it is that it makes questions like "what's the stack trace for this error?" surprisingly difficult to answer.

While Haskell's debugging story tends to be rather nice (break up code into small, composable, reusable functions; take advantage of types to make errors unrepresentable where practical; write unit and property tests for the rest), it's also great to know where errors actually come from.
Coding practices like "don't ever use partial functions like `head :: [a] -> a`" and "prefer `NonEmpty a` to `[a]` where possible" help a lot.
However, you may find yourself stuck staring at

```
recv: resource vanished
```

or similar, and that frankly sucks.

## `GHC.CallStack`

GHC has a callstack simulation mechanism.
The interface is a nullary type class, and you can include a callstack with your program by adding it:

```haskell
headNoCallStack :: [a] -> a
headNoCallStack (x:xs) = x
headNoCallStack [] = error "nope"

headWithCallstack :: HasCallStack => [a] -> a
headWithCallstack (x:xs) = x
headWithCallstack [] = error "nope"
```

Let's compare the behavior of these various functions.
The ordinary `head` from the Prelude gives us this:

```haskell
λ> head []
*** Exception: Prelude.head: empty list
```

Well, that's useless. No information about where it was even called!
Our own `headNoCallStack` gives slightly better results:

```haskell
λ> headNoCallStack []
*** Exception: nope
CallStack (from HasCallStack):
  error, called at src/Lib.hs:7:22 in main:Lib
```

We get a callstack! `error` was modified recently to carry a `CallStack` parameter, though that information is a little hidden:

```haskell
λ> :t error
error :: [Char] -> a
λ> :i error
error ::
  forall (r :: ghc-prim-0.5.0.0:GHC.Types.RuntimeRep) (a :: TYPE r).
  HasCallStack =>
  [Char] -> a
        -- Defined in ‘GHC.Err’
```

The `:info` output shows that `error` is polymorphic in the runtime representation (eg: the phantom type `a` can be an unlifted type like `Int#` or a lifted type like `Int`).
The `:type` omits the `HasCallStack` constraint for some reason.

When `headWithCallstack` throws that error, you'll get more extra information:

```
λ> headWithCallStack []
*** Exception: nope
CallStack (from HasCallStack):
  error, called at src/Lib.hs:11:24 in main:Lib
  headWithCallStack, called at <interactive>:6:1 in interactive:Ghci1
```

This constructs a `CallStack` from `headWithCallStack` down to the `error` call.
Nice!

# A Shallow Stack

How does this interact with more complex programs?
Let's write something with a bit of nesting:

```haskell
maximumCS :: (HasCallStack, Ord a) => [a] -> a
maximumCS = foldr1CS max

foldr1CS :: HasCallStack => (a -> a -> a) -> [a] -> a
foldr1CS _ [x] = x
foldr1CS k (x:xs) = k x (foldr1CS k xs)
foldr1CS _ [] = error "foldr1 empty list"

someProgram :: HasCallStack => [[Int]] -> Int
someProgram = headWithCallStack . maximumCS
```

Nothing terribly complicated, but we're propagating that callstack all the way down.
Let's see what happens when it blows up:

```
λ> someProgram []
*** Exception: foldr1 empty list
CallStack (from HasCallStack):
  error, called at src/Lib.hs:19:17 in main:Lib
  foldr1CS, called at src/Lib.hs:14:13 in main:Lib
  maximumCS, called at src/Lib.hs:22:35 in main:Lib
  someProgram, called at <interactive>:36:1 in interactive:Ghci1
λ> someProgram [[]]
*** Exception: nope
CallStack (from HasCallStack):
  error, called at src/Lib.hs:11:24 in main:Lib
  headWithCallStack, called at src/Lib.hs:22:15 in main:Lib
  someProgram, called at <interactive>:37:1 in interactive:Ghci1
```

Nice! We get a complete stack trace of everything that went wrong.
When we pass it the empty list, then we can see that `error` was called by `foldr1CS`, which was called by `maximumCS`, and finally `someProgram` was the main offender.
When given `[[]]`, we can see that `headWithCallstack` is the one that threw the exception. Nice!

# Omitting the CallStack

Let's see how this works if we omit something at some point.

```haskell
foo :: HasCallStack => Maybe a -> a
foo (Just a) = a
foo Nothing = error "foo is unpleased"

bar :: Maybe a -> a
bar = foo

baz :: HasCallStack => Maybe a -> a
baz = bar
```

These are all just `fromJust` in disguise.
`baz` delegates to `bar` and `bar` delegates to `foo`.
Let's observe the stack traces we get when we call `baz Nothing`!

```
λ> foo Nothing
*** Exception: foo is unpleased
CallStack (from HasCallStack):
  error, called at src/Lib.hs:28:15 in main:Lib
  foo, called at <interactive>:44:1 in interactive:Ghci1
λ> bar Nothing
*** Exception: foo is unpleased
CallStack (from HasCallStack):
  error, called at src/Lib.hs:28:15 in main:Lib
  foo, called at src/Lib.hs:31:7 in main:Lib
λ> baz Nothing
*** Exception: foo is unpleased
CallStack (from HasCallStack):
  error, called at src/Lib.hs:28:15 in main:Lib
  foo, called at src/Lib.hs:31:7 in main:Lib
```

Our callstack appears to be cut off!
We only get to see what happens with `foo` local stack.
Since `bar` does not have the `HasCallStack` constraint, it doesn't propagate any more information when the error is bubbled up.

If any function in the chain does *not* have `HasCallStack` in the signature, then *nothing* above that will be represented in the stack trace.
This is a pretty big limitation.

# Should I include a HasCallStack constraint?

Great question!
`HasCallStack` is implemented as [`type HasCallStack = ?callStack :: CallStack`](https://hackage.haskell.org/package/base-4.10.0.0/docs/GHC-Stack.html#t:HasCallStack) where the `?` means [an implicit parameter](https://wiki.haskell.org/Implicit_parameters) in current versions of GHC.
This is an extra parameter that gets passed around and handled in your program, which will affect performance.
Implicit parameters can potentially interact with sharing in weird ways, which might also cause strange performance issues.

`HasCallStack` is not pervasive in many libraries, so you're unlikely to actually have a `CallStack` present in the functions you pass to library or framework code.
This makes them less useful.

Lastly, the GHC Exceptions machinery doesn't have any notion of a callstack, and any *proper* exceptions that you throw or catch will not have a callstack: only `error` calls.
