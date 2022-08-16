---
title: "Dynamic Exception Reporting in Haskell"
date: 2022-08-16
layout: post
categories: programming
---

Exceptions kind of *suck* in Haskell.
You don't get a stack trace.
They don't show up in the types of functions.
They incorporate a subtyping mechanism that feels more like Java casting than typical Haskell programming.

A partial solution to the problem is `HasCallStack` - that gives us a `CallStack` which gets attached to `error` calls.
However, it *only* gets attached to `error` - so you can either have `String` error messages *and* a `CallStack`, or you can have richly typed exceptions with no location information.

A `CallStack` is a *static* piece of information about the code.
"You called `foo`, which called `bar`, which called `quuz`, which blew up with `Prelude.read: No parse`."
The `CallStack` answers a single question: "Where did this go wrong?"

But there's often *many* more interesting questions that simply "Where?"
You often want to know Who? When? How? in order to diagnose the big one: why did my code blow up?

In order to help answer these questions and develop robust exception reporting and diagnosing facilities, I created the [`annotated-exception`](https://hackage.haskell.org/package/annotated-exception) package.

# Better Call Stacks

`annotated-exception` provides a big improvement in static `CallStack` behavior.
To understand the improvement, let's dig into the core problem:

## Broken Chains and Orphan Stacks

If any function doesn't include a `HasCallStack` constraint in your stack, then the chain is broken, and you only get the stack closest to the source.

Consider this trivial example, which has a few ways of blowing up:

```haskell
import GHC.Stack

foo :: HasCallStack => Int
foo = error "foo"

bar :: HasCallStack => Int
bar = foo

baz :: Int
baz = foo

quux :: HasCallStack => Int
quux = bar

ohno :: HasCallStack => Int
ohno = baz
```

If we call `foo` in GHCi, we get the immediate stack trace:

```
λ> foo
*** Exception: foo
CallStack (from HasCallStack):
  error, called at <interactive>:4:7 in interactive:Ghci1
    foo, called at <interactive>:14:1 in interactive:Ghci2
```

Since the `bar` term has the `HasCallStack` constraint, it will add it's location to the mix:

```
λ> bar
*** Exception: foo
CallStack (from HasCallStack):
  error, called at <interactive>:4:7 in interactive:Ghci1
  foo, called at <interactive>:6:7 in interactive:Ghci1
  bar, called at <interactive>:15:1 in interactive:Ghci2
```

However, `baz` omits the constraint, which means that you won't get that function in the stack:

```
λ> baz
*** Exception: foo
CallStack (from HasCallStack):
  error, called at <interactive>:4:7 in interactive:Ghci1
    foo, called at <interactive>:8:7 in interactive:Ghci1
```

The `quux` term has the call stack, so you get the whole story again:

```
λ> quux
*** Exception: foo
CallStack (from HasCallStack):
  error, called at <interactive>:4:7 in interactive:Ghci1
    foo, called at <interactive>:6:7 in interactive:Ghci1
      bar, called at <interactive>:10:8 in interactive:Ghci1
        quux, called at <interactive>:17:1 in interactive:Ghci2
```

But here's the crappy thing - `ohno` *does* have a `HasCallStack` constraint.
You might expect that it would show up in the backtrace.
But it does not:

```
λ> ohno
*** Exception: foo
CallStack (from HasCallStack):
  error, called at <interactive>:4:7 in interactive:Ghci1
  foo, called at <interactive>:8:7 in interactive:Ghci1
```

The `CallStack` for `foo`, `baz`, and `ohno` are *indistinguishable*.
This makes diagnosing the failure difficult.

To avoid this problem, you must diligently place a `HasCallStack` constraint on *every function in your code base*.
This is pretty annoying! 
And if you have any library code that calls *your* code, the library's lack of `HasCallStack` will break your chains for you.

## `checkpoint` to the rescue

`annotated-exception` introduces the idea of a [`checkpoint`](https://hackage.haskell.org/package/annotated-exception-0.2.0.3/docs/src/Control.Exception.Annotated.html#checkpoint).
The simplest one is `checkpointCallStack`, which attaches the call-site to any exceptions thrown out of the action:

```haskell
checkpointCallStack
    :: (HasCallStack, MonadCatch m)
    => m a
    -> m a
```

Let's replicate the story from above.

```haskell
import Control.Exception.Annotated

foo :: IO Int
foo = throw (userError "foo")

-- in GHCi, evaluate:
-- λ> foo
*** Exception: 
    AnnotatedException 
         { annotations = 
             [ Annotation @CallStack 
                 [ ( "throw"
                   , SrcLoc 
                         { srcLocPackage = "interactive"
                         , srcLocModule = "Ghci1"
                         , srcLocFile = "<interactive>"
                         , srcLocStartLine = 4
                         , srcLocStartCol = 7
                         , srcLocEndLine = 4
                         , srcLocEndCol = 30
                         }
                   )
                 ]
             ]
         , exception = user error (foo)
         }
```

I've formatted the output to be a bit more legible.
Now, instead of a plain `IOError`, we've thrown an `AnnotatedException IOError`.
Inside of it, we have the `CallStack` from `throw`, which *knows where it was thrown from*.
That `CallStack` inside of the exception is reporting the *call-site* of `throw` - not the definition site!
This is true even though `foo` *does not have a `HasCallStack`* constraint!

Let's do `bar`.
We'll do `HasCallStack` *and* our `checkpointCallStack`, just to see what happens:

```haskell
import GHC.Stack

bar :: HasCallStack => IO Int
bar = checkpointCallStack foo


-- λ> bar
*** Exception: 
    AnnotatedException 
        { annotations = 
            [ Annotation @CallStack 
                [ ( "throw"
                  , SrcLoc { srcLocPackage = "interactive", srcLocModule = "Ghci1", srcLocFile = "<interactive>", srcLocStartLine = 4, srcLocStartCol = 7, srcLocEndLine = 4, srcLocEndCol = 30}
                  )
                , ( "checkpointCallStack"
                  , SrcLoc {srcLocPackage = "interactive", srcLocModule = "Ghci2", srcLocFile = "<interactive>", srcLocStartLine = 15, srcLocStartCol = 7, srcLocEndLi ne = 15, srcLocEndCol = 30}
                  )
                , ( "bar"
                  , SrcLoc {srcLocPackage = "interactive", srcLocModule = "Ghci3", srcLocFile = "<interactive>", srcLocStartLine = 17, srcLocStartCol = 1, srcLocEndLine = 17, srcLocEndCol = 4}
                  )
                ]
            ]
        , exception = user error (foo)
        }
```

We get the source location for `throw`, `checkpointCallStack`, and then the *use site* of `bar`.

Now, suppose we have our Problem Function again: `baz` doesn't have a `HasCallStack` constraint *or* a `checkpointCallStack`.
And when we called it through `ohno`, we lost the stack, even though `ohno` had the `HasCallStack` constraint.

```haskell
baz :: IO Int
baz = bar

ohno :: IO Int
ohno = checkpointCallStack baz

-- λ> ohno
*** Exception: 
    AnnotatedException 
        { annotations = 
            [ Annotation @CallStack 
                [ ( "throw"
                  , SrcLoc {srcLocPackage = "interactive", srcLocModule = "Ghci1", srcLocFile = "<interactive>", srcLocStartLine = 4, srcLocStartCol = 7, srcLocEndLine = 4, srcLocEndCol = 30}
                  )
                , ( "checkpointCallStack"
                  , SrcLoc {srcLocPackage = "interactive", srcLocModule = "Ghci2", srcLocFile = "<interactive>", srcLocStartLine = 15, srcLocStartCol = 7, srcLocEndLi ne = 15, srcLocEndCol = 30}
                  )
                , ( "bar"
                  , SrcLoc {srcLocPackage = "interactive", srcLocModule = "Ghci3", srcLocFile = "<interactive>", srcLocStartLine = 21, srcLocStartCol = 7, srcLocEndLine = 21, srcLocEndCol = 10}
                  )
                , ( "checkpointCallStack"
                  , SrcLoc {srcLocPackage = "interactive", srcLocModule = "Ghci3", srcLocFile = "<interactive>", srcLocStartLine = 23, srcLocStartCol = 8, srcLocEndLine = 23, srcLocEndCol = 31}
                  )
                ]
            ]
        , exception = user error (foo)
        }
```

When we call `ohno`, we preserve all of the entries in the `CallStack`. 
`checkpointCallStack` in `ohno` adds itself to the `CallStack` that is present on the `AnnotatedException` itself, so it doesn't need to worry about the stack being broken.
It's perfectly capable of recording that history for you.

## Ain't Just a Checkpoint - `catch` me later

The type signature for `catch` in `annotated-exception` looks like this:

```haskell
catch
    :: (HasCallStack, Exception e, MonadCatch m)
    => m a
    -> (e -> m a)
    -> m a
```

That `HasCallStack` constraint is used to give you a `CallStack` entry for any time that you `catch` an exception.

```haskell
newtype MyException = MyException String
    deriving Show

instance Exception MyException

boom :: IO Int
boom = throw (MyException "boom")

recovery :: IO Int
recovery =
    boom `catch` \(MyException message) -> do
        putStrLn message
        throw (MyException (message ++ " recovered"))
```

`recovery` catches the `MyException` from `boom`, prints the message, and then throws a *new* exception with a modified message.

```haskell
λ> recovery
boom
*** Exception: 
    AnnotatedException 
        { annotations = 
            [ Annotation @CallStack 
                [ ( "throw"
                  , SrcLoc {srcLocPackage = "main", srcLocModule = "Annotated", srcLocFile = "src/annotated.hs", srcLocStartLine = 19, srcLocStartCol = 9, srcLocEndLine = 19, srcLocEndCol = 54}
                  )
                , ( "throw"
                  , SrcLoc {srcLocPackage = "main", srcLocModule = "Annotated", srcLocFile = "src/annotated.hs", srcLocStartLine = 13, srcLocStartCol = 8, srcLocEndLine = 13, srcLocEndCol = 34}
                  )
                , ( "catch"
                  , SrcLoc {srcLocPackage = "main", srcLocModule = "Annotated", srcLocFile = "src/annotated.hs", srcLocStartLine = 17, srcLocStartCol = 5, srcLocEndLine = 19, srcLocEndCol = 54}
                  )
                ]
            ]
        , exception = MyException "boom recovered"
        }
```

Now, look at that call stack: we have the first `throw` (from `boom`), then we have the *second* `throw` (in `recovery`), and finally the `catch` in `recovery`.

So we know where the exception originally happened, where it was rethrown, and where it was caught.
This is fantastic!

But, even better - these annotations survive *even if you throw a different type of `Exception`*.
This means you can translate exceptions fearlessly, knowing that any essential annotated context won't be lost.

# Dynamic Annotations

As I said earlier, `CallStack` is fine, but it's a *static* thing.
We can figure out "what code called what other code" that eventually led to an exception, but we can't know anything about the running state of the program.

Enter `checkpoint`.
This function attaches an arbitrary `Annotation` to thrown exceptions.
An `Annotation` is a wrapper around any value that has an instance of `Show` and `Typeable`.
The library provides an instance of `IsString` for this, so you can enable `OverloadedStrings` and have stringly-typed annotations.

```haskell
constantAnnotation :: IO String
constantAnnotation =
    checkpoint "from constant annotation" $ do
        msg <- getLine
        if null msg
            then throw (MyException "empty message")
            else pure msg
```

But the real power is in using *runtime data* to annotate things.

Let's imagine you've got a web application.
You're reporting runtime exceptions to a service, like Bugnsag.
Specific teams "own" routes, so if something breaks, you want to alert the right team.

You can annotate thrown exceptions with the *route*.

```haskell
data Route 
    = Login
    | Signup
    | ViewPosts
    | CreatePost
    | EditPost PostId
    deriving Show

dispatch :: Request -> IO Response
dispatch req = 
    case parseRequest req of
        Right route ->
            checkpoint (Annotation route) $ 
                case route of
                    Login ->
                        handleLogin
                    Signup -> 
                        handleSignup
                    ViewPosts ->
                        handleViewPosts
                    CreatePost ->
                        handleCreatePost
                    EditPost postId ->
                        checkpoint (Annotation postId) $
                            handleEditPost postId
        Left _ ->
            invalidRouteError
```

Now, suppose an exception is thrown somewhere in `handleLogin`.
It's going to bubble up past `dispatch` and get handled by the Warp default exception handler.
That's going to dig into the `[Annotation]` and use that to alter the report we send to Bugsnag.
The team that is responsible for `handleLogin` gets a notification that something broke there.

In the `EditPost` case, we've *also* annotated the exception with *the post ID that we're trying to edit*.
This means that, when debugging, we can know *exactly* which post threw the given exception.
Now, when diagnosing and debugging, we can immediately pull up the problematic entry.
This gives us much more information about the problem, which makes diagnosis easier.

Likewise, suppose we have a function that gives us the logged in user:

```haskell
withLoggedInUser :: (Maybe (Entity User) -> IO a) -> IO a
withLoggedInUser action = do
    muser <- getLoggedInUser
    checkpoint (Annotation (fmap entityKey muser)) $ do
        action muser
```

If the action we pass in to `withLoggedInUser` throws an exception, that exception will carry the `Maybe UserId` of whoever was logged in.
Now, we can easily know *who* is having a problem on our service, in addition to what the problem actually is.

# The Value of Transparency

> But wait - if all exceptions are wrapped with this `AnnotatedException` type, then how do I catch things? Won't this pollute my codebase?
> 
> And, what happens if I try to catch an `AnnotatedException MyException` but some other code only threw a *plain* `MyException`? Won't that break things?

These are great questions.

`catch` and `try` from other libraries will fail to catch a `FooException` if the real type of the exception is `AnnotatedException FooException`.
However, `catch` and `try` from `annotated-exception` is capable of "seeing through" the `AnnotatedException` wrapper.

In fact, we took advantage of this earlier - here's the code for `recovery` again:

```haskell
boom :: IO Int
boom = throw (MyException "boom")

recovery :: IO Int
recovery =
    boom `catch` \(MyException message) -> do
        putStrLn message
        throw (MyException (message ++ " recovered"))
```

Note how `catch` doesn't say *anything* about annotations.
We catch a `MyException`, exactly like you would in `Control.Exception`, and the annotations are propagated.

But, let's say you want to catch the `AnnotatedException MyException`.
You just *do that*.

```haskell
recoveryAnnotated :: IO Int
recoveryAnnotated =
    boom `catch` \(AnnotatedException annotations (MyException message)) -> do
        putStrLn message
        traverse print annotations
        throw (OtherException (length message))

-- in GHCi,
λ> recoveryAnnotated
boom
Annotation @CallStack [("throw",SrcLoc {srcLocPackage = "main", srcLocModule = "Annotated", srcLocFile = "src/annotated.hs", srcLocStartLine = 13, srcLocStartCol = 8, srcLocEndLine = 13, srcLocEndCol = 34})]
*** Exception: 
    AnnotatedException 
        { annotations = 
            [ Annotation @CallStack 
                [ ( "throw"
                  , SrcLoc {srcLocPackage = "main", srcLocModule = "Annotated", srcLocFile = "src/annotated.hs", srcLocStartLine = 37, srcLocStartCol = 9, srcLocEndLine = 37, srcLocEndCol = 48}
                  )
                ]
            ]
        , exception = OtherException 4
        }
```

Now, something tricky occurs here: we don't preserve the annotations on the thrown exception.
If you catch an `AnnotatedException`, the library assumes that you're going to handle those yourself.

If you want to keep them, you'd need to throw an `AnnotatedException`:

```haskell
recoveryAnnotatedPreserve :: IO Int
recoveryAnnotatedPreserve =
    boom `catch` \(AnnotatedException annotations (MyException message)) -> do
        putStrLn message
        traverse print annotations
        throw (AnnotatedException annotations (OtherException (length message)))

-- in GHCi,
λ> recoveryAnnotatedPreserve 
boom
Annotation @CallStack [("throw",SrcLoc {srcLocPackage = "main", srcLocModule = "Annotated", srcLocFile = "src/annotated.hs", srcLocStartLine = 13, srcLocStartCol = 8, srcLocEndLine = 13, srcLocEndCol = 34})]
*** Exception: 
    AnnotatedException 
        { annotations = 
            [ Annotation @CallStack 
                [ ( "throw"
                  , SrcLoc {srcLocPackage = "main", srcLocModule = "Annotated", srcLocFile = "src/annotated.hs", srcLocStartLine = 44, srcLocStartCol = 9, srcLocEndLine = 44, srcLocEndCol = 81}
                  )
                ]
            , Annotation @CallStack 
                [ ( "throw"
                  , SrcLoc {srcLocPackage = "main", srcLocModule = "Annotated", srcLocFile = "src/annotated.hs", srcLocStartLine = 13, srcLocStartCol = 8, srcLocEndLine = 13, srcLocEndCol = 34}
                  )
                ]
            ]
        , exception = OtherException 4
        }
```

We're missing `catch`, which is unfortunate, but *generally* you aren't going to be doing this - you're either going to be handling an error completely, or rethrowing it, and the `[Annotation]` won't be relevant to you... unless you're writing an integration with Bugsnag, or reporting on them in some other way.

So `annotated-exception`'s exception handling functions can "see through" an `AnnotatedException inner` to work only on the `inner` exception type.
But what if I try to catch a `DatabaseException` as an `AnnotatedException DatabaseException`?

Turns out, the `Exception` instance of `AnnotatedException` allows you to do that.

```haskell
import qualified Control.Exception

emptyAnnotationsAreCool :: IO ()
emptyAnnotationsAreCool =
    Control.Exception.throwIO (MyException "definitely not annotated?")
        `Control.Exception.catch`
            \(AnnotatedException annotations (MyException woah)) -> do
                print annotations
                putStrLn woah


-- in GHCi,
λ> emptyAnnotationsAreCool 
[]
definitely not annotated?
```

We promote the `inner` into `AnnotatedException [] inner`.
So the library works regardless if any code you throw cares about `AnnotatedException`.
If you call some external library code which throws an exception, you'll get the first annotation you try - including if that's just from `catch`:

```haskell
catchPutsACallStack :: IO ()
catchPutsACallStack =
    Control.Exception.throwIO (MyException "definitely not annotated?")
        `catch`
            \(MyException woah) -> do
                throw (OtherException (length woah))

-- in GHCi,
λ> catchPutsACallStack 
*** Exception: 
    AnnotatedException 
        { annotations = 
            [ Annotation @CallStack 
                [ ( "throw"
                  , SrcLoc {srcLocPackage = "main", srcLocModule = "Annotated", srcLocFile = "../parsonsmatt.github.io/src/annotated.hs", srcLocStartLine = 60, srcLocStartCol = 17, srcLocEndLine = 60, srcLocEndCol = 53})
                , ("catch"
                  , SrcLoc {srcLocPackage = "main", srcLocModule = "Annotated", srcLocFile = "../parsonsmatt.github.io/src/annotated.hs", srcLocStartLine = 58, srcLocStartCol = 9, srcLocEndLine = 58, srcLocEndCol = 16}
                  )
                ]
            ]
        , exception = OtherException 25
        }
```

We get `throw` and `catch` both showing up in our stack trace.
If we'd used `Control.Exception.throwIO` instead of `Control.Exception.Annotated.throw`, then we'd *still* have `catch` as an annotation.

# Do you feel the power?

The primary purpose here is to share the technique and inspire a hunger for dynamic exception annotations.

We've been using this technique at Mercury for most of this year.
It has *dramatically* simplified how we report exceptions, the shape of our exceptions, and how much info we get from a Bugsnag report.
It's now much easier to diagnose problems and fix bugs.

The Really Big Deal here is that - we now have something *better* than other languages.
The lack of stack traces in Haskell is really annoying, and a clear way that Haskell suffers compared to Ruby or Java.
But now, with `annotated-exception`, we actually have *more powerful* and *more useful* exception annotations than a mere stack trace.
And, since this is all just library functions, you can swap to `Control.Exception.Annotated` with little fuss.
