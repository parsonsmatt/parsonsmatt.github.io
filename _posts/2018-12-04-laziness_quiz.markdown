---
title: "Laziness Quiz"
date: 2018-12-04
layout: post
categories: programming
---

Do you understand laziness?

It's okay if you don't. Most people don't. It can be somewhat surprising when something actually gets evaluated in Haskell, even when you're using bang patterns.
So, here is a quick quiz on laziness in Haskell!
If it makes you feel better, I didn't get it right either on my first try.
You can copy and paste this into a source file and run it in GHCi.

```haskell
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Quiz
    ( quiz
    ) where

import Debug.Trace
import System.IO.Unsafe
import Data.IORef

-- | A datatype with a "strict" field and a lazy field. We use
-- a "BangPattern" to annotate that `strict` should be "strict." Why the
-- quotes? Well, because it's not really as strict as you might expect!
data Foo = Foo { strict :: !Int, lazy :: Int }

-- | A global variable. Yes, Haskell has global variables, you just have to
-- be careful with them.
lineNumber :: IORef Int
lineNumber = unsafePerformIO (newIORef 0)
{-# NOINLINE lineNumber #-}

-- |  This action is used to keep track of where we are in the quiz.
logLine :: IO ()
logLine = do
    i <- atomicModifyIORef' lineNumber (\i -> (i + 1, i + 1))
    putStrLn $ "Log line: " <> show i

-- | The 'trace' functions from Debug.Trace will print a message to the
-- console whenever the expression is evaluated. It will only print it
-- once, and after that, the value will be cached.
quiz :: IO ()
quiz = do
    logLine
    let a = trace "evaluating a" $ 1 + 2
    logLine
    let !b = trace "evaluating b" $ 2 + 4
    logLine
    let foo = Foo
            { strict = trace "evaluating strict" $ 1 + 2 + 3 + 4
            , lazy = trace "evaluating lazy" $ 9 + 12
            }
    logLine
    print a
    logLine
    print a
    logLine
    case foo of
        Foo { strict, lazy } -> do
            logLine
            print lazy

```

Okay, make sure you can open this up in GHCi.
The quiz is: When will the various `trace` calls print things out?
We have four traces: the one for `a`, the one for `b`, the one for `strict` field, and the one for `lazy` field.

Read the program and guess where each of these traces will output.
Then, run the program.
How does your expectation differ from what actually happened?
What mental model were you using, and how did it differ from reality?

# Spoiling...

seriously don't look at this section until you've tried it yourself

i'm gonna put more filler here

M O A R

F I L L E R

# fill it up

okay, well, if you've made it this far, here's how I was wrong:

I expected that `"evaluating strict"` would print out with the `let foo = ...` line.
My understanding of bang patterns on data fields was that the value was evaluated to WHNF, and then the record was constructed with it.
BUT -- I also thought that declaring a data structure with a strict field would make the constructor strict as well.
It isn't!

In fact, `strict`'s evaluation doesn't print out until later -- can you guess when?

...

It's when we evaluate the `Foo` constructor to weak head normal form.
Indeed -- a strict field does not evaluate when the record is *constructed*, but rather, when the record is *evaluated*.

Anyway, this may be useful to you. Enjoy!

EDIT: The [reddit thread](https://old.reddit.com/r/haskell/comments/a339r4/laziness_quiz_can_you_get_it_right_i_didnt/) is quite good!
