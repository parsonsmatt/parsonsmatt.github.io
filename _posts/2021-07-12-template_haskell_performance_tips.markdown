---
title: "Template Haskell Performance Tips"
date: 2021-07-12
layout: post
categories: programming
---

`TemplateHaskell` is a powerful feature.
With it, you can generate Haskell code using Haskell code, and GHC will compile it for you.
This allows you to do many neat things, like [quoted type safe literals](https://hackage.haskell.org/package/qq-literals), [database entity definitions](https://hackage.haskell.org/package/persistent-2.13.1.1/docs/Database-Persist-Quasi.html), [singletonized types for type-level programming](https://hackage.haskell.org/package/singletons-th-3.0/docs/Data-Singletons-TH.html), [automatic `Lens` generation](https://hackage.haskell.org/package/lens-5.0.1/docs/Control-Lens-TH.html), among other things.

One of the main downsides to `TemplateHaskell` is that it can cause compilation times to increase significantly.
Let's dig into these slowdowns and talk about how to make them a bit less onerous.

# Firing up the external interpreter


EDIT: [Adam Gundry commented on `reddit`](https://www.reddit.com/r/haskell/comments/oiwl6z/templatehaskell_performance_tips/h4ya0ay/) that this section is wrong.
The external interpreter is only used if `-fexternal-interpreter` option is passed to GHC.
This may be why I was unable to detect the overhead from running an external interpret!

If you use `TemplateHaskell` at all in a module, then GHC needs to fire up an external interpeter.
GHC loads the interpreter (typically something like `ghci`), then executes/interprets the Haskell code.
Splices return one of the [Haskell syntax algebraic data types](https://www.stackage.org/haddock/lts-18.2/template-haskell-2.16.0.0/Language-Haskell-TH.html#g:18).

This has a constant overhead cost.
It's difficult to measure directly, since GHC doesn't have an easy means of outputting performance and timing information on a per module basis.
However, we can pass `+RTS -s -RTS` to GHC, which will cause it to print performance for a "package target." 

And, with GHC 9, I'm actually unable to determine a difference.
The noise in a given run appears to overwhelm the costs of actually firing up the interpreter.
So much for that!

(If you find different things, please let me know - you can file an issue or a PR to the [GitHub repo](https://github.com/parsonsmatt/parsonsmatt.github.io))

# Actually running code

GHC has two phases for TH:

1. Generating Code
2. Compiling Code

Generating code typically doesn't take much time at all, though this isn't guaranteed.

Fortunately, we can easily write a timing utility, since the `TemplateHaskell` generation type allows you to run arbitrary `IO` operations.

```haskell
import Data.Time (getCurrentTime, diffUTCTime)
import Language.Haskell.TH (Q, runIO, reportWarning)

timed :: String -> Q a -> Q a
timed message action = do
    begin <- runIO getCurrentTime
    result <- action
    end <- runIO getCurrentTime
    let duration = end `diffUTCTime` begin
    reportWarning $ concat [ "[", message, "]: ", show duration]
    pure result
```

Expert benchmarkers will complain about using `getCurrentTime` since it isn't monotonic, which is a valid complaint.
But we're not getting a real benchmark anyway, and we're mostly just going to see whether generation or compilation is dominating the elapsed time (hint: it'll almost always be compilation).

With this, we will get a reported warning about the duration of the code generation.
In [this reddit comment](https://www.reddit.com/r/haskell/comments/oi1x5v/tiny_use_of_template_haskell_causing_huge_memory/h4tr7n8/), I used it to determine that generation of some code was taking `0.0015s`, while compilation of the resulting code took `21.201s`.
The code looks like this:

```haskell
module Main where

import TuplesTH

$(timed "tuples" $ generateTupleBoilerplate 62)

main :: IO ()
main = do
    print $ _3 (1,2,42,"hello",'z')
```


The output looks like this:

```
Building executable 'th-perf-exe' for th-perf-0.1.0.0..
[1 of 2] Compiling Main

/home/matt/Projects/th-perf/app/Main.hs:11:2: warning: [tuples]: 0.001553454s
   |
11 | $(timed "tuples" $ generateTupleBoilerplate 62)
   |  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
[2 of 2] Compiling Paths_th_perf
  21,569,689,896 bytes allocated in the heap
   6,231,564,888 bytes copied during GC
     594,126,600 bytes maximum residency (17 sample(s))
       3,578,104 bytes maximum slop
            1641 MiB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0      1097 colls,     0 par    4.919s   4.921s     0.0045s    0.1072s
  Gen  1        17 colls,     0 par    4.466s   4.467s     0.2628s    1.0215s

  TASKS: 4 (1 bound, 3 peak workers (3 total), using -N1)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.001s  (  0.001s elapsed)
  MUT     time   11.813s  ( 12.135s elapsed)
  GC      time    9.385s  (  9.388s elapsed)
  EXIT    time    0.001s  (  0.007s elapsed)
  Total   time   21.201s  ( 21.530s elapsed)

  Alloc rate    1,825,890,582 bytes per MUT second

  Productivity  55.7% of total user, 56.4% of total elapsed
```

This sort of timing is usually only useful to determine whether you need to benchmark and optimize the *generation* phase or the *compilation* phase.
Optimizing *generation* is a relatively standard Haskell performance optimization process, so I won't cover it here.
If your code is mostly pure functions (or, with GHC 9, the new [`Quote`](https://www.stackage.org/haddock/nightly-2021-07-11/template-haskell-2.17.0.0/Language-Haskell-TH.html#t:Quote) type class), then it's straightforward to do.
Many `Q` features are not supported in `IO`, and it's difficult to accurately benchmark them.

# Optimizing Compilation

In the above example, GHC spends a tiny amount of time generating code, and then spends a *huge* amount of time compiling it.
What's going on?

In [Keeping Compilation Fast](https://www.parsonsmatt.org/2019/11/27/keeping_compilation_fast.html), I write that GHC compiles modules superlinearly in the size of the module.
That means that large modules take longer to compile than the same amount of code split up over several modules.
`TemplateHaskell` has no way of creating *modules*, or even altering the imports/exports of a given module, and so it necessarily might run into this problem.

We have two means of reducing generated code: spreading the use over multiple modules, and optimizing how we generate the code.

## Fewer Calls to TH

In [Splitting Persistent Models](https://www.parsonsmatt.org/2019/12/06/splitting_persistent_models.html), I wrote how to speed up compile-times by isolating the `persistent` model definitions into separate modules.
This results in many smaller modules, which GHC can compile much faster - in part because the modules are able to parallelized, and in part because they are smaller, and don't hit the superlinearity.

You can do this with any other thing, too. 
A large module that has a ton of data types and a `TemplateHaskell` declaration for each type will quickly become a problem in compilation.
Separating it out into multiple modules, each exporting a small subset of those types, will allow GHC to operate much more quickly.

## Smaller Code

It's relatively easy to generate a massive amount of Haskell code.
After all, the entire *point* is to make GHC generate code for us, because we don't want to write it ourselves!

In order to see how much code we're generating in a module, it's useful to enable the `-ddump-splices` option.
We can do this with a `GHC_OPTIONS` pragma above the module header:

```haskell
{-# language TemplateHaskell #-}

{-# OPTIONS_GHC -ddump-splices #-}

module Lib where

import Language.Haskell.TH.Syntax (liftTyped)

asdf :: Int
asdf = $$(liftTyped 3)
```

With this option, GHC will print the splice and the corresponding output while compiling the module.

```
Building library for th-perf-0.1.0.0..
[2 of 3] Compiling Lib
/home/matt/Projects/th-perf/src/Lib.hs:10:10-22: Splicing expression liftTyped 3 ======> 3
```

However, if you've got a performance problem, then you've probably got more output here than you have any idea what to do with.
In [the reddit thread](https://www.reddit.com/r/haskell/comments/oi1x5v/tiny_use_of_template_haskell_causing_huge_memory/h4tr7n8/), we ended up generating enough code that I couldn't scroll back to the top!
So, we'll want to dump the resulting splices to a file.
We can use the `-ddump-to-file`, and GHC will store the splices for a module in a file named `$(module-name).dump-$(phase)`.
If you're building with `stack`, then the files will be located in the `.stack-work` file.
We can get the resulting size of the file using `wc` and a bit of a glob.
In that investigation, this is the command and output:

```
$ wc -l .stack-work/**/*.dump-splices
15897 .stack-work/dist/x86_64-linux-tinfo6/Cabal-3.4.0.0/build/th-perf-exe/th-perf-exe-tmp/app/Main.dump-splices
```

That's 15,897 lines of code!
You can open that file up and see what it generates.
In that example, there wasn't much to optimize.

### Beware Splicing and Lifting

At the work codebase, we had a `TemplateHaskell` function that ended up taking several minutes to compile.
It iterated through all of our database models and generated a function that would stream each row from the database and verify that we could successfully parse everything out of the database.
This is nice to check that our `PersistField` definitions worked, or that our `JSONB` columns could all still be parsed.

I investigated the slow compile-time by dumping splices, and managed to find that it was splicing in the entire [`EntityDef`](https://www.stackage.org/haddock/lts-18.2/persistent-2.13.1.1/Database-Persist-EntityDef-Internal.html#t:EntityDef) type, multiple times, for each table.
This is a relatively large record, with a bunch of fields, and each `FieldDef` *also* is relatively large, with a bunch of fields!

The resulting code size was enormous.
Why was it doing this?
I looked into it and discovered this innocuous bit of code:

```haskell
do
    -- ...
    tableName <- [| getEntityHaskellName entityDef |]
    dbName <- [| getEntityDBName entityDef |]
    -- ...
    pure $ mkFun tableName dbName
```

You might expect that `tableName` would be an expression containing *only* the Haskell name of the entity.
However, it's *actually* the *entire expression* in the `QuasiQuote`!
Haskell allows you to implicitly lift things, sometimes, depending on scope and context etc.
The [`lift` in question refers to the `Lift` type class](https://www.stackage.org/haddock/lts-18.2/template-haskell-2.16.0.0/Language-Haskell-TH-Syntax.html#t:Lift), not the `MonadTrans` variant.
This ends up being translated to:

```haskell
tableName <- [| $(lift getEntityHaskellName) $(lift entityDef) |]
```

Lifting a function like this is relatively easy - you just splice a reference to the function.
So the resulting expression for the *function name* is something like:

```haskell
lift getEntityHaskellName
===>
    VarE 'getEntityHaskellName
```

In order to `lift` the `EntityDef` into the expression, we need to take the *complete run-time value* and transform it into valid Haskell code, which we then splice in directly.
In this case, that looks something like this:

```haskell
lift entityDef
===>
    EntityDef
        { entityHaskell = 
            EntityNameHS (Data.Text.pack "SomeTable")
        , entityDB =
            EntityNameDB (Data.Text.pack "some_table")
        , entityId = 
            EntityIdField (
                FieldDef
                    { fieldHaskell =
                        FieldNameHS (Data.Text.pack "id")
                    , fieldDB =
                        FieldNameDB (Data.Text.pack "id")
                    , fieldType = 
                        -- ....
                    , fieldSqlType =
                        -- ...
                    , -- etc... 
                    }
        , entityFields = 
            [ FieldDef { ... }
            , FieldDef { ... }
            , FieldDef { ... }
            , ...
            ]
        }
```

The combined expression splices this in:

```haskell
VarE 'getEntityHaskellName 
`AppE` 
    (ConE 'EntityDef 
    `AppE` 
        (ConE 'EntityNameHS 
        `AppE` 
            (VarE 'pack `AppE` LitE (StringL "SomeTable"))
        )
    `AppE`
        (ConE 'EntityNameDB ...)
    )
```

Which is no good - we're obviously *only* grabbing a single field from the record.
Fortunately, we can fix that real easy:

```haskell
tableName <- lift $ getEntityHaskellName entityDef
dbName <- lift $ getEntityDBName entityDef
```

This performs the access before we generate the code, resulting in significantly smaller code generation.

# Recompilation Avoidance

GHC is usually pretty clever about determining if it can avoid recompiling a module.
However, `TemplateHaskell` defeats this, and GHC doesn't even *try* to see if it can avoid recompiling - it just recompiles.
(This may be fixed in an upcoming GHC, but as of 9.0, it's still doing the safe/dumb thing).

We can't *fix* this, but we can work around it.
Try to isolate your `TemplateHaskell` use to only a few modules, and keep them as small as possible.

For example, suppose you have a ~500 line module that contains a bunch of data types, `deriveJSON` calls for those types, business logic, and handler API functions.
If *any* dependency of that module changes, you need to recompile the whole module due to the `TH` recompilation rule.
This needlessly recompiles everything - the datatypes, functions, JSON derivation, etc.

If you pull the datatypes and `TemplateHaskell` into a separate module, then *that* module needs to be recompiled every time.
However, GHC is smart enough to avoid recompiling the dependent module.
Suppose you split the 500 line module into two files, one of which is 20 lines of `data` and `TemplateHaskell`, and the other is 480 lines of functions, code, etc.
GHC will always recompile the 20 line module (very fast), and intelligently avoid recompiling the 480 lines when it doesn't need to.

## Recompilation Cascade

Recompilation Cascade is the name I've given to a problem where a tiny change triggers a `[TH]` rebuild of a module, and, since that module got rebuilt, every dependent module using `TH` gets rebuilt.
If you use `TemplateHaskell` pervasively, then you may end up having `[TH]` rebuilds for your entire codebase!
This can wreck incremental compile times.

Try to avoid this by separating out your `TemplateHaskell` into isolated modules, if at all possible.

If you use the `typed QQ literals` trick, then you can isolate those literals into a `Constants` module, and use those constants directly.
Instead of:

```haskell
module X where

sendEmailToFoo = sendEmail [email|foobar@gmail.com|] "hello world"
```

Consider using this instead:

```haskell
module Email.Constants where
  
    foobar_at_gmail = [email|foobar@gmail.com|]

module X where

    import Email.Constants

    sendEmailToFoo = sendEmail foobar_at_gmail "hello world"
```

With the latter form, `X` does not use `TemplateHaskell`, and therefore can skip recompilation if any dependencies change.
