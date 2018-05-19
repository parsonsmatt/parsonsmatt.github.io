---
title: "ghcid for the win!"
date: 2018-05-19
layout: post
categories: programming
---

## Supercharge your Haskell development experience with ghcid!

[`ghcid`](https://github.com/ndmitchell/ghcid) is -- at the current moment -- the most important tool for Haskell development environments.
It is fast, reliable, works on all kinds of projects, and is remarkably versatile.
You can use it with any editor workflow, primarily by *not* integrating your editor! (though there are integrations available if you're brave)
For these reasons, whenever someone asks about a Haskell IDE, I tell them to ignore the siren song of `ghc-mod`, `hdevtools`, `intero`, `haskell-ide-engine`, etc[^1], and just stick with the old faithful GHCi and `ghcid`.
Use whatever editor you want -- make sure it has syntax highlighting, and open up GHCi and/or `ghcid` in a separate terminal.

Here are some things we're going to do with it in this post:

- Basic warning/error reporting
- Fake type-of-expression support
- Reload your web app on every edit
- Run your test suite on every edit

As I think of additional "tricks" with `ghcid`, I will be updating this post and adding them here.
If you have a suggestion or question, please open an issue on my blog's GitHub :)

# Basic warnings and errors

This is the bread and butter of what `ghcid` is good for.
At this point, you're probably used to running `ghci` and doing `:reload` to see whether or not your code compiles.
`ghci` has some advantages over a `cabal new-build` or `stack build` or similar -- it loads everything in interpreted byte code by default, which is much faster, and is capable of very intelligent module reloading to minimize work.
This can cut the feedback time from compilation dramatically.

By default, `ghcid` will load with the flag `-fno-code` enabled.
This turns off all code generation, and basically only gives you syntax and type checking.
When you eventually customize your `ghcid` command, you will want to remember to either enable `-fno-code` or `-fobject-code`.
You need `-fobject-code` in order to do stuff like run tests, check Template Haskell expressions, etc.

To customize your `ghcid` command, you do this:

```
$ ghcid --command "the command to start ghci"
# example, for a Template Haskell heavy project:
$ ghcid --command "stack ghci package:lib --ghci-options=-fobject-code"
# example, to pick a single executable target:
$ ghcid --command "stack ghci package:exe:main-node"
# example, to defer type errors:
$ ghcid --command "stack ghci --ghci-options=-fdefer-type-errors"
```

At IOHK, I wrote up a `Makefile` with the common `ghcid` commands I use when working on the new wallet.
[This command](https://github.com/input-output-hk/cardano-sl/blob/develop/wallet-new/Makefile#L4-L6) lets me say `make ghcid` in the `wallet-new` subdirectory and get *lightning* fast reloading of code, display of all warnings and errors, and lets me run through refactorings quite nice and quickly.

# Fake type-of-expression support

Sometimes GHC feels like a reluctant wizard.
It knows things.
You know it knows things.
It knows that you know that it knows things.
But it doesn't want to tell you!

A common question that IDE authors want to ask is "What's the type of this expression?"
`ghc-mod`, `intero`, all try to support this, to varying degrees of success and performance.
But GHC is curious and easily distracted, and would much rather tell you that you're wrong than answer a question.
So let's trick the wizard!

Just today, I was working on this snippet of code, pulled from my [`servant-persistent`](https://github.com/parsonsmatt/servant-persistent) starter pack/example project:

```haskell
main :: IO ()
main = do
    env  <- lookupSetting "ENV" Development
    port <- lookupSetting "PORT" 8081
    logEnv <- defaultLogEnv
    pool <- makePool env logEnv
    store <- serverMetricStore <$> forkServer "localhost" 8000
    waiMetrics <- registerWaiMetrics store
    metr <- M.initializeWith store
    let cfg = Config { configPool = pool
                     , configEnv = env
                     , configMetrics = metr
                     , configLogEnv = logEnv }
        logger = setLogger env
    runSqlPool doMigrations pool
    generateJavaScript
    run port $ logger $ metrics waiMetrics $ app cfg
```

I wanted to know what the type of the `port` variable was.
With a more sophisticated toolchain, I might hover over `port`, and get a tooltip telling me what.
But, we're using the more primitive `ghcid`.
Well, we know what it *isn't* -- It's not `()`.
So, in the olden tradition, let's loudly be wrong and await correction:

```haskell
    run (port :: ()) $ logger $ metrics waiMetrics $ app cfg
```

We fire up `ghcid`, making sure to include the executable package target:

```
$ ghcid --command "stack ghci servant-persistent:exe:perservant"
```

And we're greeted with an error message:

```
/home/matt/Projects/servant-persistent/app/Main.hs:37:10: error:
    • Couldn't match type ‘()’ with ‘Int’
      Expected type: warp-3.2.22:Network.Wai.Handler.Warp.Types.Port
        Actual type: ()
    • In the first argument of ‘run’, namely ‘(port :: ())’
      In the expression: run (port :: ())
      In a stmt of a 'do' block:
        run (port :: ()) $ logger $ metrics waiMetrics $ app cfg
   |
37 |     run (port :: ()) $ logger $ metrics waiMetrics $ app cfg
   |          ^^^^^^^^^^
```

Ah, GHC expects it to be of type `Int`. There we go!

This works well with functions, too.
Let's say we want to know the type of `run`, instead:

```haskell
    (run :: ()) port $ logger $ metrics waiMetrics $ app cfg
```

`ghcid` is happy to tell us how wrong we are:

```
/home/matt/Projects/servant-persistent/app/Main.hs:37:5: error:
    • Couldn't match expected type ‘Integer
                                    -> Network.Wai.Application -> IO ()’
                  with actual type ‘()’
    • The function ‘run :: ()’ is applied to one argument,
      but its type ‘()’ has none
      In the expression: (run :: ()) port
      In a stmt of a 'do' block:
        (run :: ()) port $ logger $ metrics waiMetrics $ app cfg
   |
37 |     (run :: ()) port $ logger $ metrics waiMetrics $ app cfg
   |     ^^^^^^^^^^^^^^^^
/home/matt/Projects/servant-persistent/app/Main.hs:37:6: error:
    • Couldn't match expected type ‘()’
                  with actual type ‘warp-3.2.22:Network.Wai.Handler.Warp.Types.Port
                                    -> Network.Wai.Application -> IO ()’
    • Probable cause: ‘run’ is applied to too few arguments
      In the expression: run :: ()
      In the expression: (run :: ()) port
      In a stmt of a 'do' block:
        (run :: ()) port $ logger $ metrics waiMetrics $ app cfg
   |
37 |     (run :: ()) port $ logger $ metrics waiMetrics $ app cfg
   |      ^^^
```

Note that we get a slightly inconsistent message.
We've asserted that `run :: ()`, and it has *two* expected types: one from definition, and one from inferred use.
The inferred type is `Integer -> Application -> IO ()`.
The defined type is `Port -> Application -> IO ()`.
It *infers* `Integer` because, without `run` forcing `port` to be a `Port`, it has nothing else to tell it what to be, and therefore defaults to `Integer`.

# Reload your web app on every edit

`ghcid`, in addition to the `--command` flag, also takes a `--test` flag.
The flag name is somewhat too specific -- upon a successful compile with no warnings or errors, it will issue that command to GHCi for you.
It was initially intended for running tests, but we can do anything we like with it -- and we are going to use it to get our web application reloading lightning fast.

[This PR on the `servant-persistent` project](https://github.com/parsonsmatt/servant-persistent/pull/33) includes the necessary changes to get this running.
I have left a self-review on the PR, so I won't explain too much here.

The `ghcid` command we use is:

```
ghcid \
    --command "stack ghci servant-persistent" \
    --test "DevelMain.update"
```

This calls the `DevelMain.update` function on every successful compile.
The `DevelMain` module was mostly copied from the Yesod scaffold, with a few updates to make it work with this repo.
In truth, all you need to provide is a development-oriented function `IO Application` that boots your application and gives you the WAI value.
The DevelMain code uses `foreign-store` library to persist the state across GHCi sessions.

The `ghcid` README links to [an article on threepenny-gui apps](https://binarin.ru/post/auto-reload-threepenny-gui/) with a similar strategy.

This is *really* fast -- because GHCi can reload only exactly what it needs, and doesn't have to link anything, you get to see your changes almost immediately.

# Run your test suite on every edit

What's better than knowing your project compiles?
Knowing that it passes the test suite!

We'll use the `--test` command here, and we specify that we want to run the tests.
In the [`cardano-sl`](https://github.com/input-output-hk/cardano-sl/) repository, I put a [`Makefile` command for test running](https://github.com/input-output-hk/cardano-sl/blob/develop/wallet-new/Makefile#L8-L11):

```
ghcid-test: ## Have ghcid run the test suite for the wallet-new-specs on successful recompile
	ghcid \
	    --command "stack ghci cardano-sl-wallet-new:lib cardano-sl-wallet-new:test:wallet-new-specs --ghci-options=-fobject-code" \
	    --test "main"
```

There's a tricky bit here: We have to tell `stack ghci` which package targets to load.
I specify the library (`cardano-sl-wallet-new:lib`) so that it adds the library to the set of modules to watch for reloading.
Then I specify the test-suite I want to run (`cardano-sl-wallet-new:test:wallet-new-specs`).
Finally I use `--ghci-options=-fobject-code`, because this is fast, and I need to actually *run* the code (you get weird linker errors if you do `-fno-code` and try to run the nonexistent code).

# Conclusion

`ghcid` is awesome and everyone owes Neil Mitchell a beverage.

If you have a suggested use case you want added, ping me on GitHub and I'll credit you :)

[^1]: These are great projects.  But they are flaky, partially because GHC's API is difficult to interface with, and partially because GHCi's interactive features have some performance issues with larger code bases.  For small projects and libraries, they often work great.  For larger projects, or more varied environments, they show their pain.  You can spend a lot of time fussing with the editor integration and waiting on some command to finish, or you can just develop habits that don't need them (like `ghcid` in a separate terminal). I say this as the author of the [`intero-neovim`](https://github.com/parsonsmatt/intero-neovim) plugin.
