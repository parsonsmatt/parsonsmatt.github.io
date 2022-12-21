---
title: "Haddock Performance"
date: 2022-12-21
layout: post
categories: programming
---

I was recently made aware that `haddock` hasn't been working, at all, on the Mecury code base.
I decided to investigate.
Watching `htop`, `haddock` slowly accumulated memory, until it *exploded* in use and invoked the OOM killer.

My laptop has 64GB of RAM.
What.

I rebooted, and tried again.
With *no other programs running*, `haddock` was able to complete.
I enabled `-v2` and `--optghc=-ddump-timings`, which printed out GHC timing information and Haddock time/memory information.
With these flags, I could see that HTML generation *alone* was allocating 800GB of RAM.

I decided to look at the source code and see if there were any low hanging fruit.
Fortunately, there was!

# Don't use `WriterT` for logging

This section culminated in [this PR #1543](https://github.com/haskell/haddock/pull/1543), which I'll reference.
At time of writing, it has been merged.

The first thing that jumped out at me is that `haddock` used `WriterT` for logging.
Even worse, it used `WriterT [String]`.
This is maybe the slowest possible logging system you can imagine.

## At least, use the CPS Writer

`WriterT` has a big problem with space leaks.
Even the *strict* `WriterT` has this issue.
The only `WriterT` that can avoid it is the `CPS` variant, newly available in `mtl-2.3` in `Control.Monad.Writer.CPS`.
This is documented in Infinite Negative Utility's post ["Writer Monads and Space Leaks"](https://journal.infinitenegativeutility.com/writer-monads-and-space-leaks), which references two posts from Gabriella Gonzalez to the mailing list in [2012](https://mail.haskell.org/pipermail/libraries/2012-October/018599.html) and [2013](https://mail.haskell.org/pipermail/libraries/2013-March/019528.html).

## Don't use `[String]` or `String`

Beyond just leaking space, the `String` format for log messages is extremely inefficient.
This is equal to a `[Char]`, which builds up as a big thunk in memory until the whole `WriterT` computation can complete.
Each `Char` takes [2 words of memory](https://wiki.haskell.org/GHC/Memory_Footprint), and a `[Char]` takes `(1 + 3N) words + 2N` where `N` is the number of characters.
Or, ~5 words per character.
On a 64 bit machine, each word is 8 bytes, so each character costs 40 bytes.
A UTF-8 encoded `ByteString` will use 1 to 4 bytes per character.
Using a `ByteString` would make the representation much more compact, but these things get concatenated a bunch, and a `Builder` is the appropriate choice for an `O(1)` append.

Switching to `CPS.WriterT [Builder]` instead of `WriterT [String]` helps, but we're not done yet.
`[]` is a bad choice for a `WriterT`, since `tell` will `mappend` the lists.
`mappend` on lists can result in bad performance if it isn't associated correctly - `(((a ++ b) ++ c) ++ d) ++ e` is *accidentally quadratic*, since we'll traverse over the `a` list for every single `++` call.
A "difference list" has much faster appends, since it can associate things correcntly regardless of how you construct it.

To make it easier to use the API, I created an [`ErrorMessages`](https://github.com/haskell/haddock/pull/1543/files?diff=split&w=1#diff-fb24fea4d702952a1d040f7f9f4f7e547cbc2467b29587657c7fee02bfc1ee9bR686-R687) type:

```haskell
newtype ErrorMessages = ErrorMessages { unErrorMessages :: [Builder] -> [Builder] }
    deriving newtype (Semigroup, Monoid)

runErrMsgM :: ErrMsgM a -> (a, ErrorMessages)
runErrMsgM = runWriter . unErrMsgM

singleMessage :: Builder -> ErrorMessages
singleMessage m = ErrorMessages $ (m :)

errorMessagesToList :: ErrorMessages -> [Builder]
errorMessagesToList messages = unErrorMessages messages []
```

## Don't use `nub`

There were a few places that called `nub`.
`nub` is `O(n^2)` on a linked list - it must search the entire list for every element in the list to verify uniqueness.

- [`nub` called on `packages`](https://github.com/haskell/haddock/pull/1543/files?diff=split&w=1#diff-421689688051a1380a572c21720065b628525bba15300b606aca78753e699fdaL464), a list of installed packages.
- [`nub` called on `themeFiles`](https://github.com/haskell/haddock/pull/1543/files?diff=split&w=1#diff-48dfb971b4cf3e94bd44acfafd718ac029840968b23192f25ac716d2cffe831fL177)
- [`nub` called on `messages`](https://github.com/haskell/haddock/pull/1543/files?diff=split&w=1#diff-b5017e9be522fcf7f7bdfdf3fc9e8d32897ec99b84fc4122cee231ba83a0ea3cL327), the *result* of the `WriterT [String]` computation

That last one is especially gnarly.
We're doing an `O(n^2)` job, leaking space along the way, and finally we accumulate the big list- only to do an `O(n^2)` traversal over it to delete duplicates.

Fortunately, each call site of `nub` can be replaced with the *easy* `ordNub`:

```haskell
ordNub :: Ord a => [a] -> [a]
ordNub = Set.toList . Set.fromList
```

This also sorts the list, which may not be desired.
A more cumbersome implementation does this:

```haskell
ordNub :: Ord a => [a] -> [a]
ordNub = go Set.empty
  where
    go seen [] = []
    go seen (x:xs) 
        | Set.member x seen = 
            go seen xs
        | otherwise = 
            x : go (Set.insert x seen) xs
```

## Results

This small change resulted in a *huge* improvement in performance for my test case.
Running `haddock` on the `persistent-test` library, I observed a 30% improvement in the time to generate documenation with total memory use 4% better.
Allocations went from 42GB to 25GB.

I didn't bother profiling to determine this as a hot-spot - it's *always* wrong to use `WriterT` as a logger.
A further performance improvement would be to remove `WriterT` entirely and simply output the messages directly.
Then instead of retaining a big difference list of log messages, you can just print them right then and there.

# `xhtml` and `[String]`

This section is represented by [this `haddock` PR](https://github.com/haskell/haddock/pull/1546) and [this `xhtml` PR](https://github.com/haskell/xhtml/pull/18).

`haddock` uses a library [`xhtml`](https://hackage.haskell.org/package/xhtml) for generating the HTML.
This library is *old* - the initial copyright is 1999.
`xhtml` predates `ByteString` entirely, which has an earliest copyright of 2003.

Anyway, we have a similar problem.
The `Html` type is defined like this:

```haskell
newtype Html = Html { getHtmlElements :: [HtmlElement] }

data HtmlElement
      = HtmlString String
      | HtmlTag {
              markupTag      :: String,
              markupAttrs    :: [HtmlAttr],
              markupContent  :: Html
              }

-- | Attributes with name and value.
data HtmlAttr = HtmlAttr String String
```

The `xhtml` library uses `++` on lists all over the place.
The [`renderHtml'` function](https://github.com/haskell/xhtml/blob/68353ccd1a2e776d6c2b11619265d8140bb7dc07/Text/XHtml/Internals.hs#L286-L299) uses `ShowS`, fortunately - this is difference list of `Char`, so we probably won't be seeing pessimal performance.

Like the above PR to remove `WriterT [String]` and replace it with a difference list of `Builder`, I did that to `xhtml`.
All explicit lists are now difference lists, and all `String` are replaced with `Builder`.

The performance results are impressive:

| | Haddock Head | `xhtml` Builder | Absolute Difference | Relative Change |
|-|--------------|-----------------|---------------------|-----------------|
| HTML allocations  | 1134 MB | 1141 MB   | +7 MB    | 0.6% worse |
| HTML time:        | 380 ms  | 198 ms    | -182 ms  | 47.9% improvement |
| Total Memory:     | 554 MB  | 466 MB    | -88 MB    | 15.9% improvement |
| Total Allocated:  | 16.0 GB | 16.0 GB   |  0  | No change |
| Max residency:    | 238 MB  | 195 MB    | -43 MB  | 18.1% improvement |
| Total Time:       | 10.88 s  | 6.526s s | -4.354 s    | 40% improvement |

Avoiding `[]` and `String` *halves* the time to render HTML, and results in a 40% overall improvement in the time to run `haddock`.
While we don't allocate any fewer memory during HTML generation, we're using 16% less total memory and maximum residency is down by 18%.

# Conclusion

Haskell performance doesn't have to be hard.
If you avoid common footguns like `WriterT`, `[]`, `String`, `nub`, etc. then your code will probably be pretty quick.

Picking the low hanging fruit is usually worthwhile, even if you haven't spent the effort determining the real problem.
Profiling shows that `haddock` spends an enormous amount of time generating *object code* - a necessary step for any module that has `TemplateHaskell` enabled.
With GHC 9.6, we'll be able to pass `-fprefer-byte-code`, which will use the much faster byte code representation instead of object code.

Even in HTML generation, profiling indicates that we spend the majority of time doing `fixChar` - the process of escaping a character into an HTML appropriate `Builder`.
We also spend a bunch of time regenerating HTML for re-exports - the HTML documentation for a datatype, function, type class, etc is generated fresh for every module that exports it.

Even if HTML were perfectly optimized, Haddock's current design creates a huge `[Interface]`, where each `Interface` is a module that you are generating documentation for.
This `[Interface]` must be retained in memory, because it is passed to each "component" of the documentation build.
Refactoring `haddock` to stream these interfaces isn't obvious, since some doc building steps require summary of the entire `[Interface]` in order to proceed.

Figuring out a fix for the "real problems" would have been much more difficult than these easy fixes, which have still made a huge difference in overall perforamnce.
