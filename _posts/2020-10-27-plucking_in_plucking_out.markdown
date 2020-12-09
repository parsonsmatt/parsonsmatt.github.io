---
title: "Plucking In, Plucking Out"
date: 2020-10-27
layout: post
categories: programming
---

In [plucking constraints]({% post_url 2020-01-03-plucking_constraints %}), I talked about a way to shrink a set of constraints by partially concretizing it.
At the end of the article, I show how to use it for errors.
The [`plucky`](https://hackage.haskell.org/package/plucky) package documents the technique, and my upcoming library [`prio`](https://github.com/parsonsmatt/prio/blob/master/src/Lib.hs) embed plucking into run-time exceptions, effectively solving [the trouble with typed errors]({% post_url 2018-11-03-trouble_with_typed_errors %}).
Figuring that out has been bothering me for two and a half years!

This got me thinking.
Michael Snoyman's [`rio`](https://www.stackage.org/lts-16.20/package/rio-0.1.19.0) package is an alternative Prelude which bakes the [`ReaderT` Design Pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/) in to the base monad `RIO r a`, and then encourages users to write in a polymorphic style.

```haskell
-- Concrete, monomorphic:
foo :: Int -> RIO AppEnv String

-- Abstract, polymorphic:
foo :: ( MonadReader env m, MonadIO m
       , HasThing env, HasOtherThing env
       ) 
    => Int
    -> m String
```

The abstract `foo` specifies exactly what the `env` must satisfy.
With a concrete type, the `AppEnv` type is almost certainly *too big* - it probably needs to support all kinds of things, and `foo` is only a small part of that.

In [The Trouble With Typed Errors]({% post_url 2018-11-03-trouble_with_typed_errors %}), I argue that an error type that is too big is a major problem.
But I've never really been bothered with an `env` type that is too big.
Why is that?

# Profunctors

If I combine `prio`'s `CheckedT e m a` for checked runtime exceptions and `rio`'s `RIO r a`, I get this neat type:

```haskell
newtype App r e a = App 
  { unApp :: CheckedT e (RIO r a) 
  }
  deriving 
    newtype 
      ( Functor, Applicative, Monad, MonadIO
      , MonadReader r, MonadError e, MonadState s
      -- etc
      )
```

Let's simplify.
Instead of runtime exceptions with `CheckedT`, we'll use `Either e`.
We'll inline the transformers, too.

```haskell
newtype App r e a = App 
  { unApp :: r -> IO (Either e a) 
  }
  deriving
    via (ReaderT r (ExceptT e IO))
      ( Functor, Applicative, Monad, MonadIO
      -- etc
      )
```

If you squint a little, this is a `Profunctor` with an input of type `r` and an output of type `Either e a`.
`Either e a` is even just a way of "blessing" one possible output as the "bind" output while the `e` is a "short-circuit" output.
`Profunctor` is a fancy math word that makes me think about category theory.
If I apply some sloppy category theory thinking, maybe I can satisfactorily answer "Why is a too-big output bad, while a too-big input is fine?"

# The Problem With `catch`

The problem with `catch` is that it doesn't change the error set at all.

```haskell
catch
  :: Either e a
  -> (e -> Either e a)
  -> Either e a
```

There is no type-level evidence that anything has changed.
Alexis King would call this [validation, not parsing](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/).

The Correct type of `catch` is something like:

```haskell
catch
  :: forall input small rest
   . ( Contains small input
     , rest ~ Delete small input 
     )
  => Either input a
  -> (small -> Either rest a)
  -> Either rest a
```

The big difference here is that the `small` type of problem has been handled, and we're only left with the `rest`.
Using Constraint Plucking, this signature is simply:

```haskell
catch
  :: Either (Either small rest) a
  -> (small -> Either rest a)
  -> Either rest a
```

Since `catch` is our Big Problem with the output, we should get to our Big Problem with the *input* by taking the *dual* of this function.
Taking the dual means flipping the arrows and replacing sums with products.
(Real category theory experts will yell at me for this, I Will Not Log Off, but I will accept a PR linking to a better explanation).

```haskell
-- Provide alias names to help with flipping the arrows
catch0 :: start -> handler -> result

-- Let's do `start` now. We'll just convert Either to Tuple.
start ~ Either (Either small rest) a
start0 ~ ( (small, rest), a )

-- Handler:
handler ~ (small -> Either rest a)
-- Flip arrow
handler0 ~ (Either rest a -> small)
-- Tuplize
handler1 ~ ((rest, a) -> small)

-- Result
result ~ Either rest a
-- Tuplize
result ~ (rest, a)

cocatch0 
  :: start 
  -> handler 
  -> result
-- substitute our named expressions
cocatch 
  :: ((small, rest), a)
  -> ((rest, a) -> small)
  -> (rest, a)
cocatch ((small, rest), a) k =
  (rest, a)
```

Huh.
This is totally useless.
I suspect I have performed the flipping of arrows incorrectly.
[Edward Yang](http://blog.ezyang.com/2010/07/flipping-arrows-in-coburger-king/) has a really good post on category theory and flipping arrows, so I'll read that and come back to this.

# Monads?

`catch` is really like a `bind` on the `e` parameter of `Either e a`.
Just compare the signatures:

```haskell
bind
  :: Either e a
  -> (a -> Either e a)
  -> Either e a

catch
  :: Either e a
  -> (e -> Either e a)
  -> Either e a
```

This makes me think: `Monad` is the wrong approach.
I want to look at the equivalent `Comonad`.
The dual of `Either e a` is `(e, a)` - the `Env` comonad.
So let's look at `cobind` on `Env`:

```haskell
cobind
  :: Env e a
  -> (Env e a -> b)
  -> Env e b

cocatch
  :: Env e a
  -> (Env e a -> x)
  -> Env x a
```

Now we're getting somewhere.
Let's rewrite as a tuple:

```haskell
cocatch
  :: (e, a)
  -> ((e, a) -> x)
  -> (x, a)
cocatch (e, a) k = 
  (k (e, a), a)
```

Hmm. Let's curry that second argument.

```haskell
cocatch
  :: (e, a)
  -> (e -> a -> x)
  -> (x, a)
cocatch (e, a) k = (k e a, a)
```

Looks a lot like `censor` from `MonadWriter`:

```haskell
censor :: (w -> w) -> Writer w a -> Writer w a
censor f (Writer x) = Writer $ cocatch (\w _ -> f w) x
```

It's really a more specialized variant of `mapWriter`:

```haskell
cocatch w f = mapWriter (\(e, a) -> (f e a, a)) w
```

Well, that's interesting, but it doesn't answer my question.
The dual of the `Either` monad is the `Env` comonad, which is equivalent to the `Writer` monad, not the `Reader` monad.

Maybe I need to get back to the `Profunctor` approach - inspect why the contravariant part of the functor is OK to be too big.

# Inputs and Outputs

With `App r e a` , we have two output types and an input type.
We have a few tools for working on these type parameters.

```haskell
fmap  :: (a -> b) -> App r e a -> App r e b
fmapL :: (e -> f) -> App r e a -> App r f a
local :: (r -> x) -> App x e a -> App r e a
```

`local` is like our `contramap` function, but it won't work because the kinds aren't right.

We can introduce effectful variants:

```haskell
bind  :: (a -> App r e b) -> App r e a -> App r e b
catch :: (e -> App r f b) -> App r e a -> App r f a
what  :: (r -> App x e a) -> App x e a -> App r e a
```

Okay, `what` has my interest.
It can't be defined.
We never have an `x`, so there's no way we can discharge it.
Maybe we can translate this and flip arrows slightly differently...

```haskell
-- unwrap the newtypes:
local :: (r -> x) -> (x -> Either e a) -> (r -> Either e a)

localM 
  :: (r -> x -> Either e a) 
  -> (x -> Either e a)
  -> (r -> Either e a)
```

Okay, this is  obviously wrong.
The `x` is in the wrong spot!
We're not supposed to be accepting an `x` as input, we're supposed to be *producing* one.
Let's try that again.

```haskell
localM
  :: (r -> Either e x) -- App r e x
  -> (x -> Either e a) -- App x e a
  -> (r -> Either e a) -- App r e a
```

This looks much more feasible.

```haskell
localM :: App r e x -> App x e a -> App r e a
localM mkX withX = do
  x <- mkX
  localApp (\_ -> x) withX
```

Well, this is a bit of a weird one.
If we can produce an `x` from an `App r e`, then we can run an `App x e a` action into `App r e a`.

Plucking with `catch` is about incrementally *removing* constraints that add cases to a type.
So plucking with `localM` is about incrementally *adding* types to the environment product.

```haskell
localMPluck
  :: App r e x
  -> App (x, r) e a
  -> App r e a
localMPluck mkX withXR = do
  x <- mkX
  localApp (\r -> (x, r)) withXR
```

The tuple type works OK for a constraint plucking interface, but I don't really like it, so let's define a nested product.

```haskell
data a :* b = a :* b
infixr 7 :*
```

The pattern for a plucking interface is to write a class that can delegate to a type parameter.

```haskell
class Has t env where
  get :: env -> t

instance Has x x where
  get = id

instance {-# overlapping #-} Has x (x :* y) where
  get (a :* _) = a

instance {-# overlappable #-} Has x y => Has x (a :* y) where
  get (_ :* b) = get b
```

And, let's give it a nicer name - `provide`.
It's providing some new bit of information to the environment.

```haskell
provide
  :: App r e new
  -> App (new :* r) e a
  -> App r e a
```

# A Man Provides

Let's run `App` into `IO`.
We'll guarantee via `RankNTypes` that it doesn't need anything and doesn't throw anything.

```haskell
runApp :: (forall r e. App r e a) -> IO a
runApp action = do
  eitherVoidA <- runExceptT $ runReaderT (unApp action) ()
  case eitherVoidA of
    Left v -> absurd v
    Right a -> pure a
```

Since we're requiring a value that can work with *any* `r`, we can provide a `()` value.
And, likewise, since we're requiring that the `e` error type is *any* type we want, we can select `Void`.
We get a guarantee that all checked exceptions are handled and we don't need anything from the environment.

Now, let's use this stuff.
We're going to need a logger, first of all.

```haskell
data Logger = Logger

mkLogger :: App r e Logger
mkLogger = pure Logger

logInfo :: (Has Logger r) => String -> App r e ()
logInfo msg = do
  Logger <- asks get
  liftIO $ putStrLn msg

main :: IO ()
main = do
  runApp $ do
    logInfo "hello"
```

Our `main` fails with an error: `No instance for (Has Logger r)`.
So we need to provide one.

```haskell
main :: IO ()
main = do
  runApp $ do
    provide mkLogger $ do
      logInfo "asdf"
```

We can pass `provide mkLogger` to `runApp` because `mkLogger` has no requirements on the `e` or `r` types.
Now, let's make a database handle.
This one is going to require logging.

```haskell
data DbHandle = DbHandle

mkDbHandle :: (Has Logger r) => App r e DbHandle
mkDbHandle = do
  logInfo "making postgres handle"
  pure DbHandle

getUserIds :: (Has DbHandle r) => App r e [Int]
getUserIds = do
  DbHandle <- asks get
  pure [1,2,3]
```

We can't call this next to `logInfo` above, because we haven't `provide`d it.
The following code black fails with an error `No isntance for (Has DbHandle r)`.

```haskell
main :: IO ()
main = do
  runApp $ do
    provide mkLogger $ do
      logInfo "asdf"
      ids <- getUserIds
      forM_ ids $ \id -> do
        logInfo (show id)
```

We can fix it by providing one:

```haskell
main :: IO ()
main = do
  runApp $ do
    provide mkLogger $ do
      logInfo "asdf"
      provide mkDbHandle $ do
        ids <- getUserIds
        forM_ ids $ \id -> do
          logInfo (show id)
```

This works just fine.
What about throwing errors?

# Plucking Errors

We need to pluck a sum type.

```haskell
data a || b = This a | That b

class lil :< big where
  inject :: lil -> big
  project :: big -> Maybe lil

instance lil :< lil where
  inject = id
  project = Just

instance {-# overlapping #-} lil :< (lil || rest) where
  inject = This
  project x = case x of
    This a -> Just a
    _ -> Nothing

instance {-# overlappable #-} (lil :< rest) => lil :< (not || rest) where
  inject = That . inject
  project x = case x of
    This _ -> Nothing
    That a -> project a

throw :: lil :< big => lil -> App r big a
throw = throwError . inject

catch :: App r (lil || rest) a -> (lil -> App r rest a) -> App r rest a
catch action handler  =
  ReaderT $ \r ->
    runReaderT action r `catchE` \lilOrRest ->
      case lilOrRest of
        This lil ->
          runReaderT (handler lil) r
        That rest ->
          throwE rest
```

Let's suppose that creating a database handle actually has a `PgError` exception associated with it.


```haskell
data DbExn = DbExn

mkDbHandle :: (Has Logger r, DbExn :< e) => App r e DbHandle
mkDbHandle = do
  logInfo "making postgres handle"
  if 3 == 4
    then pure DbHandle
    else throw DbExn
```

Now, our `main` no longer compiles!
GHC complains about `No instance for (DbExn :< e) arising from a use of mkDbHandle`.
So we need to *discharge* that exception in the above block.

We'll define `handle` as a shorthand (`handle = flip catch`), and we can discharge the error:

```haskell
main :: IO ()
main = do
  runApp $ do
    provide mkLogger $ do
      logInfo "asdf"
      handle (\DbExn -> logInfo "uh oh") 
        $ provide mkDbHandle 
        $ do
          ids <- getUserIds
          forM_ ids $ \id -> do
            logInfo (show id)
```

This now compiles.
But I suspect the `handle f . provide mk` pattern is common enough to factor it out.

```haskell
providing
  :: (lil -> App r rest a)
  -> App r (lil || rest) x
  -> App (x :* r) (lil || rest) a
  -> App r rest a
providing handler provider = 
  handle handler . provide provider

main :: IO ()
main = do
  runApp $ do
    provide mkLogger $ do
      logInfo "asdf"
      providing (\DbExn -> logInfo "uh oh") mkDbHandle $ do
        ids <- getUserIds
        forM_ ids $ \id -> do
          logInfo (show id)
```

Neat!

# Wait, where were we?

Right.
Right. I'm trying to figure out why a big type for an error is a Problem, but a big type for an environment isn't.
In doing so, I figured out how to make composable, growing environments that play nicely with constraints.

If I don't have a composable, growing environment that plays nicely with constraints, then what do I have?
Usually just a big `AppEnv` type that has everythign I ever need.
Functions may be defined in terms of constraints, but usually are just defined in:

```haskell
newtype App a = App { unApp :: ReaderT AppEnv IO a }
```

When do I need to get away from `App`?
Only when I want to use a function in another *context*.
It can be frustrating to provide an entire `AppEnv` when I only need a database handle.
But it rarely *bites* me in a way that is frustrating.
Why?

Ignoring inputs is safe.
But ignoring outputs is dangerous.

```haskell
fine :: (Int, String) -> Int
fine (i, _) = i

bad :: Either Int String
bad = Left 2
```

`fine` throws away the input.
It's wasteful, but it's not *lying*.
`bad`, however, encodes partiality - it says it *might* have an `Int` or a `String`, but it definitely doesn't have a `String`.
It's not *lying*, or even *wrong*, it's just not helpful.
If you want to use the `Int` inside, you gotta handle the `Right String` case.

```haskell
useBad :: (String -> Int) -> Int
useBad handleString = either id handleString bad
```

Now, `fine` has a problem: it's too specific about it's requirements.
If I want to call `fine` with the result of `useBad`, then I have to come up with a `String` from somewhere.
I don't *know* whether or not it actually uses the `String` or not, so I have no idea if it matters what `String` I use.
We can make `fine`'s type more precise:

```haskell
fine :: (Int, unused) -> Int
fine (i, _) = i
```

Now, since `unused` is a type variable that I get to pick, I can pass `()` to satisfy the type checker.

Likewise, we can refine `bad`'s type to make it more specific to what it has:

```haskell
bad :: Either Int Void
bad = Left 2
```
Now we *know* we're never getting a `Right` out of it, so we don't have to worry about it.
Our calling code is much simpler:

```haskell
useBad :: Int
useBad = either absurd id bad
```

# Getting vs Handling

It's a question of "Where to get?" vs "How to handle?"

If you know the inputs to a function, you need to provide all of them.
If some part of the input is unnecessary, you may perform extra work providing it (only for it to be thrown away).

If you know the outputs of your function, you need to handle all of them.
If some part of the output is unnecessary, you may perform extra work handling it (only for the code path to never be used).

It's less consequential if we're sloppy about our inputs.
Computational waste usually isn't that expensive.
But we care much more about the correctness and shape of our outputs.

I'm still not satisfied with what I've covered here.
I think there's a lot more to this.
