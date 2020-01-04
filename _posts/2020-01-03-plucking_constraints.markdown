---
title: "Plucking Constraints"
date: 2020-01-03
layout: post
categories: programming
---

There's a Haskell trick that I've observed in a few settings, and I've never seen a name put to it.
I'd like to write a post about the technique and give it a name.
It's often useful to write in a type class constrained manner, but at some point you need to discharge (or satisfy?) those constraints.
You can pluck a single constraint at a time.

This technique is used primarily used in `mtl` (or other effect libraries), but it also has uses in error handling.

# Gathering Constraints

We can easily gather constraints by using functions that require them.
Here's a function that has a `MonadReader Int` constraint:

```haskell
number :: (MonadReader Int m) => m Int
number = ask
```

Here's another function that has a `MonadError String` constraint:

```haskell
woops :: (MonadError String m) => m void
woops = throwError "woops!"
```

And yet another function with a `MonadState Char` constraint:

```haskell
update :: (MonadState Char m) => m ()
update = modify succ
```

We can seamlessly write a program that uses all of these functions together:

```haskell
program = do
    number
    woops
    update
```

GHC will happily infer the type of `program`:

```haskell
program
    :: ( MonadReader Int m
       , MonadError String m
       , MonadState Char m
       )
    => m ()
```

At some point, we'll need to actually *use* this.
Virtually all Haskell code that gets used is called from `main :: IO ()`. 

Let's try just using it directly:

```haskell
main :: IO ()
main = program
```

GHC is going to complain about this.
It's going to say something like:

```
No instance for `MonadReader Int IO`  arising from a use of `program`
    ....

No instance for `MonadState Char IO` arising from a use of `program`
    ....

Couldn't match type `IOException` with type `String`
    ....
```

This is GHC's way of telling us that it doesn't know how to run our program in `IO`.
Perhaps the `IO` type is not powerful enough to do all the stuff we want as-is.
And it has a conflicting way to throw errors - the `MonadError` instance is for the `IOException` type, not the `String` that we're trying to use.
So we have to do something differently.

# Unify

Let's try figuring out what GHC is doing with `main = program`.
First, we'll look at the equations:

```haskell
program
    :: ( MonadReader Int m
       , MonadError String m
       , MonadState Char m
       )
    => m  ()
main 
    :: IO ()
```

GHC sees that the "shape" of these types is similar.
It can substitute `IO` for `m` in `program`.
Does that work?

```haskell
program
    :: ( MonadReader Int IO
       , MonadError String IO
       , MonadState Char IO
       )
    => IO ()
```

Yeah! That looks okay so far.
Now, we have a totally concrete constraint: `MonadReader Int IO` doesn't have any type variables.
So let's look it up and see if we can find an instance

 . . .

Unfortunately, there's no instance defined like this.
If there's no instance for `IO`, then how are we going to satisfy that constraint?
We need to get rid of it and discharge it somehow!

The `mtl` library gives us a type that's sole responsibility is discharging the `MonadReader` instance: `ReaderT`.
Let's check out the `runReaderT` function:

```haskell
runReaderT 
    :: ReaderT r m a 
    -> r
    -> m a
```

`runReaderT` says:

> My first argument is a `ReaderT r m a`.
> My second argument is the `r` environment.
> And then I'll take off the `ReaderT` business on the type, returning only `m`.

We're going to pluck off that `MonadReader` constraint by turning it into a concrete type.
And `runReaderT` is one way to do that plucking.

GHC inferred a pretty general type for `program` earlier, but we can pick a more concrete type.

```haskell
program
    :: ( MonadError String n
       , MonadState Char n
       )
    => ReaderT Int n ()
```

Notice how we've shifted a constraint into a concrete type.
We've fixed the type of `m` to be `ReaderT Int n`, and all the other constraints got delegated down to this new type variable `n`.
We don't need to pick this concrete type at our definition site of `program`.
Indeed, we can provide that annotation somewhere else, like in `main`:

```haskell
main :: IO ()
main =
    let 
        program' 
            :: ( MonadError String n
               , MonadState Char n
               )
            => ReaderT Int n ()
        program' = 
            program
     in
        runReaderT program' 3
```

We're literally saying "`program'` is exactly like `program` but we're making it a tiny bit more concrete."

Now, GHC still isn't happy.
It's going to complain that there's no instance for `MonadState Char IO` and that `String` isn't equal to `IOException`.
So we have a little more work to do.

Fortunately, the `mtl` library gives us types for plucking these constraints off too.
`StateT` and `runStateT` can be used to pluck off a `MonadState` constraint, as well as `ExceptT` and `runExceptT`.

Let's write `program''`, which will use `StateT` to 'pluck' the `MonadState Char` constraint off.

```haskell
main :: IO ()
main =
    let 
        program' 
            :: ( MonadError String n
               , MonadState Char n
               )
            => ReaderT Int n ()
        program' = 
            program

        program''
            :: (MonadError String n)
            => ReaderT Int (StateT Char n) ()
        program'' =
            program'

        programRead 
            :: (MonadError String n)
            => StateT Char n ()
        programRead =
            runReaderT program'' 3
     in
        runStateT programRead 'c'
```

GHC *still* isn't happy - it's going to complain that `()` and `((), Char)` aren't the same types.
Also we still haven't dealt with `IOException` and `String` being different.

So let's use `ExceptT` to pluck out that final constraint.

```haskell
main :: IO ()
main =
    let 
        program' 
            :: ( MonadError String n
               , MonadState Char n
               )
            => ReaderT Int n ()
        program' = 
            program

        program''
            :: (MonadError String n)
            => ReaderT Int (StateT Char n) ()
        program'' =
            program'

        program''' 
            :: (Monad m)
            => ReaderT Int (StateT Char (ExceptT String m) ()
            -> m ()
        program''' =
            program''
-- ... snip ...

```

Okay, so I'm going to snip here and talk about something interesting.
When we plucked the `MonadError` constraint out, we didn't totally remove it.
Instead, we're left with a `Monad` constraint.
We'll get into this later.
But first, let's look at the steps that happen when we run the program, one piece at a time.


```haskell
-- ... snip ...
        programRead 
            :: (Monad m)
            => StateT Char (ExceptT String m) ()
        programRead =
            runReaderT program''' 3

        programStated
            :: (Monad m)
            => ExceptT String m ((), Char)
        programStated =
            runStateT programRead 'a'

        programExcepted 
            :: (Monad m)
            => m (Either String ((), Char))
        programExcepted =
            runExceptT programStated

        programInIO 
            :: IO (Either String ((), Char))
        programInIO =
            programExcepted

     in do
        result <- programInIO
        case result of
            Left err -> do
                fail err
            Right ((), endState) -> do
                print endState
                pure ()
```

GHC doesn't error on this!

When we finally get to `programExcepted`, we have a type that GHC can happily accept.
The `IO` type has an instance of `Monad`, and so we can just substitute `(Monad m) => m ()` and `IO ()` without any fuss.

These are all of the steps, laid out explicitly, but we can condense them significantly.

```haskell
program
    :: ( MonadReader Int m
       , MonadError String m
       , MonadState Char m
       )
    => m ()
program = do
    number
    woops
    update

main :: IO ()
main = do
    result <- runExceptT (runStateT (runReaderT program 3) 'a')
    case result of
        Left err -> do
            fail err
        Right ((), endState) -> do
            print endState
            pure ()
```

# Plucking Constraints!

The general pattern here is:

1. A function has many constraints.
2. You can pluck a single constraint off by making the type a little more concrete.
3. The rest of the constraints are delegated to the new type.

We don't need to *only* do this in `main`.
Suppose we want to discharge the `MonadReader Int` inside of `program`:

```haskell
program
    :: ( MonadState Char m
       , MonadError String m
       )
    => m ()
program = do
    i <- gets fromEnum
    runReaderT number i
    woops
    update
```

We plucked the `MonadReader` constraint off of `number` directly and discharged it right there.

So you don't have to just collect constraints until you discharge them in `main`.
You can pluck them off one-at-a-time as you need to, or as it becomes convenient to do so.

# How does it work?

Let's look at `ReaderT` and `MonadReader` to see how the type and class are designed for plucking.
We don't need to worry about the implementations, just the types:

```haskell
newtype ReaderT r m a

-- or, with explicit kinds,
newtype ReaderT
    (r :: Type)
    (m :: Type -> Type)
    (a :: Type)

class MonadReader r m | m -> r

instance (Monad m) => MonadReader r (ReaderT r m)

instance (MonadError e m) => MonadError e (ReaderT r m)
instance (MonadState s m) => MonadState s (ReaderT r m)
```

`ReaderT`, partially applied, as a few different readings:

```haskell
-- [1]
ReaderT r       :: (Type -> Type) -> (Type -> Type)
-- [2]
ReaderT r m                       :: (Type -> Type)
-- [3]
ReaderT r m a                              :: Type
```

1. With just an `r` applied, we have a 'monad transformer.' 
    Don't worry if this is tricky: just notice that we have something like `(a -> a) -> (a -> a)`.  At the value level, this might look something like: 
    ```haskell
    updatePlayer :: (Player -> Player) -> GameState -> GameState
    ```
    Where we can call `updatePlayer` to 'lift' a function that operates on `Player`s to an entire `GameState`.
2. With an `m` and an `r` applied, we have a 'monad.'
    Again, don't worry if this is tricky. Just notice that we have something that fits the same shape that the `m` parameter has.
3. Finally, we have a regular old value that has runtime types.

The important bit here is the 'delegation' type variable.
For the class we know how to handle, we can write a 'base case':

```haskell
instance (Monad m) => MonadReader r (ReaderT r m)
```

And for the classes that we don't know how to handle, we can write 'recursive cases':

```haskell
instance (MonadError e m) => MonadError e (ReaderT r m)
instance (MonadState s m) => MonadState s (ReaderT r m)
```

Now, GHC has all the information it needs to pluck a single constraint off and delegate the rest.

# Plucking Errors

I mentioned that this technique can also be applied to errors.
First, we need to write classes that work for our errors.
Let's say we have database, HTTP, and filesystem errors:

```haskell
class AsDbError err where
    liftDbError :: DbError -> err
    isDbError :: err -> Maybe DbError

class AsHttpError err where
    liftHttpError :: HttpError -> err
    isHttpError :: err -> Maybe HttpError

class AsFileError err where
    liftFileError :: FileError -> err
    isFileError :: err -> Maybe FileError
```

Obviously, our 'base case' instances are pretty simple.

```haskell
instance AsDbError DbError where
    liftDbError = id
    isDbError = Just

instance AsHttpError HttpError where
    liftHttpError = id
    isHttpError = Just

-- etc...
```

But we need a way of "delegating."
So let's write our 'error transformer' type for each error:

```haskell
data DbErrorOr err = IsDbErr DbError | DbOther err

data HttpErrorOr err = IsHttpErr HttpError | HttpOther err

data FileErrorOr err = IsFileErr FileError | FileOther err
```

Now, we can write an instance for `DbErrorOr`.

```haskell
instance AsDbError (DbErrorOr err) where
    liftDbError dbError = IsDbErr dbError
    isDbError (IsDbErr e) = Just e
    isDbError (DbOther _) = Nothing
```

This one is pretty simple - it is also a 'base case.'
Let's write the recursive case:

```haskell
instance AsHttpError err => AsHttpError (DbErrorOr err) where
    liftHttpError httpError = DbOther (liftHttpError httpError)
    isHttpError (IsDbErr _) = Nothing
    isHttpError (DbOther err) = isHttpError err
```

Here, we're just writing some boilerplate code to delegate to the underlying `err` variable.
We'd want to repeat this for every permutation, of course.
Now, we can compose programs that throw varying errors:

```haskell
program
    :: (AsHttpError e, AsDbError e)
    => Either e ()
program = do
    Left (liftHttpError HttpError)
    Left (liftDbError DbError)
```

The constraints collect exactly as nicely as you'd want, and the type class machinery allows you to easily go from the single type to the concrete type.

Let's 'pluck' the constraint.
We'll 'pick' a concrete type and delegate the other constraint to the type variable:

```haskell
program' 
    :: (AsHttpError e)
    => Either (DbErrorOr e) ()
program' = program
```

GHC is pretty happy about this.
All the instances work out, and it solves the problem of how to delegate everything for you.

We can pattern match directly on this, which allows us to "catch" individual errors and discharge them:

```haskell
handleLeft :: Either err a -> (err -> Either err' a) -> Either err' a
handleLeft (Right r) _ = Right r
handleLeft (Left l) f = f l

program'' :: AsHttpError e => Either e ()
program'' =
    handleLeft program $ \err ->
        case err of
            IsDbErr dbError ->
                Right ()
            DbOther dbOther ->
                Left dbOther
```

Voila! We've "handled" the database error, but we've delegated handling the HTTP error.
The technique of 'constraint plucking' works out here.

Now, an astute reader might note that *this technique is so boring.*
There's so much boilerplate code!!
SO MUCH!!!

Come on, y'all.
It's exactly the same amount of boilerplate code as the `mtl` library requires.
Is it really *that* bad?

> YESSSS!!!

Okay, yeah, it's pretty bad.
This encoding is primarily here to present the 'constraint plucking' technique.
You can do a more general and ergonomic approach to handling errors like this, but describing it is out of scope for this post.

Hopefully you find this concept as useful as I have.
Best of luck in your adventures!
