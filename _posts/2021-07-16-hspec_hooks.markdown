---
title: "Hspec Hooks"
date: 2021-07-16
layout: post
categories: programming
---

The `hspec` testing library includes many useful facilities for writing tests, including a powerful "hooks" capability.
These hooks allow you to provide data and capabilities to your tests.

# `SpecWith`

The typical `hspec` test suite looks like this:

```haskell
main :: IO ()
main = 
    hspec specs

specs :: Spec
specs = do
    describe "math" $ do
        it "1 + 1" $ do
            1 + 1
                `shouldBe`
                    2
        it "3 * 2" $ do
            3 * 2
                `shouldBe`
                    6
    describe "words" $ do
        it "breaks stuff up" $ do
            words "asdf asdf asdf"
                `shouldBe`
                    ["asdf", "asdf", "asdf"]
```

Everything is a `Spec`, and it's all nice and cute.

Suddenly, you want to provide a database connection to each item in a spec.
You can do this using a plain ol' function argument, and this works alright.

```haskell
main :: IO ()
main = do
    db <- createDatabase
    specs db

specs :: DB -> Spec
specs db = do
    describe "SELECT" $ do
        it "works" $ do
            result <- runDb db "SELECT...."
            result
                `shouldBe`
                    [1,2,3]
```

But - what if we want to have a *fresh* database connection made, for each test?
Well, then it's a bit more awkward.

```haskell
specs :: Spec
specs = do
    describe "SELECT" $ do
        it "works" $ do
            db <- createDatabase
            result <- runDb db "SELECT...."
            result
                `shouldBe`
                    [1,2,3]
        it "does other stuff" $ do
            db <- createDatabase
            result <- runDb db "OTHER STUFF..."
            result 
                `shouldBe`
                    [4,3,2]
```

That's not much fun!

`hspec` gives us a function `before` that can be used to provide a fresh value for each item in a spec.

```haskell
specs :: Spec
specs = before createDatabase $ do
    describe "SELECT" $ do
        it "works" $ \db -> do
            result <- runDb db "SELECT...."
            result
                `shouldBe`
                    [1,2,3]
        it "does other stuff" $ \db -> do
            result <- runDb db "OTHER STUFF..."
            result 
                `shouldBe`
                    [4,3,2]
```

Let's look at the type of `before`.

```haskell
before :: IO a -> SpecWith a -> Spec
```

This raises some questions.
What is a `SpecWith`?
All of the `describe` stuff functions in [a `SpecM` monad](https://hackage.haskell.org/package/hspec-core-2.8.2/docs/Test-Hspec-Core-Spec.html#g:2), which constructs the `Spec` tree and allows for filtering, focusing, and mapping of spec items.
That link shows that `type Spec = SpecWith ()`.
Expanding our type, we get this:

```haskell
before :: IO a -> SpecWith a -> SpecWith ()
```

A `SpecWith` is a test that expects some additional context.

# More `before`

So, `before` is used to provide a fresh thing to every test item.
What if you want to create a single thing and have it be shared among every spec item?

We can use `beforeAll` to accomplish that.
It has the same signature.
The only difference is that the creation action is run once, and then shared among every test.

What if you don't want to pass anything to the tests, but you want to run some action?

```haskell
before_ :: IO () -> SpecWith a -> SpecWith a
```

You may be wondering: "What if I want to run an action *once* before *all* the items in the test go, but don't provide a value?"
`hspec` has you covered - `beforeAll_` works exactly like that.

There's one more tricky thing here - `beforeWith`.

Note that in `before`, the result is a `Spec` - a test without extra context.
How do we call `before` on something that has already had `before` called on it?
`beforeWith` comes to the rescue.

```haskell
beforeWith :: (b -> IO a) -> SpecWith a -> SpecWith b
```

If you understand `Contravariant` functors, then that intuition will carry you a decent way.
If you don't, that's cool - let's dig into it.

Let's say we have some group of tests that want to run a set of migrations against the database, and also provide some information along with the database connection.
We'll insert a fake `User` and make the `Id` available to the resulting tests.

```haskell
spec :: Spec
spec = before createDatabase $ do
    describe "SELECT" $ do
        it "has a database" $ \db -> do
            ...

    beforeWith createUser $ describe "With User" $ do
        it "has a db and a user" $ \(db, userId) -> do
            ...

createUser :: DB -> IO (DB, UserId)
createUser db = do
    userId <- runDb $ insert User { name = "asdf" }
    pure (db, userId)
```

Now, `beforeWith` means that each item gets run before each spec.
So each test item in the database will have a different `User` created for the test.

Naturally, there's `beforeAllWith`, which would only be run once, and would provide the same `UserId` to each test item.

You may wonder: "Is there a `beforeAllWith_`? Or even just `beforeWith_`?"
There is not, and the reason is that they're redundant.
Note how `before_` and `beforeAll_` don't affect the context of the specs.

```Haskell
before_    :: IO () -> SpecWith a -> SpecWith a
beforeAll_ :: IO () -> SpecWith a -> SpecWith a
```

If we want to *not* affect the context of the spec, then we can just return it directly.

```haskell
beforeWith_ :: (a -> IO ()) -> SpecWith a -> SpecWith a
beforeWith_ action = 
    beforeWith $ \a -> do
        action a
        pure a
```

# `after`

The `before` family of functions are useful for providing data and preparing the state of the world for a test.
`after` is useful for tearing it down, or cleaning up after a test.

```haskell
after :: ActionWith a -> SpecWith a -> SpecWith a
```

`ActionWith` is a type synonym, so let's review the definition and inline it here:

```haskell
type ActionWith a = 
    a -> IO ()

after :: (a -> IO ()) -> SpecWith a -> SpecWith a
```

(I often find that inlining type synonyms helps with `hspec` when reading and understanding it)

Let's write a function that deletes the `User` out of the database for all the terms.

```haskell
spec :: Spec
spec = before createDatabase $ do
    describe "SELECT" ...

    beforeWith createUser $ 
        after deleteUser $
        describe "With User" $ do
            it "has a db and a user" $ \(db, userId) -> do
                ...

createUser :: DB -> IO (DB, UserId)
createUser db = do
    userId <- runDb $ insert User { name = "asdf" }
    pure (db, userId)

deleteUser :: (DB, UserId) -> IO ()
deleteUser (db, userId) = do
    runDb $ delete userId
    pure ()
```

Now, we aren't polluting our database with all those `User` rows.

`afterAll` does what you expect, if you know how `beforeAll` works.
The action is run exactly once, after all spec items have been run.
If we replace `after` with `afterAll` in the above code, we'll get some slightly weird results.

```haskell
beforeWith createUser $
    afterAll deleteUser $ 
        describe "With User" $ do
            it "has a thing" ...
            it "likes cats" ...
            it "also likes dogs" ...
```

The `beforeWith` is called *each time* - so we create a fresh `User` for each database.
`afterAll` gets called on the *last* spec item - so we keep the first two `User` rows in the database.

`after_` and `afterAll_` ignore the `a` from `SpecWith a`.
Instead of being an `ActionWith a` or an `(a -> IO ())` as the first parameter, it's merely the `IO ()` action.

# `around`

`around` is pretty tricky.
It encapsulates the pattern above - create something for each test, then tear it down afterwards.
Most uses of `before create $ after destroy $ ...` can be refactored to use `around` and enjoy greater exception safety.

Let's start off with `around_`.
It doesn't worry about the extra context, which makes it easier to understand.

```haskell
around_ :: (IO () -> IO ()) -> SpecWith a -> SpecWith a
```

Our first argument is a function, which accepts an `IO ()` action and returns another one.
The `IO ()` can be named `runTest`, and it becomes clear how it works:

```Haskell
spec :: Spec
spec = 
    around_ 
        (\runTest -> do
            putStrLn "beginning"
            runTest
            putStrLn "ending"
        ) 
        $ describe "My tests" $ 
```

So, our `IO ()` parameter is our test, and we can do whatever we want around it.

Let's get back to `around`.

```haskell
around :: (ActionWith a -> IO ()) -> SpecWith a -> Spec

around :: ((a -> IO ()) -> IO ()) -> SpecWith a -> SpecWith ()
```

It's really similar, but our `runTest` is now a *function* from `a` to the `IO ()`.
Let's write our user creation/deletion helper with `around`.

```haskell
spec :: Spec
spec = 
    around 
        (\runTest -> do
            db <- createDatabase
            userId <- createUser db
            runTest (db, userId)
            deleteUser (db, userId)
        ) $ describe "With User" $ do
            it "has a user" $ \(db, userId) -> ...

            it "ok ya i get it" $ \(db, userId) -> ...
```

One thing that's neat is that we can use `bracket` style to safely close out resources, too.
Instead of creating a database connection, let's use the `withDatabase` sort of API.

```haskell
spec :: Spec
spec = 
    around 
        (\runTest -> do
            withDatabase $ \db -> do
                userId <- createUser db
                runTest (db, userId)
                deleteUser (db, userId)
        ) $ describe "With User" $ do
            it "has a user" $ \(db, userId) -> ...

            it "ok ya i get it" $ \(db, userId) -> ...
```

Now, if an exception is thrown in the test *or* in the `around` action, the `withDatabase` function gets a chance to clean up the database connection.
Resource safety FTW!

# `aroundWith`

You may have noticed that `around` results in a `Spec`, not a `SpecWith`.
You may have further inferred that there must be an `aroundWith` that lifted that restriction.
There is, and the type signature is a bit scary.

```haskell
aroundWith
    :: (ActionWith a -> ActionWith b) 
    -> SpecWith a
    -> SpecWith b

-- inlining ActionWith type synonym
aroundWith
    :: ((a -> IO ()) -> (b -> IO ())) 
    -> SpecWith a
    -> SpecWith b

-- deleting unnecessary parens
aroundWith
    :: ((a -> IO ()) -> b -> IO ()) 
    -> SpecWith a
    -> SpecWith b
```

The callback to `aroundWith` is intriguing.
The `b` is provided *to us*, and we must provide an `a` to the callback.
That `b` represents the "outer context" of our test suite - the result type, what we're plugging the whole test into.
While the `a` represents the "inner context" of the argument `SpecWith a` that we're passed.
`aroundWith` is saying: "I know how to unify these two contexts."

```haskell
aroundWith $ \runTest outerContext -> do
    innerContext <- createInnerContext outerContext
    runTest innerContext
```

Now, we can rewrite our database creation, user creation, etc to properly delete and create these things.
More importantly - it happens in a composable manner.

```haskell
spec :: Spec
spec = do
    let
        provideDatabase runTest =
            withDatabase $ \db ->
                runTest db

    around provideDatabase $ describe "With Database" $ do
        it "has stuff" ...
        it "okay" ...

        let 
            provideUser runTest db = do
                userId <- createUser db
                runTest (db, userId)
                deleteUser (db, userId)

        aroundWith provideUser $ describe "With User" $ do
            it ...
            it ...
```

We can even use `bracket` internally, to ensure that exceptions are handled neatly.

```haskell
spec :: Spec
spec = do
    let
        provideDatabase runTest =
            withDatabase $ \db ->
                runTest db

    around provideDatabase $ describe "With Database" $ do
        it "has stuff" ...
        it "okay" ...

        let 
            provideUser runTest db = do
                bracket
                    (createUser db)
                    (\userId -> deleteUser (db, userId))
                    (\userId -> runTest (db, userId))

        aroundWith provideUser $ describe "With User" $ do
            it ...
            it ...
```

Finally, if you're just mapping the `a` type, there's `mapSubject`, which lets you modify the type for the underlying items.

```haskell
mapSubject :: (b -> a) -> SpecWith a -> SpecWith b
```

# Hspec Rules

I love writing tests with `hspec`.
Hopefully, you'll enjoy writing fancy composable tests with the library too!
