---
title: "Effectful Property Testing"
date: 2020-03-11
layout: post
categories: programming
---

You're convinced that [Property Based Testing](https://hypothesis.works/articles/what-is-property-based-testing/) is awesome.
You've read about [using PBT to test a screencast editor](https://wickstrom.tech/programming/2019/03/02/property-based-testing-in-a-screencast-editor-introduction.html) and can't wait to do more.
But it's time to write some property tests that integrate with an external system, and suddenly, it's not so easy.

The fantastic [`hedgehog`](https://github.com/hedgehogqa/haskell-hedgehog) library has two "modes" of operation: generating values and making assertions on those values.
I wrote the compatibility library [`hspec-hedgehog`](https://hackage.haskell.org/package/hspec-hedgehog) to allow using `hspec`'s nice testing features with `hedgehog`'s excellent error messages.
But then the time came to start writing property tests against a Postgresql database.

At work, we have a lot of complex SQL queries written both in [`esqueleto`](https://hackage.haskell.org/package/esqueleto) and in raw SQL.
We've decided we want to increase our software quality by writing tests against our database code.
While both Haskell and SQL are declarative and sometimes obviously correct, it's not always the case.
Writing property tests would help catch edge cases and prevent bugs from getting to our users.

# IO Tests

It's considered good practice to model tests in three separate phases:

1. Arrange
2. Act
3. Assert

This works really well with property based testing, especially with `hedgehog`.
We start by generating the data that we need.
Then we call some function on it.
Finally we assert that it should have some appropriate shape:

```haskell
spec :: Spec
spec = describe "some property" $ do
    it "works" $ hedgehog $ do
        value <- someGenerator
        let result = someFunction value
        result === someExpectedValue
```

It's relatively straightforward to call functions in IO.
`hedgehog` provides a function [`evalIO`](https://www.stackage.org/haddock/lts-15.3/hedgehog-1.0.2/Hedgehog.html#v:evalIO) that lets you run arbitrary IO actions and still receive good error messages.

```haskell
spec :: Spec
spec = describe "some IO property" $ do
    it "works" $ hedgehog $ do
        value <- someGenerator
        result <- evalIO $ someFunction value
        result === someExpectedValue
```

For very simple tests like this, this is fine.
However, it becomes cumbersome quite quickly when you have a lot of values you want to make assertions on.

```haskell
spec :: Spec
spec = describe "some IO property" $ do
    it "works" $ hedgehog $ do
        value0 <- someGenerator0
        value1 <- someGenerator1
        value2 <- someGenerator2

        (a, b, c, d, e) <- evalIO $ do
            prepare value0
            prepare value1
            prepare value2

            a <- someFunction

            alterState

            b <- someFunction
            c <- otherFunction
            d <- anotherFunction
            e <- comeOnReally

            pure (a, b, c, d, e)

        a === expectedA
        diff a (<) b
        c === expectedC
        d /== anyNonDValue
```

This pattern becomes unweildy for a few reasons:

1. It's awkward to have to `pure` up a tuple of the values you want to assert against.
2. It's repetitive to declare bindings twice for all the values you want to assert against.
3. Modifying a return means adding or removing items from the tuple, which can possibly be error-prone.

Fortunately, we can do better.

# pure on pure on pure

Instead of returning values to a different scope, and then doing assertions against those values, we will return an *action* that does assertions, and then call it.
The simple case is barely changes:

```haskell
spec :: Spec
spec = describe "some simple IO property"
    it "works" $ hedgehog $ do
        value <- someGenerator

        assertions <- evalIO $ do
            result <- someFunction value
            pure $ do
                result === expectedValue

        assertions
```

An astute student of monadic patterns might notice that:

```haskell
foo = do
    result <- thing
    result
```

is equivalent to:

```haskell
foo = do
    join thing
```

and then simplify:


```haskell
spec :: Spec
spec = describe "some simple IO property"
    it "works" $ hedgehog $ do
        value <- someGenerator

        join $ evalIO $ do
            result <- someFunction value
            pure $ do
                result === expectedValue
```

Nice!

Because we're returning an *action* of assertions instead of *values* that will be asserted against, we don't have to play any weird games with names or scopes.
We've got all the values we need in scope, and we make assertions, and then we defer returning them.
Let's refactor our more complex example:

```haskell
spec :: Spec
spec = describe "some IO property" $ do
    it "works" $ hedgehog $ do
        value0 <- someGenerator0
        value1 <- someGenerator1
        value2 <- someGenerator2

        join $ evalIO $ do
            prepare value0
            prepare value1
            prepare value2

            a <- someFunction

            alterState

            b <- someFunction
            c <- otherFunction
            d <- anotherFunction
            e <- comeOnReally

            pure $ do 
                a === expectedA
                diff a (<) b
                c === expectedC
                d /== anyNonDValue
```

On top of being more convenient and easy to write, it's more difficult to do the wrong thing here.
You can't accidentally swap two names in a tuple, because there is no tuple!

# A Nice API

We can write a helper function that does some of the boilerplate for us:

```haskell
arrange :: PropertyT IO (IO (PropertyT IO a)) -> PropertyT IO a
arrange mkAction = do
    action <- mkAction
    join (evalIO action)
```

Since we're feeling cute, let's also write some helpers that'll make this pattern more clear:

```haskell
act :: IO (PropertyT IO a) -> PropertyT IO (IO (PropertyT IO a))
act = pure

assert :: PropertyT IO a -> IO (PropertyT IO a)
assert = pure
```

And now our code sample looks quite nice:

```haskell

spec :: Spec
spec = describe "some IO property" $ do
    it "works" $ 
        arrange $ do
            value0 <- someGenerator0
            value1 <- someGenerator1
            value2 <- someGenerator2

            act $ do
                prepare value0
                prepare value1
                prepare value2

                a <- someFunction

                alterState

                b <- someFunction
                c <- otherFunction
                d <- anotherFunction
                e <- comeOnReally

                assert $ do 
                    a === expectedA
                    diff a (<) b
                    c === expectedC
                    d /== anyNonDValue
```

# Beyond IO

It's not enough to just do `IO`.
The problem that motivated this research called for `persistent` and `esqueleto` tests against a Postgres database.
These functions operate in `SqlPersistT`, and we use database transactions to keep tests fast by rolling back the commit instead of finalizing.

Fortunately, we can achieve this by passing an "unlifter":

```haskell
arrange 
    :: (forall x. m x -> IO x)
    -> PropertyT IO (m (PropertyT IO a))
    -> PropertyT IO a
arrange unlift mkAction = do
    action <- mkAction
    join (evalIO (unlift action))

act :: m (PropertyT IO a) -> PropertyT IO (m (PropertyT IO a))
act = pure

assert :: Applicative m => PropertyT IO a -> m (PropertyT IO a)
assert = pure
```

With these helpers, our database tests look quite neat.

```haskell
spec :: SpecWith TestDb
spec = describe "db testing" $ do
    it "is neat" $ \db -> 
        arrange (runTestDb db) $ do
            entity0 <- forAll generateEntity0
            entityList <- forAll
                $ Gen.list (Range.linear 1 100)
                $ generateEntityChild

            act $ do
                insert entity0
                before <- someDatabaseFunction
                insertMany entityList
                after <- someDatabaseFunction


                assert $ do
                    before === 0
                    diff before (<) after
                    after === length entityList
```

# On Naming Things

No, I'm not going to talk about [*that* kind of naming things](https://www.parsonsmatt.org/2017/06/23/on_naming_things.html).
This is about actually giving names to things!

The most general types for `arrange`, `act`, and `assert` are:

```haskell
act, assert :: Applicative f => a -> f a
act = pure
assert = pure

arrange 
    :: Monad m
    => (forall x. n x -> m x)
    -> m (n (m a)) -> m a
arrange transform mkAction = do
    action <- mkAction
    join (transform action)
```

These are pretty ordinary and unassuming functions.
They're so general.
It can be hard to see all the ways they can be useful.

Likewise, if we only ever write the direct functions, then it can be difficult to capture the pattern and make it obvious in our code.

Giving a thing a name makes it *real* in some sense.
In the Haskell sense, it becomes a value you can link to, provide Haddocks for, and show examples on.
In our work codebase, the equivalent functions to the `arrange`, `act`, and `assert` defined here have nearly 100 lines of documentation and examples, as well as more specified types that can help guide you to the correct implementation.

Sometimes designing a library is all about *narrowing* the potential space of things that a user can do with your code.
