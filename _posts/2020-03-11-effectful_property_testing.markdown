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

This pattern becomes unwieldy for a few reasons:

1. It's awkward to have to `pure` up a tuple of the values you want to assert against.
2. It's repetitive to declare bindings twice for all the values you want to assert against.
3. Modifying a return means adding or removing items from the tuple, which can possibly be error-prone.

Fortunately, we can do better.

# pure on pure on pure

Instead of returning values to a different scope, and then doing assertions against those values, we will return an *action* that does assertions, and then call it.
The simple case barely changes:

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

# A Real Example

OK, OK, so that last one was too abstract.
Let's say we're writing a billing system (jeez, i sure do use that example a lot).
We keep track of `Invoice`s, which group `InvoiceLineItem`s that contain actual amounts.
We have a model for `Payment`s, which record details on a `Payment` like how it was made, who made it, whether it was successful, etc.
A `Payment` can be applied to many `Invoice`s, so we have a join table `InvoicePayment` that records the amount of each payment allocated toward an `Invoice`.

Neat.

Because we *love* Postgresql, quite a bit of our business logic is performed database-side, either via custom SQL functions or `esqueleto` expressions.
One of these functions is `invoicePaidTotal`, which tells us the total amount paid towards an `Invoice`.

Here's the `esqueleto` code:

```haskell
invoicePaidTotal
  :: SqlExpr (Entity DB.Invoice)
  -> SqlExpr (Value (Dollar E2))
invoicePaidTotal i =
  fromMaybe_ zeroDollars $
    subSelect . from $ \(ip `InnerJoin` p) -> do
      on (p ^.DB.PaymentId ==. ip ^.DB.InvoicePaymentPaymentId)
      where_ (ip ^.DB.InvoicePaymentInvoiceId ==. i ^.DB.InvoiceId)
      where_ (Payment.isSucceeded p)
      pure (sumDollarDefaultZero (ip ^.DB.InvoicePaymentTotal))
```

(Some of these functions are internal to the work codebase, but they do the obvious thing)

This is equivalent to the following SQL:

```sql
SELECT 
    COALESCE(SUM(ip.total), 0)
FROM invoice_payment AS ip
INNER JOIN payment AS p
    ON p.id == ip.payment_id
WHERE ip.invoice_id = :invoice_id
  AND payment_succeeded(p)
```

Now, we want to write a test for it.
Upon inspection, we're testing two things: SQL's `SUM` function and the `payment_succeeded` function (which itself is actually another esqueleto expression that would unfold).

So, we can write a property:

- If there are no `Payment`s or `InvoicePayment`s in the database, then this function should return `$0.00`.
- If there are some `InvoicePayment`s in the database, then this function should return the sum of their `total` fields provided that the associated `Payment` is successful.

Here's the test code.
We'll start by looking at `arrange` bit, which creates the database models.

```haskell
arrange (runTestDb db) "invoicePaidTotal" $ do
    let invoiceId = InvoiceKey "invoice"
        invoice = baseInvoice

    payments <- 
        forAll $
        Gen.list (Range.linear 1 50) $ do
            id <- Gen.id
            name <- Gen.faker Faker.name
            pure $ Entity id basePayment
                { paymentName = name
                }

    invoicePayments <-
        forAll $
        for payments $ (Entity paymentId _) -> do
            amount <- Gen.integral (Range.linear 1 1000)
            pure $ InvoicePayment invoiceId paymentId amount

    act $ do
        ... snip ...
```

Values like `baseInvoice`, `basePayment` are useful as test fixtures.
I've generally found that writing generators for models isn't nearly as useful as generating *modifications* to models that alter what you care about.
This doesn't catch as many potential edge case bugs, so it has it's downsides, but if the client name being "Foobar" instead of "AsdfQuux" affects payment totals, then something is deeply weird.

Alright, let's `act`!
I usually like to define the function under test as `subject`, along with whatever scaffolding needs to happen to make it easy to call.
In this case, I want to test an `esqueleto` `SqlExpr`, which means I need convert it into a query and run it.
Calling it `subject` is just an aesthetic thing.

```haskell
    act $ do
        let subject =
                fmap (unValue . head)
                select $
                from $ \i -> do
                where_ $ i ^. InvoiceId ==. val invoiceId
                pure $ invoicePaidTotal i
```

I call `head` fearlessly here because I don't care about runtime errors in test suites.
YOLO.

Next, we're going to insert our invoice, and call `subject` to get the paid total.

```haskell
        insertKey invoiceId invoice

        beforePayments <- subject
```

Then we'll mutate the state of the database by inserting all the invoice payments and invoices.

```haskell
        insertEntityMany payments
        insertMany invoicePayments

        afterPayments <- subject
```

And that's all we need to start writing some assertions.

```haskell
        assert $ do
            beforePayments === 0

            afterPayments === do
                map invoicePaymentTotal
                $ filter isSuccessfulPayment
                $ invoicePayments
```

`isSuccessfulPayment` is a Haskell function that mirrors the logic in the SQL.
If this test passes, then we know that the logic is all set.

Next up, we might want to write an *equivalence test* for the Haskell `isSuccessfulPayment` and the esqueleto/SQL `Payment.isSuccessful`.
This would look something like:

```haskell
arrange (runTestDb db) $ do
    Entity paymentId payment <- forAll Payment.gen
    act $ do
        insertKey paymentId payment

        dbPaymentSuccessful <-
            fmap (unValue . head) $
            select $
            from $ \p -> do
            where_ $ p ^. PaymentId ==. val paymentId
            pure (Payment.isSuccessful p)

        assert $ do
            dbPaymentSuccessful 
                === 
                paymentIsSuccessful payment

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
