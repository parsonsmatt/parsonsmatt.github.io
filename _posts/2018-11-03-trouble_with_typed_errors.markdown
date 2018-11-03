---
title: "The Trouble with Typed Errors"
date: 2018-11-03
layout: post
categories: programming
---

You, like me, program in either Haskell, or Scala, or F#, or Elm, or PureScript, and you don't like runtime errors.
They're awful and nasty!
You have to debug them, and they're not represented in the types.
Instead, we like to use `Either` (or something isomorphic) to represent stuff that might fail:

```haskell
data Either l r = Left l | Right r
```

`Either` has a `Monad` instance, so you can short-circuit an `Either l r` computation with an `l` value, or bind it to a function on the `r` value.

So, we take our unsafe, runtime failure functions:

```haskell
head   :: [a] -> a
lookup :: k -> Map k v -> v
parse  :: String -> Integer
```

and we use informative error types to represent their possible failures:

```haskell
data HeadError = ListWasEmpty

head :: [a] -> Either HeadError a

data LookupError = KeyWasNotPresent

lookup :: k -> Map k v -> Either LookupError v

data ParseError 
    = UnexpectedChar Char String
    | RanOutOfInput

parse :: String -> Either ParseError Integer
```

Except, we don't really use types like `HeadError` or `LookupError`.
There's only one way that `head` or `lookup` could fail.
So we just use `Maybe` instead.
`Maybe a` is just like using `Either () a` -- there's only one possible `Left ()` value, and there's only one possible `Nothing` value.
(If you're unconvinced, write `newtype Maybe a = Maybe (Either () a)`, derive all the relevant instances, and try and detect a difference between this `Maybe` and the stock one).

But, `Maybe` isn't great -- we've lost information!
Suppose we have some computation:

```haskell
foo :: String -> Maybe Integer
foo str = do
    c <- head str
    r <- lookup str strMap
    eitherToMaybe (parse (c : r))
```

Now, we try it on some input, and it gives us `Nothing` back.
Which step failed?
We actually can't know that!
All we can know is that *something* failed.

So, let's try using `Either` to get more information on what failed.
Can we just write this?

```haskell
foo :: String -> Either ??? Integer
foo str = do
    c <- head str
    r <- lookup str strMap
    parse (c : r)
```

Unfortunately, this gives us a type error.
We can see why by looking at the type of `>>=`:

```haskell
(>>=) :: (Monad m) => m a -> (a -> m b) -> m b
```

The type variable `m` must be an instance of `Monad`, and the type `m` must be *exactly the same* for the value on the left and the function on the right.
`Either LookupError` and `Either ParseError` are not the same type, and so this does not type check.

Instead, we need some way of accumulating these possible errors.
We'll introduce a utility function `mapLeft` that helps us:

```haskell
mapLeft :: (l -> l') -> Either l r -> Either l' r
mapLeft f (Left l) = Left (f l)
mapLeft _ r = r
```

Now, we can combine these error types:

```haskell
foo :: String 
    -> Either 
        (Either HeadError (Either LookupError ParseError)) 
        Integer
foo str = do
    c <- mapLeft Left (head str)
    r <- mapLeft (Right . Left) (lookup str strMap)
    mapLeft (Right . Right) (parse (c : r))
```

There! Now we can know exactly how and why the computation failed.
Unfortunately, that type is a bit of a monster.
It's verbose and all the `mapLeft` boilerplate is annoying.

At this point, most application developers will create a "application error" type, and they'll just shove everything that can go wrong into it.

```haskell
data AllErrorsEver
     = AllParseError ParseError
     | AllLookupError LookupError
     | AllHeadError HeadError
     | AllWhateverError WhateverError
     | FileNotFound FileNotFoundError
     | etc...
```

Now, this slightly cleans up the code:

```haskell
foo :: String -> Either AllErrorsEver Integer
foo str = do
    c <- mapLeft AllHeadError (head str)
    r <- mapLeft AllLookupError (lookup str strMap)
    mapLeft AllParseError (parse (c : r))
```

However, there's a pretty major problem with this code.
`foo` is claiming that it can "throw" all kinds of errors -- it's being honest about parse errors, lookup errors, and head erors, but it's also claiming that it will throw if files aren't found, "whatever" happens,  and `etc`.
There's no way that a call to `foo` will result in `FileNotFound`, because `foo` can't even do `IO`!
It's absurd.
The type is too large!
And I have [written about keeping your types small]({% post_url 2018-10-02-small_types %}) and how wonderful it can be for getting rid of bugs.

Suppose we want to handle `foo`'s error.
We call the function, and then write a case expression like good Haskellers:

```haskell
case foo "hello world" of
    Right val -> 
        pure val
    Left err -> 
        case err of
            AllParseError parseError ->
                handleParseError parseError
            AllLookupError lookupErr ->
                handleLookupError
            AllHeadError headErr ->
                handleHeadError
            _ ->
                error "impossible?!?!?!"
```

Unfortunately, this code is brittle to refactoring!
We've claimed to handle all errors, but we're really not handling many of them.
We currently "know" that these are the only errors that can happen, but there's no compiler guarantee that this is the case.
Someone might later modify `foo` to throw another error, and this case expression will break.
Any case expression that evaluates any result from `foo` will need to be updated.

The error type is too big, and so we introduce the possibility of mishandling it.
There's another problem.
Let's suppose we know how to handle a case or two of the error, but we must pass the rest of the error cases upstream:

```haskell
bar :: String -> Either AllErrorsEver Integer
bar str = 
    case foo str of
        Right val -> Right val
        Left err ->
            case err of
                AllParseError pe ->
                    Right (handleParseError pe)
                _ ->
                    Left err
```

We *know* that `AllParseError` has been handled by `bar`, because -- just look at it!
However, the compiler has no idea.
Whenever we inspect the error content of `bar`, we must either a) "handle" an error case that has already been handled, perhaps dubiously, or b) ignore the error, and desperately hope that no underlying code ever ends up throwing the error.

Are we done with the probles on this approach?
No!
There's no guarantee that I throw the *right* error!

```haskell
head :: [a] -> Either AllErrorsEver a
head (x:xs) = Right x
head [] = Left (AllLookupError KeyWasNotPresent)
```

This code typechecks, but it's *wrong*, because `LookupError` is only supposed to be thrown by `lookup`!
It's obvious in this case, but in larger functions and codebases, it won't be so clear.

# Monolithic error types are bad

So, having a monolithic error type has a ton of problems.
I'm going to make a claim here:

> All error types should have a single constructor

That is, no sum types for errors.
How can we handle this?

Let's maybe see if we can make `Either` any nicer to use.
We'll define a few helpers:

```haskell
type (+) = Either
infixr + 5

l :: l -> Either l r
l = Left

r :: r -> Either l r
r = Right
```

Now, let's refactor that uglier `Either` code with these new helpers:

```haskell
foo :: String 
    -> Either 
        (HeadError + LookupError + ParseError) 
        Integer
foo str = do
    c <- mapLeft l (head str)
    r <- mapLeft (r . l) (lookup str strMap)
    mapLeft (r . r) (parse (c : r))
```

Well, the syntax is nicer.
We can `case` over the nested Either in the error branch to eliminate single error cases.
It's easier to ensure we don't claim to throw errors we don't -- after all, GHC will correctly infer the type of `foo`, and if GHC infers a type variable for any `+`, then we can assume that we're not using that error slot, and can delete it.

Unfortuntaly, there's still the `mapLeft` boilerplate.
And expressions which you'd *really* want to be equal, aren't --

```haskell
x :: Either (HeadError + LookupError) Int
y :: Either (LookupError + HeadError) Int
```

The values `x` and `y` are *isomorphic*, but we can't use them in a `do` block because they're not exactly equal.
If we add errors, then we must revise *all* `mapLeft` code, as well as all `case` expressions that inspect the errors.
Fortunately, these are entirely compiler-guided refactors, so the chance of messing them up is small.
However, they contribute significant boilerplate, noise, and busywork to our program.

# Boilerplate be gone!

Well, turns out, we *can* get rid of the order dependence and boilerplate with type classes!
The most powerful approach is to use "classy prisms" from the `lens` package.
Let's translate our types from concrete values to prismatic ones:

```haskell
-- Concrete:
head :: [a] -> Either HeadError a

-- Prismatic:
head :: AsHeadError err => [a] -> Either err a


-- Concrete:
lookup :: k -> Map k v -> Either LookupError v

-- Prismatic:
lookup 
    :: (AsLookupError err) 
    => k -> Map k v -> Either err v
```

Now, type class constraints don't care about order -- `(Foo a, Bar a) => a` and `(Bar a, Foo a) => a` are exactly the same thing as far as GHC is concerned.
The `AsXXX` type classes will *automatically* provide the `mapLeft` stuff for us, so now our `foo` function looks a great bit cleaner:

```haskell
foo :: (AsHeadError err, AsLookupError err, AsParseError err)
    => String -> Either err Integer
foo str = do
    c <- head str
    r <- lookup str strMap
    parse (c : r)
```

This appears to be a significant improvement over what we've had before!
And, most of the boilerplate with the `AsXXX` classes is taken care of via Template Haskell:

```haskell
makeClassyPrisms ''ParseError
-- this line generates the following:

class AsParseError a where
    _ParseError :: Prism' a ParseError
    _UnexpectedChar :: Prism' a (Char, String)
    _RanOutOfInput :: Prism' a ()

instance AsParseError ParseError where
    -- etc...
```

However, we do have to write our own boilerplate when we eventually want to concretely handle these ttypes.
We may end up writing a huge `AppError` that all of these errors get injected into.

There's one major, fatal flaw with this approach.
While it composes very nicely, it doesn't decompose at all!
There's no way to catch a single case and ensure that it's handled.
The machinery that prisms give us don't allow us to separate out a single constraint, so we can't pattern match on a single error.

Once again, our types become ever larger, with all of the problems that entails.

# Generics to the rescue!

What we *really* want is:

- Order independence
- No boilerplate
- Easy composition
- Easy decomposition

In PureScript or OCaml, you can use open variant types to do this flawlessly.
Haskell doesn't have open variants, and the attempts to mock them end up quite clumsy to use in practice.

I'm happy to say that the entire job is handled quite nicely with the amazing [`generic-lens`](https://hackage.haskell.org/package/generic-lens) package.
I created [a gist](https://gist.github.com/parsonsmatt/880fbf79eaad6ed863786c6c02f8ddc9) that demonstrates their usage, but the *magic* comes down to this simple fact: there's an instance of the prismatic `AsType` class for `Either`, which allows you to "pluck" a constraint off.
This satisfies all of the things I wanted in my list, and we can consider representing errors mostly solved.

# Mostly?

Well, `ExceptT e IO a` still imposes a significant runtime performance hit, and asynchronous exceptions aren't considered here.
A bifunctor IO type like `newtype BIO err a = BIO (IO a)` which carries the type class constraints of the errors it contains is promising, but I haven't been able to write a satisfying interface to this yet.

I also haven't used this technique in a large codebase yet, and I don't know how it scales.
And the technique does require you to be comfortable with `lens`, which is a fairly high bar for training new folks on.
I'm sure that API improvements could be made to make this style more accessible and remove some of the lens knowledge prerequisites.
