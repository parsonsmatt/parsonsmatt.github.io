---
title: "RankNTypes via Lambda Calculus"
date: 2021-11-30
layout: post
categories: programming
---

`RankNTypes` is a language extension in Haskell that allows you to write *even more* polymorphic programs.
The most basic explanation is that it allows the *implementer* of a function to pick a type, rather than the *caller* of the function.
A very brief version of this explanation follows:

# The Typical Explanation

Consider the identity function, or `const`:

```haskell
id :: a -> a
id x = x

const :: a -> b -> a
const a b = a
```

These functions work for any types that the *caller* of the function picks.
Which means that, as *implementers*, we can't know anything about the types involved.

Let's say we want to apply a function to each element in a tuple.
Without a type signature, we can write:

```haskell
applyToBoth f (a, b) = (f a, f b)
```

Vanilla Haskell will provide this type:

```haskell
applyToBoth :: (a -> a) -> (a, a) -> (a, a)
```

This is a perfectly useful type, but what if we want to apply it to a tuple containing two different types?
Well, we can't do anything terribly *interesting* with that - if we don't know anything about the type, the only thing we can provide is `id`.

```haskell
applyToBoth :: (forall x. x -> x) -> (a, b) -> (a, b)
applyToBoth f (a, b) = (f a, f b)
```

And that `forall x` inside of a parentheses is a `RankNType`.
It allows the implementer of the function to select the type that the function will be used at, and the *caller* of the function must provide something *sufficiently polymorphic*.

This explanation is a bit weird and difficult, even though it captures the basic intuition.
It's not super obvious *why* the caller or the implementer gets to pick types, though.
Fortunately, by leveraging the lambda calculus, we can make this more precise!

# Whirlwind Tour of Lambda

Feel free to skip this section if you're familiar with the lambda calculus.
We're going to work from untyped, to simply typed, and finally to the polymorphic lambda calculus.
This will be sufficient for us to get a feeling for what `RankNTypes` are.

## Untyped Lambda Calculus

The untyped lambda calculus is an extremely simple programming language with three things:

1. Variables 
2. Anonymous Functions (aka lambdas)
3. Function Application 

This language is Turing complete, surprisingly.
We'll use Haskell syntax, but basically, you can write things like:

```
id = \x -> x

const = \a -> \b -> a

apply = \f -> \a -> f a
```

## Simply Typed Lambda Calculus

The *simply typed* lambda calculus adds an extremely simple type system to the untyped lambda calculus.
All terms must be given a type, and we will have a pretty simple type system - we'll only have `Unit` and function arrows.
A lambda will always introduce a function arrow, and a function application always eliminates it.

```
id :: Unit -> Unit
id = \(x :: Unit) -> x

idFn :: (Unit -> Unit) -> (Unit -> Unit)
idFn = \(f :: Unit -> Unit) -> f

const :: Unit -> Unit -> Unit
const = \(a :: Unit) -> \(b :: Unit) -> a

apply :: (Unit -> Unit) -> Unit -> Unit
apply = \(f :: Unit -> Unit) -> \(a :: Unit) -> f a
```

This is a much less powerful programming language - it is not even Turing Complete.
This *makes sense* - type systems forbid certain valid programs that are otherwise syntactically valid.

The type system in this is only capable of referring to the constants that we provide.
Since we only have `Unit` and `->` as valid type constants, we have a super limited ability to write programs.
We can still do quite a bit - natural numbers and Boolean types are perfectly expressible, but many higher order combinators are impossible.

Let's add polymorphic types.

## Polymorphic Lambda Calculus

The magic of the lambda calculus is that we have a *means of introducing variables*.
The problem of the simply typed lambda calculus is that we don't have variables.
So we can introduce *type variables*.

Like Haskell, we'll use `forall` to introduce type variables.
In a type signature, the syntax will be the same.
However, unlike Haskell, we're going to have *explicit* type variable application and introduction at the *value level* as well.

Let's write `id` with our new explicit type variables.

```
id :: forall a. a -> a
id = forall a. \(x :: a) -> x
```

Let's write `const` and `apply`.

```
const :: forall a. forall b. a -> b -> a
const = forall a. forall b. \(x :: a) -> \(y :: b) -> x

apply :: forall a. forall b. (a -> b) -> a -> b
apply = forall a. forall b. \(f :: a -> b) -> \(x :: a) -> f x
```

Finally, let's apply some type variables.

```
constUnit :: Unit -> Unit -> Unit
constUnit = 
    const @Unit @Unit 

idUnitFn :: (Unit -> Unit) -> (Unit -> Unit)
idUnitFn = 
    id @(Unit -> Unit) f

idReturnUnitFn :: forall a. (a -> Unit) -> (a -> Unit)
idReturnUnitFn =
    forall a. id @(a -> Unit)

constUnitFn :: Unit -> (Unit -> Unit) -> Unit
constUnitFn = 
    const @Unit @(Unit -> Unit)
```

We're passing types to functions.
With all of these simple functions, the *caller* gets to provide the type.
If we want the *implementer* to provide a type, then we'd just put the `forall` inside a parentheses.
Let's look at the `applyBoth` from above.
This time, we'll have explicit type annotations and introductions!

```
applyBoth 
    :: forall a. forall b. (forall x. x -> x) -> (a, b) -> (a, b)
applyBoth =
    forall a. forall b.           -- [1]
    \(f :: forall x. x -> x) ->   -- [2]
    \((k, h) :: (a, b)) ->        -- [3]
        (f @a k, f @b h)          -- [4]
``` 

There's a good bit going on here, so let's break it down on a line-by-line basis.

1. Here, we're introducing our type variables `a` and `b` so that we can refer to them in the type signatures of our variables, and apply them to our functions.
2. Here, we're introducing our first value parameter - the function `f`, which itself has a type that accepts a type variable.
3. Now, we're accepting our second value parameter - a tuple `(k, h) :: (a, b)`. We can refer to `a` and `b` in this signature because we've introduced them in step 1.
4. Finally, we're supplying the type `@a` to our function `f` in the left hand of the tuple, and the type `@b` to the type in the right. This allows our types to check.

Let's see what it looks like to *call* this function.
To give us some more interesting types to work with, we'll include `Int` and `Bool` literals.

```
foo :: (Int, Bool)
foo = 
    applyBoth 
        @Int @Bool 
        _f 
        (3, True)
```

We haven't decided what `_f` will look like exactly, but the type of the value is `forall x. x -> x`.
So, syntactically, we'll introduce our type variable, then our value-variable:

```
foo :: (Int, Bool)
foo = 
    applyBoth 
        @Int @Bool 
        (forall x. \(a :: x) -> (_ :: x))
        (3, True)
```

As it happens, the only value we can possibly plug in here is `a :: x` to satisfy this.
We know absolutely nothing about the type `x`, so we cannot do anything with it.

```
foo :: (Int, Bool)
foo = 
    applyBoth 
        @Int @Bool 
        (forall x. \(a :: x) -> a)
        (3, True)
```

# Tug of War

`applyBoth` is an awful example of `RankNTypes` because there's literally nothing useful you can do with it.
The reason is that we don't give the *caller* of the function any options!
By giving the caller of the function *more information*, they can do more useful and interesting things with the results.

This mirrors the guarantee of parametric polymorphism.
The less that we know about our inputs, the less we can do with them - until we get to types like `const :: a -> b -> a` where the implementation is completely constrained.

What this means is that we provide, as *arguments to the callback function*, more information!

Let's consider this other type:

```haskell
applyBothList :: (forall x. [x] -> Int) -> ([a], [b]) -> (Int, Int)
applyBothList f (as, bs) = 
    (f as, f bs)
```

Now the function knows a good bit more: we have a *list* as our input (even if we don't know anything aobut the type), and the output is an `Int`.
Let's translate this to our polymorphic lambda calculus.

```haskell
applyBothList =
    forall a. forall b.
    \(f :: forall x. [x] -> Int) ->
    \( as :: [a], bs :: [b] ) ->
    ( f @a as, f @b bs )
```

When we call this function, this is what it looks like:

```haskell
    applyBothList
        @Int @Char
        (forall x. \(xs :: [x]) -> length @x xs * 2)
        ( [1, 2, 3], ['a', 'b', 'c', 'd'] )
```

# Constraints?

In Haskell, a type class constraint is elaborated into a record-of-functions that is indexed by the type.

```haskell
class Num a where
    fromInteger :: Integer -> a
    (+) :: a -> a -> a
    (*) :: a -> a -> a
    (-) :: a -> a -> a
    -- etc...

-- under the hood, this is the same thing:

data NumDict a = NumDict
    { fromInteger :: Integer -> a
    , (+) :: a -> a -> a
    , (-) :: a -> a -> a
    , (*) :: a -> a -> a
    }
```

When you have a function that accepts a `Num a` argument, GHC turns it into a `NumDict a` and passes it explicitly.

```haskell
-- Regular Haskell:
square :: Num a => a -> a
square a = a * a

-- What hapens at runtime:
square :: NumDict a -> a -> a
square NumDict {..} a = a * a
```

Or, for a simpler variant, let's consider `Eq`.

```haskell
-- Regular Haskell:
class Eq a where
    (==) :: a -> a -> Bool

-- Runtime dictionary:
newtype EqDict a = EqDict { (==) :: a -> a -> Bool }

-- Regular:
allEqual :: (Eq a) => a -> a -> a -> Bool
allEqual x y z =
    x == y && y == z && x == z

-- Runtime dictionary:
allEqual :: EqDict a -> a -> a -> a -> Bool
allEqual (EqDict (==)) x y z =
    x == y && y == z && x == z
```

(Note that binding a variable name to an operator is perfectly legal!)

One common way to extend the power or flexibility of a `RankNTypes` program is to include allowed constraints in the callback function.
Knowing how and when things come into scope can be tricky, but if we remember our polymorphic lambda calculus, this becomes easy.

Consider this weird signature:

```haskell
weirdNum :: (forall a. Num a => a) -> String
weirdNum someNumber = 
    show (someNumber @Int)
```

This isn't *exactly* a function.
What sort of things can we call here?

Well, we have to *produce* an `a`.
And we know that we have a `Num a` constraint.
This means we can call `fromInteger :: Integer -> a`.
And, we can also use any other `Num` methods - so we can add to it, double it, square it, etc.

So, calling it might look like this:

```haskell
main = do
    putStrLn $ weirdNum (fromInteger 3 + 6 * 2)
```

Let's elaborate this to our lambda calculus.
We'll convert the type class constraint into an explicit dictionary, and then everything should work normally.

```
weirdNum =
    \(number :: forall a. NumDict a -> a) ->
        show @Int intShowDict(number @Int intNumDict)
```

Now, let's call this:

```
    weirdNum 
        ( forall a. 
        \(dict :: NumDict a) -> 
            fromInteger dict 3
        )
```

# More on the Lambda Calculus

If you've found this elaboration interesting, you may want to consider reading [Type Theory and Formal Proof](https://www.amazon.com/Type-Theory-Formal-Proof-Introduction/dp/110703650X).
This book is extremely accessible, and it taught me almost everything I know about the lambda calculus.
