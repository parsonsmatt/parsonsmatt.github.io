---
title: "An Elegant Fizzbuzz"
date: 2016-02-27
layout: post
categories: programming
---

Fizzbuzz is a notorious programming problem to give during interviews.
It's designed to weed out people that can't program at all.
The problem formulation is:

> Print the numbers 1 to 100.
If the number is a multiple of 3, then print "Fizz" instead of the number.
If the number is a multiple of 5, then print "Buzz" instead of the number.
If the number is divisible by both 3 and 5, then print "FizzBuzz" instead.

The basic implementation of the problem looks like this:

```java
class Fizz {
    public static void fizzBuzz() {
        for (int i = 1; i <= 100; i++) {
            if (i % 3 == 0 && i % 5 == 0) {
                System.out.println("FizzBuzz");
            } else if (i % 3 == 0) {
                System.out.println("Fizz");
            } else if (i % 5 == 0) {
                System.out.println("Buzz");
            } else {
                System.out.println(i);
            }
        }
    }
}
```

Which we can translate directly to Haskell:

```haskell
fizzBuzz :: IO ()
fizzBuzz =
    forM_ [1..100] (\i ->
        if i `mod` 3 == 0 && i `mod` 5 == 0 then
            putStrLn "FizzBuzz"
        else if i `mod` 3 == 0 then
            putStrLn "Fizz"
        else if i `mod` 5 == 0 then
            putStrLn "Buzz"
        else
            print i
    )
```

If the candidate is capable of answering it easily, then the spec gets increased:

> Now if it is divisible by 7, print "Quux".
> If it's divisible by a combination, then it needs to print all the words.

Uh oh! These prime factors are no fun.
There's going to be an additional `if` case for each prime factor, and then you'll need an additional `if` case for each possible combination.
It's much simpler if we can handle the logic without worrying about the duplication.

```java
class Fizz {
    public static void fizzBuzz() {
        for (int i = 1; i < 101; i++) {
            StringBuilder s = new StringBuilder();
            if (i % 3 == 0) {
                s.append("Fizz");
            }
            if (i % 5 == 0) {
                s.append("Buzz");
            }
            if (i % 7 == 0) {
                s.append("Quux");
            }
            if (s.length() == 0) {
                s.append(i);
            }
            System.out.println(s.toString());
        }
    }
}
```

We've reduced the logic duplication by accumulating state.
In Haskell, we'd use the State monad:

```haskell
fizzBuzz :: (MonadIO m, MonadState String m) => m ()
fizzBuzz =
    forM_ [1..100] (\i -> do
        put ""
        when (i `mod` 3 == 0) (modify (++"Fizz"))
        when (i `mod` 5 == 0) (modify (++"Buzz"))
        when (i `mod` 7 == 0) (modify (++"Quux"))
        str <- get
        when (null str) (put (show i))
        get >>= liftIO . putStrLn
    )
```

This implementation has the unfortunate problem of being somewhat inefficient:
those `++` calls are $O(n)$, and we'll have to traverse the whole string each time we add something to the end.

This solves the "extensibility" problem, but there's another issue: the printing is tied in with the generation of the strings.
Let's convert this to a map -- that will both solve the performance issue noted above as well as making it less difficult to observe what's happening.
In Java, we've got:

```java
class Fizz {
    public static String fizzLogic(int i) {
        StringBuilder s = new StringBuilder();
        if (i % 3 == 0) {
            s.append("Fizz");
        }
        if (i % 5 == 0) {
            s.append("Buzz");
        }
        if (i % 7 == 0) {
            s.append("Quux");
        }
        if (s.length() == 0) {
            s.append(i);
        }

        return s.toString();
    }

    public static List<String> fizzBuzz() {
        return IntStream
            .range(1, 101)
            .mapToObj(Fizz::fizzLogic)
            .collect(ArrayList::new, ArrayList::add, ArrayList::addAll);
    }
}
```

And, in Haskell:

```haskell
fizzBuzz :: [Integer] -> [String]
fizzBuzz = map fizzLogic

fizzLogic :: Integer -> String
fizzLogic i =
    if i `mod` 3 == 0 && i `mod` 5 == 0 then
        "FizzBuzz"
    else if i `mod` 3 == 0 then
        "Fizz"
    else if i `mod` 5 == 0 then
        "Buzz"
    else
        show i
```

Alas! Now that we're back to a single expression, we have to consider all the cases again as single if blocks.
The Java implementation is nicer! What gives?!
Well, `hlint` is telling us to refactor that to use guards rather than `if`, so let's do that and see if anything jumps out at us.

```haskell
fizzLogic :: Integer -> String
fizzLogic i
    | i `mod` 3 == 0 && i `mod` 5 == 0 = "FizzBuzz"
    | i `mod` 3 == 0                   = "Fizz"
    | i `mod` 5 == 0                   = "Buzz"
    | otherwise                        = show i
```

Now that it's laid out like this... I think I see a pattern!
Let me align it a little differently:

```haskell
fizzLogic :: Integer -> String
fizzLogic i
    | i `mod` 3 == 0 && i `mod` 5 == 0 = "Fizz" ++ "Buzz"
    | i `mod` 3 == 0                   = "Fizz" ++ ""
    |                   i `mod` 5 == 0 = ""     ++ "Buzz"
    | otherwise                        = show i
```

Do you see it?
It's one of our favorite things: a monoid!
Actually, it's a whole *bunch* of monoids.

# Monoids

A monoid is a neat little idea from abstract algebra that shows up almost everywhere.
A monoid is a collection of three things:

1. A set of objects
2. An associative binary operation (that is, $a \diamond (b \diamond c) = (a \diamond b) \diamond c$)
3. An identity value for the operation (that is, $a \diamond id = a$ and $id \diamond a = a$)

Boolean values and `&&` form a monoid, where the set is `{True, False}`, the operation is `&&`, and the identity is `True`.
Strings and `++` form a monoid, where `""` (the empty string) is the identity element.
Integers, `+`, and `0` form a monoid, as do the integers, `*`, and `1`.
They're everywhere!

Getting back to fizzing and buzzing, let's codify the general form of the rule.
We get an integer, and we might return a string.
If we return multiple strings, we concatenate them all.
If we don't, then we just print the number.

We seem to have a set of rules that may or may not fire.
If more than one rule fires, we combine the results of the rule.
If none fire, then we need a default value.

Let's represent this in Haskell:

```haskell
type FizzRule = Integer -> Maybe String

rule :: Integer -> String -> FizzRule
rule n m i =
    case i `mod` n of
         0 -> Just m
         _ -> Nothing

fizz = rule 3 "Fizz"
buzz = rule 5 "Buzz"
```

Alright, so now we have a `[FizzRule]`.
How do we use that?

There are quite a few neat things we can do.
`sequence` is a promising candidate:

```haskell
sequence :: Monad m => [m a] -> m [a]
-- or, the more generic:
sequenceA :: (Applicative f, Traversable t)
          => t (f a) -> f (t a)
```

As it happens, `a -> b` forms a monad and an applicative!
So we if specialize `sequence` to functions (and then again to `Integer -> Maybe String`), we get:

```haskell
sequence :: [a -> b] -> (a -> [b])
sequence :: [Integer -> Maybe String] -> Integer -> [Maybe String]
```

This is pretty close!
Now we want to combine the `[Maybe String]` into a `Maybe [String]`.
This is, again, `sequence`, since `Maybe` is a monad.
Finally, we want to concatenate those inner strings.
We can use `fmap` over the Maybe with `mconcat`:

```haskell
mconcat :: Monoid m => [m] -> m

combineRules :: [Integer -> Maybe String] -> Integer -> Maybe String
combineRules rules i = fmap mconcat (sequence (sequence rules i))
```

Nice! This kind of polymorphic power is what's so neat about Haskell.
Those type classes are doing a bunch of work for us under the hood, and we don't really have to worry about it.

What if I told you a lot of that work was unnecessary?
We can punt almost all of this to our fancy monoid instances with a single function:

```haskell
fold :: (Foldable t, Monoid m) => t m -> m
```

# Dat Fold

Here's where things get fun:

```haskell
fizzBuzz :: [FizzRule] -> [Integer] -> [String]
fizzBuzz rules = map f
  where
    f i = fromMaybe (show i) (ruleSet i)
    ruleSet = fold rules
```

The magic happens in `fold rules`.
Let's inspect the type signature of `fold`:

```haskell
fold :: (Foldable t, Monoid m) => t m -> m
```

We've got a `[FizzRule]` or `[Integer -> Maybe String]`.
So `Foldable t ~ []` and `Monoid m ~ Integer -> Maybe String`.
The monoid instance that comes into play here is:

```haskell
instance Monoid m => Monoid (a -> m) where
    mempty = const mempty
    mappend f g = \x -> f x `mappend` g x
```

So we require yet another monoid instance for the result of the function.
The instance that comes into play *here* is:

```haskell
instance Monoid a => Monoid (Maybe a) where
    mempty = Nothing
    mappend (Just a) (Just b) = Just (mappend a b)
    mappend (Just a) Nothing  = Just a
    mappend Nothing  (Just a) = Just a
    mappend Nothing  Nothing  = Nothing
```

Along with the `Monoid` instance for `String`, which is `mappend = (++)` and `mempty = ""`.
So we've folded three levels of Monoidal structure together.
All that work came for free with the `fold` function.

Note that we've come across a really powerful concept here: `fold rules` can be used to take a bunch of distinct rules, run them across the same input, and collect their responses.
Fizzbuzz is a somewhat trivial implementation, but the generalized concept is really useful.

If you're feeling frisky, you can get a little more type class magic going by using the Functor and Applicative instance for functions.
This lets us write:

```haskell
fizzBuzz :: (Functor f, Foldable t)
         => t (Integer -> Maybe String)
         -> f Integer
         -> f String
fizzBuzz rules = fmap (fromMaybe <$> show <*> ruleSet)
  where
    ruleSet = fold rules
```

What?

The Functor instance for functions is just function composition.
Compare the type signature of `(.) :: (b -> c) -> (a -> b) -> (a -> c)` with:

```haskell
instance Functor ((->) r) where
    fmap :: (a -> b) -> (r -> a) -> (r -> b)
    fmap f g = f . g
```

Now, we get to the Applicative instance...

```haskell
instance Applicative ((->) r) where
    pure = const
    f <*> g = \x -> f x (g x)
```

The pattern `f <$> g <*> h` with explicit parentheses is `(f <$> g) <*> x`.
When we expand that out, we get `\x -> f (g x) (h x)`, which is how we get `fromMaybe <$> show <*> ruleSet`.

I don't know about you, but when I was working on this, my brain just about exploded.
There's one last fun bit of abstract nonsense...

# A monoid in the category...

The joke in functional programming is:

> "A monad is just a monoid in the category of endofunctors, what's the problem?"

Everyone laughs because *wait what*.
But -- as it happens, we get a little bit of a hint as to the deeper meaning!

The `Foldable` class is the [`toFreeMonoid`](http://comonad.com/reader/2015/free-monoids-in-haskell/) class.
One of the member functions is `foldMap`:

```haskell
foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
```

which maps each element of the structure to a monoid and then `mappends` them all together into a final monoid value.

Now, suppose we specialize `foldMap` such that `Foldable t ~ []` and `Monoid m ~ [b]`... This is the resulting signature:

```haskell
foldMap :: (a -> [b]) -> [a] -> [b]
```

Which should look really familiar: that's very nearly the type signature of `>>=`!
In fact, that's precisely the type signature of `=<<` specialized to lists:

```haskell
-- or :: (a -> [b]) -> [a] -> [b]
(=<<) :: (a -> m b) -> m a -> m b
(=<<) = flip (>>=)
```

## uhhh

Fizzbuzz has a neat monoidal solution that directly relates to rules engines.
I accidentally discovered another neat connection between monoids, folds, and monads.
You never know where this kind of thing might take you!
