---
title: "Effects and Structures I"
date: 2016-02-27
layout: post
categories: programming
---

A lot of people have this idea that functional programming is all about programming without any effects.
`IO` is a big obvious effect -- none of that please!
`State` is another effect, no thanks.
Exceptions are also an effect, and we don't want those in pure code, but they're there anyway.
Laziness is also an effect: a `something :: a` in Haskell is *really* a pointer to a thunk that might be a value of `a`, or it might be an unevaluated function that will yield an `a`, or it might be an exception that blows up when you try to evaluate it.

Real world Haskell code uses effects all over the place.
They're just annotated in the types, so we know what's going on.
Since they're just normal Haskell types, we can also do much more cool stuff with them.
Let's explore some effectful Haskell, extract some pure code, and see why stacking effects is useful.

# Effectful Haskell

Haskell is an excellent imperative programming language.
We can get all the effects we want by stacking them on top of each other.
Let's simulate a bit of Java.
First, we'll need IO:

```haskell
type JavaI a = IO a
```

But that's not quite enough -- Java has implicit nulls, so let's account for that with the `Maybe` type.

```haskell
type JavaN a = IO (Maybe a)
```

Also, there are exceptions, which we can model with `Either e a`.

```haskell
type JavaE e a = IO (Either e (Maybe a))
```

We also want mutable state:

```haskell
type JavaS s e a = IO (State s (Either e (Maybe a)))
```

And read-only environment information:

```haskell
type JavaR r s e a = IO (Reader r (State s (Either e (Maybe a))))
```

We can express all of this in slightly more idiomatic Haskell as:

```haskell
type Java r s e a = MaybeT (ExceptT e (StateT s (ReaderT r IO))) a
```

Now we've got a great big fat stack of effects we'll be using.
We can convert this out of our stack into a more concrete value using this `runJava` function:

```haskell
runJava :: r -> s -> Java r s e a -> IO (Either e (Maybe a), s)
runJava r s = flip runReaderT r . flip runStateT s . runExceptT . runMaybeT
```

We pass in an initial environment `r`, an initial state `s`, and it returns an `IO` with a pair of values.
The first is `Either` an error or a `Maybe a`, which might be a null value.
The second item is the result state after running the action.

Now that we've defined all of that, let's write some imperative code!
Here's `fizzBuzz`, first in the real Java and then in our imperative Haskell:

```java
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
```

```haskell
fizzBuzz :: Java () () () ()
fizzBuzz =
    forM_ [1..100] (\i ->
        if i `mod` 3 == 0 && i `mod` 5 == 0 then
            liftIO (putStrLn "FizzBuzz")
        else if i `mod` 3 == 0 then
            liftIO (putStrLn "Fizz")
        else if i `mod` 5 == 0 then
            liftIO (putStrLn "Buzz")
        else
            liftIO (putStrLn (show i))
    )
```

There's no environment, state, exception, or return value, so we use the `()` type to indicate that.
All we're doing is looping over the range `[1..100]`, passing each number `i` in to the lambda, and doing the standard FizzBuzz stuff to it.
There's a good bit of commonality that's bugging me though -- let's abstract out the `liftIO (putStrLn ...)`.
Haskell's `if ... then ... else ...` is an expression, so the whole thing evaluates to a value.

```haskell
fizzBuzz :: Java () () () ()
fizzBuzz =
    forM_ [1..100] (\i -> liftIO (putStrLn (
        if i `mod` 3 == 0 && i `mod` 5 == 0 then
            "FizzBuzz"
        else if i `mod` 3 == 0 then
            "Fizz"
        else if i `mod` 5 == 0 then
            "Buzz"
        else
            show i)
        )
    )
```

Unfortunately, Java's `if` statements block this sort of a refactoring.
We can factor the expression into a lambda and call it repeatedly, though:

```java
public static void fizzBuzz() {
    Consumer<String> action = str -> System.out.println(str);

    for (int i = 1; i <= 100; i++) {
        if (i % 3 == 0 && i % 5 == 0) {
            action.accept("FizzBuzz");
        } else if (i % 3 == 0) {
            action.accept("Fizz");
        } else if (i % 5 == 0) {
            action.accept("Buzz");
        } else {
            action.accept(new Integer(i).toString());
        }
    }
}
```

Of course, this is totally opaque.
It'd be much better to return a list of strings that we can verify for correctness.
We can use our mutable state to accumulate a list of strings.
In Java, this is going to look like:

```java
public static List<String> fizzBuzz() {
    final List<String> state = new ArrayList<>();
    Consumer<String> action = str -> state.add(str);

    for (int i = 1; i <= 100; i++) {
        if (i % 3 == 0 && i % 5 == 0) {
            action.accept("FizzBuzz");
        } else if (i % 3 == 0) {
            action.accept("Fizz");
        } else if (i % 5 == 0) {
            action.accept("Buzz");
        } else {
            action.accept(new Integer(i).toString());
        }
    }

    return state;
}
```

And, in Haskell:

```haskell
fizzBuzzS :: Java r [String] e [String]
fizzBuzzS = do
    forM_ [1..100] (\i -> modify (\s -> s ++ [
        if i `mod` 3 == 0 && i `mod` 5 == 0 then
            "FizzBuzz"
        else if i `mod` 3 == 0 then
            "Fizz"
        else if i `mod` 5 == 0 then
            "Buzz"
        else
            show i]
        )
    )
    get
```

We use the odd `s ++ [if...]` syntax because Haskell's singly linked lists don't have a convenient `appendToEnd` function, because this is actually not a great idea: appending an item to the end of a singly linked list like this is $O(n)$ time, and it only gets worse the more times you do it.

And, in fact... this is telling us that we're doing something silly.
We are directly transforming each `int` into a `String`.
This is a map!
Fortunately, `forM` and `forM_` in Haskell are secretly `mapM` with arguments reversed, so we can just switch to `forM` and return the list directly without needing to worry about the state.

```haskell
fizzBuzzS :: Java r s e [String]
fizzBuzzS = do
    forM [1..100] (\i -> return (
        if i `mod` 3 == 0 && i `mod` 5 == 0 then
            "FizzBuzz"
        else if i `mod` 3 == 0 then
            "Fizz"
        else if i `mod` 5 == 0 then
            "Buzz"
        else
            show i
        )
    )
```
Of course, now I want to factor out that fizzbuzz function.
Factoring it out in Java and Haskell is about the same thus far:

```java
public static String fizzLogic(int i) {
    if (i % 3 == 0 && i % 5 == 0) {
        return "FizzBuzz";
    } else if (i % 3 == 0) {
        return "Fizz";
    } else if (i % 5 == 0) {
        return "Buzz";
    } else {
        return new Integer(i).toString();
    }
}

// so functional, wow
public static List<String> fizzBuzz() {
    return IntStream
        .range(1, 101)
        .mapToObj(Fizz::fizzLogic)
        .collect(ArrayList::new, ArrayList::add, ArrayList::addAll);
}
```

And, in Haskell:

```haskell
fizzBuzzS :: Java r s e [String]
fizzBuzzS = return (map fizzLogic [1..100])

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

# A slight refactoring detour

We'll get back to effects and why explicit effects are so cool and useful next post.
I got sidetracked and shaveyaked and here I am solving fizzbuzz.

Apparently, people used to get asked FizzBuzz for interview questions.
Then it became a bit of a joke, so people continued to use it.
However, after the interviewee wrote up the memorized internet solution, the interviewer would then ask:

> Modify the code so that multiples of 7 print "Baz"

And then everything was terrible.
Making fizzbuzz extensible isn't trivial.
If we expose a tiny bit of computational structure, though, a solution presents itself to us...

Anyway, now that I've factored it out, `hlint` is yelling at me to refactor the function to use guards instead of `if`.
Let's do that!
Oftentimes, the way a function is laid out and presented can alter our understanding of it.

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
2. An associative binary operation (that is, `a <> (b <> c) = (a <> b) <> c`)
3. An identity value for the operation (that is, `a <> mempty = a` and `mempty <> a = a`)

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
```

As it happens, `a -> b` forms a monad!
So we if specialize `sequence` to functions (and then again to `Integer -> Maybe String`), we get:

```haskell
sequence :: [a -> b] -> a -> [b]
sequence :: [Integer -> Maybe String] -> Integer -> [Maybe String]
```

This is pretty close!
Now we want to combine the `[Maybe String]` into a `Maybe [String]`.
This is, again, `sequence`, since `Maybe` is a monad.
Finally, we want to concatenate those inner strings.
We can use `fmap` over the Maybe, and use `mconcat`:

```haskell
mconcat :: Monoid m => [m] -> m

combineRules :: [Integer -> Maybe String] -> Integer -> Maybe String
combineRules rules i =
  (fmap mconcat :: Maybe [String] -> Maybe String)
  . (sequence :: [Maybe String] -> Maybe [String])
  $ (sequence :: [Integer -> Maybe String] -> (Integer -> [Maybe String]))
  rules i
```

Whaat -- `fmap mconcat (sequence (sequence rules i))`??
That's awesome.
Except, it's kind of difficult to grok.

What if I told you a lot of that work was unnecessary?
We can punt almost all of this to our fancy monoid instances with a single function:

```haskell
fold :: (Foldable t, Monoid m) => t m -> m
```

# Dat Fold

Here's where things get fun:

```haskell
genericFizzBuzz :: [FizzRule] -> [String]
genericFizzBuzz rules = map f [1..100]
  where
    f i = fromMaybe (show i) (fold rules i)
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
fizzBuzz :: (Functor f, Foldable t) => t (Integer -> String) -> f Integer -> f String
fizzBuzz rules = fmap (fromMaybe <$> show <*> fold rules)
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
where `pure` is `const` and `(f <*> g)` is `\x -> f (g x) x`!
The pattern `f <$> g <*> h` is then `\x -> f (g x) (h x)`, which is how we get `fromMaybe <$> show <*> fold rules`.

I don't know about you, but when I was working on this, my brain just about exploded.
There's one last fun bit...

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

Well, I set out to explore how explicit effects allow us to do some really cool stuff, and I ended up exploring Fizzbuzz's monoidal nature.
I accidentally discovered another neat connection between monoids, folds, and monads.
Next post I do plan on writing about effects and what we gain when we're explicit!
