---
title: "Grokking Fix"
date: 2016-10-26
layout: post
categories: programming
---

> This post is intended for beginners of functional programming interested in an exploration of laziness, Haskell, and recursion

Haskell's laziness enables some pretty cool tricks.
The `fix` function is one of the neater ones, though it can be hard to understand how to use it from just the implementation and type signature.
If you grab a calculator and put *any* number into it, you can start hitting the `cos` button.
After a while, the number will start getting closer and closer to the [*fixed point* of cosine](http://mathworld.wolfram.com/DottieNumber.html).

A fixed point of a function is some value where applying the function to the value returns the same value.
The equation is a little easier to get, for some function $f$, the fix point $c$ is:

$$f(c) = c$$

We can implement this in Haskell! The entirety of the magic is right here:

```haskell
fix :: (a -> a) -> a
fix f = 
    let x = f x 
     in x
```

You might be thinking:

> wat

And you'd be right! This is a really odd definition.
It relies on the fact that Haskell values are lazy, and that you can refer to terms before defining them.

# fixing identity

The type signature says that we, the callers of the function, get to choose whatever `a` type we want.
`(a -> a)` calls to mind `id`, which we can use as an easy first choice to see how Haskell evaluates this expression.

```haskell
-- We call it:
fix id

-- Rewrite:
let x = id x
 in x

-- Rewrite `x` on the right hand side in terms of how it is defined:
let x = id (id x)
 in x

-- Repeat:
let x = id (id (id x))
 in x

-- Again, but with function composition:
let x = (id . id . id . id) x
 in x

-- *yawn*
let x = (id . id . ... . id . id) x
 in x
```

So this is just an infinite application of `id` to `x`! 
But where is `x`? What is it? This is precisely `_|_`: bottom, the value-that-is-no-value, the term `undefined`!
So no matter how far you dig into that infinite pile of `id`s, you'll never reach `bottom`.

Another way to write this is to inline the definition of `id` right into our calling of `fix`.

```haskell
fix (\x -> x)
```

Well, that was kind of pointless.
How else can we use this function?
Perhaps `fix cos` can get us that number we want!

If we type that into GHCi, though, we get `*** Exception: <<loop>>`.
The function doesn't have any way to terminate recursion, so this still repeats forever.

# fixing more interesting things

Specializing the type means we can specialize to *anything* we want. 
This *includes* function types!
So we can also specialize the type of `fix` to be:

```haskell
fix :: (a        -> a       ) -> a        [1]
fix :: ((b -> c) -> (b -> c)) -> (b -> c) [2]
fix :: ((b -> c) ->  b -> c ) -> b -> c   [3]
```

Here we have:

1. The original definition.
2. Specializing the type variable `a` to the function type `b -> c`
3. Dropping some redundant parentheses (remember, function arrow associates to the right, so `a -> (b -> c)` is equivalent to `a -> b -> c`)

This small change has had a pretty dramatic effect on how the type signature reads. 
`fix :: (a -> a) -> a` reads like "Give me a function that takes a single argument and returns a value of the same type, and I'll give you a value of that type."
The two parameter version reads like: 

> Give me a function that takes two arguments: the first being a function from `b` to `c`, and the second being a value of type `b`.
> Then, if you give me a `b`, then, I'll give you a `c`.

This is much more interesting. What might an example of this look like?

```haskell
cosFixpoint x =
    fix (\f b -> 
            if cos b == b 
               then b
               else f (cos b)
         ) x
```

Evaluating `cosFixpoint` for any `x` gives us the same result:

```haskell
λ> fix (\f x -> if x == cos x then x else f (cos x)) 3
0.7390851332151607
λ> fix (\f x -> if x == cos x then x else f (cos x)) 4
0.7390851332151607
λ> fix (\f x -> if x == cos x then x else f (cos x)) 5
0.7390851332151607
λ> fix (\f x -> if x == cos x then x else f (cos x)) 6
0.7390851332151607
```

Now you might notice something interesting here.
The function argument `f` -- what is that function's definition?
It's the lambda!
We could rewrite this as an explicit recursion with a very similar structure:

```haskell
cosFixpointExplicit x =
    if cos x == x
       then x
       else cosFixpointExplicit (cos x)
```

In fact, we can use `fix` to factor out recursion anywhere we might find it.
What might this look like for `last`?

```haskell
last :: [a] -> Maybe a
last [] = Nothing
last [x] = Just x
last (x:xs) = last xs
```

First, we'd factor out the named recursion, and then pattern match on the list.

```haskell
last' = fix $ \f xs ->
    case xs of
        [] -> 
            Nothing
        [x] -> 
            Just x
        (x:xs) -> 
            f xs
```

How about `map`?

```haskell
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

map' f = fix $ \recurse list ->
    case list of
        [] -> []
        (x:xs) -> f x : recurse xs
```

Neat!

# monadic fixins

Do we need another definition of the function to work with monadic functions?
Let's specialize the type and see what happens:

```haskell
fix :: (a -> a) -> a
fix :: Monad m 
    => ((a -> m b) -> a -> m b) -> a -> m b
```

That checks out. Let's write something that does a bit of `IO` with `fix` now:

```haskell
printUntilZero =
    fix $ \f x -> 
        if x >= 0
           then do
               print x
               f (x - 1)
           else 
               pure x

-- pasting into GHCi:

λ> fix (\f x -> if x >= 0 then do print x; f (x - 1) else pure x) 4
4
3
2
1
0
-1
```

The `-1` in the output is the *return* value of the fix expression: the printed numbers are the side effects.

Well, that's weird and cool. How exactly does this all work again?
Let's review the definition:

```haskell
fix :: (a -> a) -> a
fix f = 
    let x = f x
     in x
```

Haskell's laziness allows us to essentially rewrite this one step at a time, exactly as we demand the results of it.
How does the language compile and understand this?

First, it checks the syntax, which is totally okay.

Second, it collects all of the declarations and their type signatures together.
We're declaring three things: 

1. The top level function `fix`
2. `fix`'s first argument, `f`
3. The `let` bound variable `x`, which is available in the bodies of the `let` variable *and* the expression immediately following the `in`.

Then, it goes to type check the expression.
As it type checks, it also ensures that no variables are undeclared.
Since `x` is declared in the `let` block, and `f x` is well typed, it accepts this definition.

Haskell will continue applying `f` to `x` each time we demand the next bit of evaluation, or alternatively, whenever `f` self-terminates.
We saw above that `fix id` dug a big hole of `id` applications, from which we'd never get out.
But specializing to a function type allowed us to provide a starting point, and terminate early!

# The Secret Tricks

1. Combinators are cool.
2. You can specialize plain `a` types to function types `a -> b` for interesting results.
3. Laziness is fun
