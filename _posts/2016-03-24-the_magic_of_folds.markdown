---
title: "The Magic of Folds"
date: 2016-03-24
layout: post
categories: programming
---

Folds are a common stumbling point for people learning about the functional paradigm.
I remember being pretty confused about the difference between a left fold, a right fold, and how either of them differ from a `reduce`.
I'm going to try and explain them in a way that's easy for non-functional programmers to get.
If you don't get it, I've screwed up -- feel free to let me know!

To keep things simple, the example will just use singly linked lists in Java and Haskell.

# Lists

Let's get warmed up and write out our `List` class.
We're trying on our functional programming hats, so we'll keep it fairly simple:

```java
class List<T> {
    public final T head;
    public final List<T> tail;

    private List(T head, List<T> tail) {
        this.head = head;
        this.tail = tail;
    }

    public static <T> List<T> Cons(T item, List<T> rest) {
        return new List<T>(item, rest);
    }
 
    public static <T> List<T> Nil() {
        return new List<>(null, null);
    }
}
```

We've got two constructors: `Cons` for putting something on a list, and `Nil` for an empty list. We can build a simple list like:

```java
Cons(1, Cons (2, Cons(3, Nil())))
```

And the same in Haskell:

```haskell
data [t] = t : [ts]
         | []
```

If you're unfamiliar with Haskell, this defines two constructors: an infix constructor `:` and the empty list `[]` constructor.
`t` is a type parameter.
We can make `1 : 2 : 3 : []` like this, though the language gives us syntax sugar to write `[1, 2, 3]` instead.

To continue warming up, let's write out `map` and `filter` for lists.
This will help with our intuition on folds later on!

# Maps And Filters

`map` is a function or method usually defined on lists and arrays, though you can define it for all kinds of types.
Our intuition for a map is that -- for a structure like `List<A>`, we'll have a `Function<A, B>`.
We'll take all the `<A>` values in the list and return a new list with `<B>` values instead.

Recursive stuff works best if you can think of the possible cases.
For lists, we've got two constructors: `Nil` and `Cons`.
If we have a `Nil`, we return another `Nil`.
If we have a `Cons`, we return another `Cons` after applying the function to the `head` and `map`ping over the tail:

```java
static <A, B> List<B> map(Function<A, B> f, List<A> list) {
    if (list.isNil()) {
        return List.Nil();
    }
    return List.Cons(f.apply(list.head), map(f, list.tail));
}
```

Haskell gives us pattern matching, so instead of an `if`, we match on the constructors:

```haskell
map f [] = []
map f (head : tail) = (f head) : (map f tail)
```

This is a little concise, and we can possibly make it more clear by naming some of the intermediate steps.

```java
static <A, B> List<B> map(Function<A, B> f, List<A> list) {
    if (list.isNil()) {
        return List.Nil();
    }

    A val = list.head;
    List<A> rest = list.tail;

    B newVal = f.apply(val);
    List<B> newRest = map(f, rest);

    return List.Cons(newVal, newRest);
}
```

```haskell
map f [] = []
map f (head : tail) =
    let newVal = f head
        newRest = map f tail
     in newVal : newRest
```

Cool. Alright, let's do `filter` now.
Filter takes a `predicate` and returns a new list where the elements in the new list returned true for the predicate.
Filtering an empty list gives us an empty list.
When we filter a `Cons`, we have two cases:

1. Applying the `predicate` to the `head` returns `true`.
2. The above returns `false`.

In both cases, we'll want to continue filtering the list.
In the first case, we want to keep the item.
In the second case, we don't want to keep it.

```java
static <A> List<A> filter(Function<A, Boolean> predicate, List<A> list) {
    if (list.isNil()) {
        return List.Nil();
    }

    if (predicate.apply(list.head)) {
        return List.Cons(list.head, filter(predicate, list.tail));
    }

    return filter(predicate, list.tail);
}
```

```haskell
filter pred [] = []
filter pred (head : tail) =
    if pred head
       then head : (filter pred tail)
       else filter pred tail
```

# Last Warmup

Finally, let's write `sum` to sum all the numbers in a list.
The sum of an empty list is `0`, and the sum of a non-empty list is the value of the head plus the sum of the tail.

Easy enough, let's write it out:

```java
static Integer sum(List<Integer> list) {
    if (list.isNil()) {
        return 0;
    }
    return list.head + sum(list.tail);
}
```

```haskell
sum [] = 0
sum (x:xs) = x + sum xs
```

Map, filter, and sum all have some things in common:

1. They recursively walk a list
2. They do something with each element with the result of the rest of the list
2. They have a value for the empty list case.

Alright, with that, we're ready to conquer folds.

# Fold

A fold has three arguments:

1. The zero value (or, what to do with the end of the list)
2. The function to combine 
3. The list to fold

`foldr` is a right fold, `foldl` is a left fold, and they're defined like this:

```haskell
foldr k z []     = z
foldr k z (x:xs) = k x (foldr k z xs)

foldl k z []     = z
foldl k z (x:xs) = foldl k (k z x) xs
```

The variable `k` is our combining function.
`foldl` is tail recursive, and passes the result of combining the accumulator `z` with the current item on the list.

"but matt, this doesn't help me understand"

Right. We're getting there.
Haskell lets us use functions infix if we surround them with backticks, so we can also write `foldr` like this:

```haskell
foldr k z [] = z
foldr k z (x:xs) = x `k` (foldr k z xs)
```

The infix isn't superfluous.
We can get a nice intuition for how `foldr` works on a list with it.

Let's see how Haskell would write out `[1..3]` without any sugar:

```haskell
1 : 2 : 3 : []
```

Take every `:` and replace it with `k`, and take the `[]` and replace it with `z`:

```haskell
1 `k` 2 `k` 3 `k` z
```

Now we can substitute `k` for `+` and `z` for `0` and see that this is `sum`:

```haskell
1 + 2 + 3 + 0
```

We can get `map` by replacing `k` with `(\x acc-> f x : acc)`, and we can get `filter` by replacing `k` with `(\x acc -> if p x then x:acc else acc)`.

Let's walk through an example, step by step:

```haskell
-- initial function call
foldr 0 (+) (1 : 2 : 3 : [])

-- recurse:
-- foldr k z (x:xs) = x `k` foldr k z xs
= 1 + (foldr 0 (+) (2 : 3 : []))

-- recurse:
= 1 + (2 + (foldr 0 (+) (3 : [])))

-- recurse:
= 1 + (2 + (3 + (foldr 0 (+) [])))

-- base case:
-- foldr k z [] = z
= 1 + (2 + (3 + 0))
```

Yup! What about `foldl`? There must be some magic there, right?
Nope, though it might look a little strange:

```haskell
-- initial function call
foldl 0 (+) (1 : 2 : 3 : [])

-- recurse:
-- foldl k z (x:xs) = foldl k (k z x) xs
foldl (+) (0 + 1) (2 : 3 : [])

-- recurse:
foldl (+) ((0 + 1) + 2) (3 : [])

-- recurse:
foldl (+) (((0 + 1) + 2) + 3) []

-- base case:
-- foldl k z [] = z
(((0 + 1) + 2) + 3)
```

Interesting! This has nearly the same shape as what `foldr` ended up looking like, but the parentheses are nested differently.
With `foldr`, we directly replace `[]` with our `z` value.
`foldl` prepends the `z` value to the list and just drops the `[]` entirely, so our `foldr` "replace the `:` with `k`" trick needs to be adjusted slightly.

With addition, it doesn't really matter, since you can swap arguments and parentheses around.
Let's try it with subtraction and see the difference:

```haskell
foldr (-) 0 [1..3]
-- desugar the list:
= 1 : 2 : 3 : []
-- replace : and [] with right associating k and z
= 1 `k` (2 `k` (3 `k` z))
-- replace with args:
= 1 - (2 - (3 - 0))
-- evaluate:
= 1 - (2 - 3)
= 1 - (-1)
= 2

foldl (-) 0 [1..3]
= 1 : 2 : 3 : []
-- prepend with z
= z : 1 : 2 : 3 : []
-- replace : with left associating k and drop the []
= ((z `k` 1) `k` 2) `k` 3
-- replace z with 0 and k with -
= ((0 - 1) - 2) - 3
-- evaluate:
= (-1 - 2) - 3
= -3 - 3
= -6
```

If you're curious about the evaluation of these functions, you can use their cousins `scanr` and `scanl`.
Instead of returning a single end result, they return a list of all intermediate steps.

```haskell
-- In GHCi,
λ> scanr (+) 0 [1..3]
[6,5,3,0]
λ> scanl (+) 0 [1..3]
[0,1,3,6]
λ> scanr (-) 0 [1..3]
[2,-1,3,0]
λ> scanl (-) 0 [1..3]
[0,-1,-3,-6]
```

# caffeine pls

Alright, enough Haskell for now, let's implement these two in Java:

```java
static <A, B> B foldRight(BiFunction<A, B, B> k, B z, List<A> list) {
    if (list.isNil()) {
        return z;
    }

    return k.apply(list.head, foldRight(k, z, list.tail));
}

static <A, B> B foldLeft(BiFunction<B, A, B> k, B z, List<A> list) {
    if (list.isNil()) {
        return z;
    }

    return foldLeft(k, k.apply(z, list.head), list.tail);
}

```

And now, let's rewrite `map` and `filter` in terms of these:

```java
static <A, B> List<B> mapR(Function<A, B> f, List<A> list) {
    return foldRight(
            (x, acc) -> List.Cons(f.apply(x), acc), 
            List.Nil(), 
            list
        );
}

static <A> List<A> filterR(Function<A, Boolean> p, List<A> list) {
    return foldRight(
            (x, acc) -> p.apply(x) ? List.Cons(x, acc) : acc,
            List.Nil(),
            list
        );
}
```

# What's the point of foldRight?

So, at first glance, you might think that `foldl` is superior.
It's in tail recursive position, so a clever enough compiler could easily optimize it to a loop (unfortunately, Java doesn't have tail recursion as of now).
Lacking tail recursion, though, they both have to do about the same amount of work, and seem to be equivalent.

There are two reasons why `foldRight` is useful.
We can see the first by implementing map in terms of `foldLeft`:

```java
static <A, B> List<B> mapL(Function<A, B> f, List<A> list) {
    return foldLeft(
            (acc, x) -> acc.append(f.apply(x)),
            List.Nil(),
            list
        );
}
```

Alas! This is quadratic in the size of the input list!
Appending to the end of a singly linked list is $O(n)$ time.
We can verify this by looking at the simplest definition of `append`:

```java
public List<T> append(T elem) {
    return foldRight(List::Cons, Cons(elem), this);
}
```

So `foldRight` can be useful when constructing new lists.
In fact, we can easily write `concat` using `foldRight`:

```java
public static <T> List<T> concat(List<T> first, List<T> second) {
    return foldRight(List::Cons, second, first);
}
```

The ease of writing functions like this isn't a coincidence.
`foldRight` is theoretically entwined with singly linked lists.
The ordinary definitions of data structures involve "how do I construct this," but you can also define data structures in terms of "how do I deconstruct this."
`foldRight` is that definition.
This is referred to as the Church encoding of a list.

# laziness

Another difference between `foldl` and `foldr` is how they work with laziness.
Laziness can be tricky to understand at first, since it defies all of our intuitions about how to evaluate code.

Consider the implementation of `map` using `foldr`:

```haskell
map f xs = foldr (\x acc -> f x : acc) [] xs
```

A `map` law is that composing two maps is the same as a single map with the two functions composed.
In fancy math,

$$map f \circ map g = map (f \circ g)$$

In Haskell,

```haskell
map f . map g = map (f . g)
```

In Java,

```java
map(f, map(g, list)) = map(compose(f, g), list)
```

If we can fuse the two maps like this, then we can make this dramatically more efficient.
Does `foldr` respect this law with respect to performance?
Let's watch `map (+1) . map (*2)` work, using our `foldr` definitions.
`print` will demand our values and force their evaluation.

```haskell
printEach []     = print "Done"
printEach (x:xs) = do
    print x 
    printEach xs

printEach (map (+1) (map (*2) [1, 2, 3]))
```

`print` is what actually forces evaluation here, so nothing gets evaluated until `print` forces it, and only as much as is required for `print`.
First, `printEach` matches on `map (+1) (map (*2) [1,2,3])`.
It needs to know if that evaluates to `[]` or `(x:xs)`.
This causes `map (+1)` to match on `map (*2) [1,2,3]`.
Which causes `map (*2)` to match on `[1,2,3]`.

```haskell
printEach (
    map (+1) (
        map (*2) [1, 2, 3]
    )
)

-- substitute `foldr` definition for `map`:
printEach (
    map (+1) (
        foldr (\x acc -> x * 2 : acc) [] [1,2,3]
    )
)

-- foldr's (x:xs) case matches, expression becomes:
printEach (
    map (+1) (
        1 * 2 : foldr (\x acc -> x * 2 : acc) [] [2, 3]
    )
)

-- first `map` can pattern match now, so we expand to the 
-- foldr definition:
printEach (
    foldr (\x acc -> x + 1 : acc) [] (
        1 * 2 : foldr (\x acc -> x * 2 : acc) [] [2, 3]
    )
)

-- foldr matches on `(x:xs)`, so we evaluate that bit:
printEach (
    1 * 2 + 1 : foldr (\x acc -> x + 1 : acc) [] (
        foldr (\x acc -> x * 2 : acc) [] [2, 3]
    )
)

-- printEach matches on (x:xs), so we can go to `print x`:
printEach (1 * 2 + 1 : xs) = do
    print (1 * 2 + 1)
    printEach xs

-- `print` does the match and prints it, and then recurses:
printEach (
    foldr (\x acc -> x + 1 : acc) [] (
        foldr (\x acc -> x * 2 : acc) [] [2, 3]
    )
)
```

This process repeats, until eventually the inner `foldr` yields an empty list, and then the outer `foldr` yields an empty list, and then `printEach` prints `"Done."` to finish things off.
