---
title: "Hardware Simulation in Idris I"
date: 2016-02-18
layout: post
categories: programming
---

I've been digging into Idris lately, and have found using dependent types to be a fun and challenging exercise.
Last semester, I implemented some basic computer architecture concepts in Idris as a means of learning both.
There's more work that I'd like to do on it, and I'd like to write up the process involved thus far.

You shouldn't need any knowledge in either Idris or computer hardware to get this.
Some experience with Elm, ML, Haskell, etc. might be useful though!
Let's build some (simulated) hardware!

# Boolean Logic

Computers implement Boolean logic using voltages.
If a source has higher voltage, that's `True`.
If it has lower voltage, that's `False`.
The exact specifics depend on the hardware manufacturer, and we'll assume that we have `True` and `False`.

One neat fact about Boolean logic is that it is completely expressible with a single operation.
`NAND` and `NOR` are both capable of expressing the entirety of Boolean logic.
Here is the truth table for `NAND`:

| x | y | NAND x y |
|---|---|----------|
| T | T |  F       |
| T | F |  T       |
| F | T |  T       |
| F | F |  T       |

And the implementation in Idris:

```idris
nand : Bool -> Bool -> Bool
nand True True = False
nand x    y    = True
```

Idris, like Haskell, allows top level pattern matching.
This makes it pretty easy to define the function.
We use `x : y` to say `x` has the type `y`, like Elm and ML (and proper math).

We'll be using the above type signature frequently, so we'll want to define a type alias for it.
This is how that looks in Idris:

```idris
Gate : Type
Gate = Bool -> Bool -> Bool
```

We declare a term `Gate` that has the type `Type`.
Then we define `Gate` to be equal to a function type `Bool -> Bool -> Bool`.
Idris uses the same stuff to talk about types and terms.

Now we need to recover `AND`, `OR`, `NOT`, etc.
Let's do `AND` first.
Wikipedia is [kind enough to include a diagram for constructing `AND` from `NAND`](https://en.wikipedia.org/wiki/And_gate), and has diagrams for all the other gates too.
Here are the implementations:

```idris
and : Gate
and a b = nand (nand a b) (nand a b)
```

Idris, like Haskell and Elm, uses whitespace for function application.
So `f x` is the function `f` applied to `x`.

```idris
or : Gate
or a b = nand (nand a a) (nand b b)

not : Bool -> Bool
not a = nand a a

nor : Gate
nor a b = nand q q
  where
    q = nand (nand a a) (nand b b)
```

Idris lets you define sub expressions in `where` declarations.
If the type is simple enough, then it can infer the type.
We don't have to say `q : Bool`, for example.

```idris
xor : Gate
xor a b = nand d e
  where
    c : Bool
    c = nand a b
    d = nand c a
    e = nand c b
```

Note that in `xor`, we *are* required to annotate the type of `c`.
Dependently typed languages trade complete type inference for additional expressiveness in the types.

# Adders

Now that we've assembled all of our logic gates, we can start doing some interesting work with them.
`OR` in Boolean logic is equivalent to addition.
We'll represent True as 1 and False as 0.
Compare the truth table for `OR` with adding 0 and 1:

| x | y | x OR y | x + y |
|---|---|--------|-------|
| 1 | 1 | 1      | 2     |
| 1 | 0 | 1      | 1     |
| 0 | 1 | 1      | 1     |
| 0 | 0 | 0      | 0     |

This is almost right!
When `x` and `y` are both 1, then `x OR y` loses the information.
What we're missing is an implied `0` to indicate that we're at `2` and not `1`.
To make it right, we really need `1 OR 1` to be `10`.

So we need an additional bit: a carry bit.
This will only be set when `x` and `y` are both `1`.
We also can't use OR, since it doesn't have the right truth table anymore.
We'll interpret the `carry` bit as the two's place and the XOR bit as the one's place.
Here's our truth table, with + represented in binary:

| x | y | carry | x XOR y | x + y |
|---|---|-------|---------|-------|
| 1 | 1 | 1     | 0       | 10    |
| 1 | 0 | 0     | 1       | 01    |
| 0 | 1 | 0     | 1       | 01    |
| 0 | 0 | 0     | 0       | 00    |

So our `carry` bit is the same as `AND`, and our first bit is `XOR`.
Let's put it in Idris:

```idris
halfAdder : Bool -> Bool -> (Bool, Bool)
halfAdder x y = (xor x y, and x y)
```

Idris has easy tupling like Haskell and Elm.
We can signify that a function has two outputs by returning a tuple of values.
The half adder is capable of adding two Boolean values, but since it doesn't take a carry bit, it can't deal with it's own output.
A full adder has a `CarryIn` input and uses that to determine the result and output.
The Idris implementation looks like:

```idris
fullAdder : Bool -> Bool -> Bool -> (Bool, Bool)
fullAdder x y c = (result, carryOut)
  where
    result   = xor x (xor y c)
    carryOut = or (and x y) (and c (xor x y))
```

Alright, so we've implemented the circuit for adding two `Bool`s with a carry value.
We're going to cheat a little bit and use Idris's vectors rather than worrying about making memory right now.

# Actual Operations: Vectors!

We haven't really taken advantage of any dependent typing yet.
Length indexed vectors are the first dependently typed data structure that most people work with.
They're pretty useful!
Idris defines them using this syntax:

```idris
data Vect : Nat -> Type -> Type where
    Nil  : Vect Z a
    (::) : a -> Vect k a -> Vect (S k) a

data Nat = Z | S Nat
```

We can read that like "Vect is a type constructor that takes a natural number, a type, and returns a type."
The first data constructor, `Nil`, has the type `Vect Z a`.
`Z` is a representation of the natural number zero, so `Nil` has length 0 (as we'd expect).
`::` is used to prepend a value of type `a` to a vector of type `Vect k a`, resulting in a `Vect (S k) a`.
The `S` is for successor, and essentially means `+1`.

This lets us talk about functions which only take non-empty vectors:

```idris
head : Vect (S n) a -> a
```

Idris does pattern matching on the `(S n)`, which will succeed for values greater than 0.
It's now a compile time type error to provide `Nil` to `head`, which is great!

We're going to represent a machine word as a vector of Bool:

```idris
Word : Nat -> Type
Word n = Vect n Bool

Byte : Type
Byte = Word 8
```

Zero, as a `Byte`, will just be 8 `False` values.
That's easy enough to implement:

```idris
zeroByte : Byte
zeroByte = replicate 8 False
```

But what if we want a `zero` of arbitrary word size?
Idris allows us to use type information in our function bodies.
Check this out:

```idris
zero : Word n
zero {n} = replicate n False
```

The `{n}` is the exact same natural number in the type!
If we load that in the REPL, we can check out that it works:

```idris
src/Hardware> the (Word 2) zero
[False, False] : Vect 2 Bool
src/Hardware> the (Word 8) zero
[False,
 False,
 False,
 False,
 False,
 False,
 False,
 False] : Vect 8 Bool
```

What is `the`?

```idris
src/Hardware> :t the
the : (a : Type) -> a -> a
```

It's a function that takes a type and a value of that type and returns itself.
We can use this to constrain the type of `zero` and make it produce the right vector.

# Endianness

When we want to convert a series of `Bool`s into a number, we have to know which end is significant.
We can either start with the least significant bits, known as little-endian, or the most significant, known as big-endian.
A number in little endian format has the following structure:

    bit: 0 0 0 0 0 0 0 0
    2^i: 0 1 2 3 4 5 6 7

It reads backwards of what we might expect.
We can take the first bit, raise it to the power of two appropriate to its position, and add it to the rest of the list recursively converted.
Meanwhile, a big-endian format has this structure:

    bit: 0 0 0 0 0 0 0 0
    2^i: 7 6 5 4 3 2 1 0

That's how we normally read numbers.

Let's start with little-endian:

```idris
littleEndian : Word n -> Nat
littleEndian = helper 0
  where
    helper : Nat -> Word m -> Nat
    helper x [] = 0
    helper x (True :: bs) = (power 2 x) + helper (x + 1) bs
    helper x (False :: bs) = helper (x + 1) bs
```

We don't have to worry about the length of the list because we can just increase the `x` as we pass it along.
We can make this work for big-endian by reversing the list, but that's inefficient.
Instead, we can take advantage of the fact that we know the length of the list in the type.

```idris
bigEndian : Word n -> Nat
bigEndian [] = 0
bigEndian {n = S k} (b :: bs) =
  (if b then power 2 k else 0) + bigEndian bs
```

First, we pattern match on the empty vector, and give 0 as a result.
Then, we pattern match on `b :: bs`, and also assert that `n` is non-zero (that is, the successor of a natural number).
This brings into scope the variable `k`, which we can use.
We either add `0` or `2^k` whether the bit is set or not, and add that to the result of calling `bigEndian` on the rest of the list.

# Adding Two Words

Let's recap: we've built logic gates, zeroes, and a means of converting a `Word n` into a `Nat`ural number.
Nice!
Now, let's implement addition of two vectors using a [ripple carry adder](https://en.wikipedia.org/wiki/Adder_(electronics)#Ripple-carry_adder).

Note that this function only works on little-endian `Word`s because the carry bit flows the wrong direction for a big-endian word.
We would have to reverse the two lists in order to make this operate correctly on big-endian vectors.

```idris
rippleCarry : Word n -> Word n -> Word n
rippleCarry x y = go False x y
  where
    go : Bool -> Vect n Bool -> Vect n Bool -> Vect n Bool
    go carry [] [] = Data.Vect.Nil
    go carry (a :: as) (b :: bs) =
      let (s, c) = fullAdder a b carry
       in s :: go c as bs
```

We're asserting that you can only add two `Word`s of the same length, so when we pattern match in `go`, we don't have to worry about mismatched lengths.

Now, the last challenge: given a `Nat`, it'd be nice to get the `Word n` from it.
We can implement that simply for lists:

```idris
mkWordList : Nat -> List Bool
mkWordList Z = []
mkWordList s@(S k) = ((s `mod` 2) == 1) :: mkWordList (divCeil k 2)
```

`Z` is the empty list.
For the `S`uccessor of any natural number `k`, we see if the `S k` is evenly divisible by 2.
If it is, we have a `True` bit, and otherwise we have `False`.
Then we `cons` that onto the front of the list formed by recursing down.

But what happens when we try for vectors?

```idris
mkWord : (n : Nat) -> Word m
mkWord Z = ?zero
mkWord (S k) = ?succ
```

What is `m` going to be?
We know that, given `x` bits, we can store `2^x` numbers, with the largest being `2^x - 1`.
Idris defines `log2 : Nat -> Nat`, which is the inverse of `power 2 n`.
So `m` should be `S (log2 n)`

Let's put in the `?zero` case now. We'll use the 'empty vector = 0' convention we've been using:

```idris
mkWord (n : Nat) -> Word (S (log2 n))
mkWord Z = []
```

Boom! Type error:

```

Type checking ./src/Hardware.idr
./src/Hardware.idr:98:8:
When checking right hand side of mkWord with expected type
        Word (S (log2 0))

Type mismatch between
        Vect 0 a (Type of [])
and
        Vect (S (log2 0)) Bool (Expected type)

Specifically:
        Type mismatch between
                0
        and
                S (log2 0)
```

Well, `0` doesn't match `S k`, so that's wrong!
Fortunately, we can handle that case directly in the type.
Check this out:

```idris
mkWord : (n : Nat) -> Word (case n of Z => 0; S k => S (log2 (S k)))
mkWord Z = []
```

We can use a `case` statement right there in the type signature.
Pretty awesome, right?
Now we can get to the other case:

```idris
mkWord s@(S k) = ((s `mod` 2) == 1) :: mkWord (k `divCeil` 2)
```

We get another type error here:

```idris
*src/Hardware> :r
Type checking ./src/Hardware.idr
./src/Hardware.idr:99:38-40:
When checking right hand side of mkWord with expected type
        Word (case block in mkWord at ./src/Hardware.idr:97:34 (S k)
                                                               (S k))

When checking argument xs to constructor Data.Vect.:::
        Type mismatch between
                Word (case block in mkWord at ./src/Hardware.idr:97:34 (divCeil k
                                                                                2)
                                                                       (divCeil k
                                                                                2)) (Type of mkWord (divCeil k
                                                                                                             2))
        and
                Vect (log2 (S k)) Bool (Expected type)

        Specifically:
                Type mismatch between
                        case block in mkWord at ./src/Hardware.idr:97:34 (divCeil k
                                                                                  2)
                                                                         (divCeil k
                                                                                  2)
                and
                        log2 (S k)

```

So it appears that Idris can't determine that, by reducing `k` by 2 with each application of the recursion, it'll only take `log2 k` in order to bottom out.
To be proper, we should really prove that this holds.
Perhaps I'm wrong, and my code doesn't work!
Maybe there's a corner case I'm forgetting, and Idris is rightfully blocking me from providing garbage.

Perhaps I have a deadline looming, and I need to Move Fast and (potentially) Break Things.
I'm pretty sure that I didn't screw up too bad, and some playing at the REPL indicates that I'm alright.
Fortunately, Idris is remarkably flexible.
Here's how we can make that typecheck:

```idris
mkWord s@(S k) = believe_me (((s `mod` 2) == 1) :: mkWord (k `divCeil` 2))
```

Idris has an escape hatch `believe_me : a -> b` that allows you to make assertions to the compiler without proving them.
I'm planning on learning more about proving things with Idris so that I can make this work, but for right now, this works for me.

# Further work...

Next time, I'm planning on implementing memory units so we can stop cheating and use a hardware simulation for memory.
Then we can build a processor, some RAM, and have a real hardware simulation going on!
