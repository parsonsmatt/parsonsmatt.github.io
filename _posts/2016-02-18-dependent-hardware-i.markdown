---
title: "Dependendently Typed Hardware"
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
| 1 | 1 | 1     | 0       | 00    |
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
The Idris implementation
