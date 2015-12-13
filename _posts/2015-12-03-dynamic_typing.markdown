---
title: "Dynamic Typing Will Never Die"
date: 2015-12-03
layout: post
categories: programming
---

## What once was dead may never die

The debate between static and dynamic typing typically assumes that one is a Clear Winner.
Like all debates that feature a false binary, this ignores all of the wonderful shades of grey and interesting trade offs.
There's a lot of subtlety that I feel many treatments miss, so I felt compelled to add my own.
Instead of talking about the Right and Wrong answers, let's instead talk about the tradeoffs.
Let's talk about what is bought, at what price, and whether it's worth it.

## What is a type system?

### Bit Patterns

All values in a computer program have some runtime representation as a binary number.
A character `c` is interpreted differently from an integer `8`, which is interpreted differently from a floating point number `0.325`.
In memory, they're all just series of `0`s and `1`s arranged in specific manners.
The combination of _arrangement_ and _interpretation_ form the abstract idea of a data type.

In assembly language, all you've got are registers which are full of bits.
It's your responsibility to treat them appropriately.
It's the ultimate dynamic language!

C gives us a tiny bit more safety. We can say something like:

```c
char c = 0;
int i = 0;
double d = 0;
```

and C will store the information in `c` in a manner appropriate to a character type.
The same value gets stored in `i`, but C knows to treat it like an integer.
The `d` variable gets `0`, but converts it to a different representation suitable to the double representation.

We can easily cast types from one to the other, with one caveat.
If one type needs more information to represent, and we try to convert it to a smaller type, then we *lose* that information!
In one sense, then, a type system is about how the computer is representing and interpreting our bits of information.

C's type system is very simple and primitive, and trades a little bit of flexibility for a good bit of safety.
We don't have to worry about how to interpret or store our data since the compiler can handle it pretty well.
If we see that a variable has a type of `double`, then we can be reasonably sure that it contains a reasonable value for a `double`.
Since it is so easy to cast, though, we can't be entirely sure.

Since C's type system is so primitive, it's not capable of expressing a lot of ideas.
Sometimes it thinks we're wrong, and it tells us so.
We cast types to shut it up, because we know better.
And in many cases, we do!

In this sense, we can see that the type system is a way of restricting what we can say.

### Objects

In object-oriented languages, the focus is shifted to talking about objects: a more abstract idea than simply 1s and 0s.
OOP languages often include a variety of complex mechanisms around inheritance, interfaces, and other equivalence relations.
Polymorphism becomes a big deal!

Numbers of differing types can be added.
We can `+` two integers, two natural numbers, or two floating point numbers.

The primary effect of a type system is to *reduce the amount of things you can express*.

### Mathematical Notions

From a mathematical perspective, a type is a collection of possible values.
For instance, we say that `Bool` is a type consisting of the two values `{ true, false }`.
`Integer` is a type consisting of all the positive and negative whole numbers.
It doesn't make any se

