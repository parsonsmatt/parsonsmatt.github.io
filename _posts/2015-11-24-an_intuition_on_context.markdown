---
title: "An Intuition on Context I"
date: 2015-11-24
layout: post
categories: programming
---

## Not a monad tutorial!

I don't want to teach you about monads.
Instead, I want to give you some intuition and understanding around what is meant by "a functor/monad is like a context."
This post is intended to be accessible to everyone with a basic knowledge of programming.
I'll use minimal Haskell syntax to express some ideas, and I'll explain the meaning as I go.

We'll start with a way to get information from types, and then we'll use that to determine how much information can be stored in a context.
In another post, I'll cover what a functor is, and how it relates to the idea of a context, and how to extend that to applicative and monad.

## Types as numbers

If you're not familiar with the formal notion of a set, a set is an unordered collection of unique things.
The set `{ 1, 2, 3 }` is equivalent to `{ 3, 1, 2 }` and `{ 3, 3, 2, 1 }` (since the 3 really only gets counted once).
They're different from arrays and lists, which have an ordering and allow duplicate items.
For most purposes, we can think of a type as a set (though this idea breaks down with further scrutiny).

We can represent the type `Boolean` as a set of possible values `{ True, False }`.
When we say "This variable has the `Boolean` type," we're saying "This variable can have a value that is in the set of Boolean values."

Regardless of what is inside the set, we can count the number of things in a set.
In this way, we can treat a set as a number.
We've already covered the number 2: `Boolean`!
The number 1 can be represented with the unit type.
In Haskell, this is referred to as `()`, and it only has one value, also written as `()`.

We get 0 from the `Void` type.
There are no values of the void type.
Sometimes, the word 'value' doesn't exactly fit what we mean -- and the word 'inhabitant' gets used instead.

We'll want 3 for some later examples, so we'll use the following type/set and values for that purpose:

```haskell
data Circuit = High | Low | Disconnected
```

This is a Haskell declaration that says:

> I'm creating a new type called `Circuit` with three possible data constructors: `High`, `Low`, `Disconnected`.

This type represents digital circuits, which can either be `High` voltage (a 1), `Low` voltage (a 0), or `Disconnected` entirely.

As a refresher, here's a table on the numbers we have so far, their Haskell declarations, and their set descriptions:

- 0
  - Void = {}
  - `data Void`
- 1
  - Unit = { unit }
  - `data () = ()`
- 2
  - Boolean = { true, false }
  - `data Bool = True | False`
- 3
  - Circuit = { High, Low, Disconnected }
  - `data Circuit = High | Low | Disconnected`

## Generics and Higher Kinded Types

Above, we talked about how we can determine the size of a set regardless of what it contains.
It's so often useful to talk about containers irrespective of their contents that virtually all modern programming languages have some facility for generics.
Java allows you to define a generic class with `<T>` notation.
Here's an example of an immutable singly linked list class:

```java
public class List<T> {
  final T       head;
  final List<T> rest;

  private List(T val, List<T> rest) {
    this.head = val;
    this.rest = rest;
  }

  public static List<T> Nil() {
    return new List<T>(null, null);
  }

  public static List<T> Cons(T val, List<T> rest) {
    return new List<T>(val, rest);
  }
}
```

where the `T` is a type variable, allowing you to instantiate a `List<Integer>` or `List<String>`.
We're only making the `Nil` and `Cons` static methods public to help ensure that it's constructed correctly.

Haskell's own system is quite a bit more powerful and less verbose.
If we want to declare a `List` type that takes a single type parameter, analogous to the above, we'd do:

```haskell
data List a
  = Nil
  | Cons a (List a)
```

This data declaration is a bit more complex than the `Circuit` declaration.
The `data` keyword creates a *type constructor*.
`Circuit` is a type constructor that takes 0 type arguments.
`List` is a type constructor that takes 1 type argument.

Next up, we create two *data constructors*.
The circuit definition had three data constructors, none of which took any arguments.
The `List` definition has two: the first being `Nil`, and represents an empty list.
The second is `Cons` which has two *fields* -- the first is a value of type `a`, and the second is a value of type `List a`.

So, in Java and Haskell, we can now construct singly linked lists like this:

```java
List<Integer> l = Cons(1, Cons(2, Cons(3, Nil())));
```

```haskell
l :: List Integer
l = Cons 1 (Cons 2 (Cons 3 Nil))
```

In Haskell, you apply arguments to things by putting them next to each other.
So `List` is a type constructor, and `List Integer` is the type `Interger` applied to the type constructor `List`.
Likewise, `Cons 3 Nil` is the data constructor `Cons` with the values `3` and `Nil` applied to it.

Let's simplify, though.
List has two fields, one of which is recursively defined.
That's a little tricky for what we're working with right now.
We can get a lot simpler.
The `Identity` type will suffice as our simplest thing:

```haskell
data Identity a = Identity a
```

It doesn't *do* anything.
It just sits there, referencing a single type, and containing a single value of that type.

We'll also want to be able to represent *choice*: "I have *either* this *or* that".

```haskell
data Either a b
  = Right b
  | Left a
```

When I see "or", I immediately want to be able to say "and".
We can say "I have this *and* that" with a pair, or tuple:

```haskell
data (a, b) = (a, b)
```

Now, we're ready to start exploring some ~TYPE MATH~.

## Type Math

We have numbers, so we can probably do some sort of math with them!
The notion of sets/types as numbers seems to only really apply to the counting numbers.
It doesn't make much sense to think about a set with -1 elements.
Likewise, it doesn't make much sense to think about a set with 3.14 elements.

Natural numbers can be added.
From addition, we can derive multiplication, and from multiplication, we can derive exponentiation.
As it happens, this applies to types, too!

### Addition

We've already seen how to do addition with types.
The `|` symbol is a way of adding another value to a type.
We can explore that with a new definition:

```haskell
data BoolOrCircuit
  = B Bool
  | C Circuit
```

We've declared a *type constructor* `BoolOrCircuit`, and two new *data constructors*.
`B` takes an argument of type `Bool`, and `C` takes an argument of type `Circuit`.

How many elements are in `BoolOrCircuit`?
Well, we have two possibilities on the `B` side: `B True` and `B False`.
On the `C` side, we have `C High`, `C Low`, and `C Disconnected` for five total values.
2 + 3 = 5, so this checks out!
The single-argument constructor `C` has as many values as the type of the argument,
and the `|` allows us to add the count of values together.

Since we arrive at the total number of values by summing the values of each constructor,
types like this are known as *sum types*.

We can use `Either` to represent this without requiring a new data type:

```haskell
type BoolOrCircuit2 = Either Bool Circuit
```

### Multiplication

What if we have multiple arguments?

```haskell
data BoolAndCircuit
  = BC Bool Circuit
```

`BC` here takes two arguments, the first is a `Bool` and the second is a `Circuit`.
We can enumerate all the possible values:

```haskell
BC True  High
BC False High
BC True  Low
BC False Low
BC True  Disconnected
BC False Disconnected
```

And... there are six!
This generalizes to an arbitrary number of elements -- when we take multiple arguments, we can know the total possible values of the type by taking the *product* of the values of each type.
That's what is meant when people say "product type."

As with `Either` being the general sum type, we can use the general product type to make this:

```haskell
type BoolAndCircuit = (Bool, Circuit)
```

### Exponentiation

Exponentiation is a little trickier.
We can add two types with `Either a b`.
And we can multiply them with `(a, b)`.
How can we raise the type `a` to the power of `b`?

The answer is functions!

```haskell
type Exponent b a = a -> b
```

If we use the `TypeOperators` language extension, we can do:

```haskell
type b :^ a = a -> b
```

which makes the relation more clear.

Let's think about the unit type -- x^1 is always equal to x, and 1^x is always equal to 1.
So `() :^ Bool` is like 1^2 which should have only one possible implementation.
Let's unpack and implement it:

```haskell
fn :: () :^ Bool ~ Bool -> ()
fn True  = ()
fn False = ()
```
In fact, there's no other possible way to do this.
No matter the input, we'll always return `()`.

`Bool :^ ()` is 2^1, which is equal to 2. So there are two possible implementations of the type signature:

```haskell
fn1 :: Bool :^ () ~ () -> Bool
fn1 () = True

fn2 :: Bool :^ () ~ () -> Bool
fn2 () = False
```

If we consider `Bool :^ Bool`, we'll see that there are 4 possible implementations of the function.
`Bool :^ Circuit` has 8 possible implementations, and `Circuit :^ Bool` has 9 possible implementations.

[This blog post series](https://chris-taylor.github.io/blog/2013/02/10/the-algebra-of-algebraic-data-types/) presents these ideas with much more information and rigor than I do, and if you find it interested, I'd recommend you check it out!
We now have enough background information on types to talk about how they can contain information!

## Information in the context

Alright, so let's talk about some contexts and the information contained therein.
`Identity` is the simplest context.
It is the context of identity, of sameness.
There is no extra information here.

`Maybe` gives us some more information!
It looks like this:

```haskell
data Maybe a
  = Just a
  | Nothing
```

`Just` is essentially `Identity`, so the `Just` constructor doesn't add any information to `a`.
`Nothing`, however, is added to it, so the type `Maybe a` has `1 + a` inhabitants.

`List`, likewise, gives us even more information: in addition to the elements,
we have them in a linear order, *and* we have a count of how many elements there are in the list.

How many inhabitants do lists have?
For each element in the list, we have `a` possible values.
So a list of size 0 has 1 value: the empty list.
A list of size 1 has `a` values.
A list of size 2 has `a * a` values.
A list of size 3 has `a * a * a` values, and we end up with the sequence:

    1 + a + a^2 + a^3 + a^4 + ...

which is also:

    a^0 + a^1 + a^2 + a^3 + ...

If you read the blog post I linked above, then you'll get to see how this result can be *derived* from type arithmetic in a very cool way.

Lists also keep another bit of information around: an idea of sequence, or order, of the elements.
So the list context has two bits of information: how many elements, and in what order.

There are two more common contexts.
We'll get to see how added information gives us more power and also more complexity.

### Reader

Reader is a context where we have some read-only environment information.
The `Reader` type in Haskell is defined like:

```haskell
newtype Reader r a 
  = Reader 
  { runReader :: r -> a 
  }
```

Here, we're defining a new type, calling it `Reader`.
We know how to compute the possible implementations of a function-arrow -- exponentiation!
So for a `Reader r a` function, we know we have `a ^ r` implementations.

Adding read-only information to a function, then, exponentially increases the possible ways for the function to work.

### State

Stateful computation is modeled in Haskell as a function that takes some state as input, and produces a result value and a new state.
We define it like:

```haskell
newtype State s a
  = State
  { runState :: s -> (a, s)
  }
```

This is similar to the definition of Reader above, but we're also returning a new value of our state type.
How many inhabitants does this type have?
It's a bit trickier than Reader, but we've got all we need to figure it out.
We have a pair, so we'll multiply `a*s`.
We have a function, so we'll exponentiate: `(a*s)^s`.

Adding mutable state to a function multiplies the value's inhabitants by the state's inhabitants and then raises *that* to the power of the state's inhabitants.
Yikes.
Considering `State Circuit Bool`, we've got `(Bool, Circuit) :^ Circuit`, which translates to `(2 * 3) ^ 3 = 216`.
Talk about a huge increase in complexity!

## Next time...

We've now got a bit of an idea on how basic types and functions can carry information, and can use that to figure out how much information is stored in a generic data type.
We can also think about these generic data types as contexts which add some information to the types they contain.
In the next post, we'll Level Up our ability to *use* these contexts in a generic and reusable way.

[Here's the link to the next post](https://www.parsonsmatt.org/2015/11/29/an_intuition_on_context_ii.html)
