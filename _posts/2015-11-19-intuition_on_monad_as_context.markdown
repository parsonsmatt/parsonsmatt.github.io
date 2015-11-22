---
title: "Intuition on Monad as Context"
date: 2015-11-19
layout: post
categories: programming
---

## Not a monad tutorial!

I don't want to teach you about monads.
Instead, I want to give you some intuition and understanding around what is meant by "a monad is like a context."
This post is intended to be accessible to everyone with a basic knowledge of programming.
I'll use minimal Haskell syntax to express some ideas, and I'll explain the meaning as I go.


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

Alright, so let's talk about some contexts and the information contained.
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

Here, we're defining a new type, calling it `Reader`
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

This is similar to the above, but we're all returning a new value of our state type.
How many inhabitants does this type have?
It's a bit trickier than Reader, but we've got all we need to figure it out.
We have a pair, so we'll multiply `a*s`.
We have a function, so we'll exponentiate: `(a*s)^s`.

Adding mutable state to a function multiplies the value's inhabitants by the state's inhabitants and then raises *that* to the power of the state's inhabitants.
Yikes.
Considering `State Circuit Bool`, we've got `(Bool, Circuit) :^ Circuit`, which translates to `(2 * 3) ^ 3 = 216`.
Talk about a huge increase in complexity!

## Functor

A functor is a combination of some context and a function `fmap` that allows us to lift a function into that context.
The `fmap` function can't access any information in the context, and it's not allowed to change the context.
A more formal way of expressing that are the *Functor Laws*:

1. `fmap id === id`
2. `fmap (f . g) === fmap f . fmap g`

(where `id` is the identity function and returns its argument unchanged)

We can write a `fmap` for all of the above contexts:

```haskell
fmap :: (a -> b) -> List a -> List b
fmap _ Nil              = Nil
fmap f (Cons head rest) = Cons (f head) (fmap f rest)

fmap :: (a -> b) -> Either e a -> Either e b
fmap _ (Left e)  = Left e
fmap f (Right a) = Right (f a)

fmap :: (a -> b) -> (e, a) -> (e, b)
fmap f (e, a) = (e, f a)
```

We're not used to thinking of functions as context, so Reader and State might seem tricky at first.
We'll do a bit of type-driven development to figure out what we need exactly.
I'll start by pattern matching on the `Reader` data constructor, and I know we'll need a `Reader` constructor at the end, so we can put that together.

```haskell
fmap :: (a -> b) -> Reader r a -> Reader r b
fmap f (Reader readerFn) = Reader (\env -> _)
```

That `_` has the type `b`, as GHC happily tells us.
So `readerFn` is a function with the signature `readerFn :: r -> a`.
And `env` is a value of type `r`.
Finally, we have `f`, a function with signature `f :: a -> b`.
If we do `readerFn env`, that gives us a value of type `a`, and we can do `f (readerFn env)` to get a value of type `b`.

```haskell
fmap :: (a -> b) -> Reader r a -> Reader r b
fmap f (Reader readerFn) =
  Reader (\env -> f (readerFn env))
```

Note that the function we're mapping over `Reader` doesn't get to see the environment.
It has no access to the *context* of the computation, just the result value.

Finally, `State` is like:

```haskell
fmap :: (a -> b) -> State s a -> State s b
fmap f (State stateFn) =
  State (\s -> 
    let (a, s') = stateFn s
     in (f a, s')
  )
```

We take the old state function, and make a new state function.
This new function first applies the state to the old state function, and gets the new state and result out.
It applies the function `f` to the result of the stateful computation.

Like `Reader`, the `f` function is completely unaware of the state.


## Applicative

The fmap function that functors get is pretty limited.
It doesn't get to know anything about the context, so all it can do is transform each element.
The Applicative class introduce the `<*>` operator, pronounced "apply".
The `<*>` allows us to start taking advantage of the information present in the context.

We also get `pure` -- a generic way to lift something into the Applicative context.
`pure` and `<*>` have to follow a big rule: 

```haskell
pure f <*> a === fmap f a
```

Let's start with the basics: if we have `Identity (a -> b)` and `<*>` it with `Identity a`, then we'll get an `Identity b` back.
There's no extra information to consider, so we don't have anything extra to *do* with our applicative powers.
Check out the implementation:

```haskell
instance Applicative Identity where
  pure = Identity
  Identity f <*> a = fmap f a
```

For `Maybe`, we gain a tiny bit of new power.
With `Maybe a`, we have the information contained in `a`, and one extra bit of information: `Nothing`.

```haskell
instance Applicative Maybe where
  pure = Just
  Just f  <*> m = fmap f m
  Nothing <*> _ = Nothing
```

With Functor, we definitely had to have a function to fmap over the value.
With Applicative, we can potentially do `Nothing <*> Just 'a'`, which results in `Nothing`.
We've gained a way to change the structure!

Functors weren't allowed to change the structure of the thing being mapped over.
Applicatives are.
When we're applying, we get a new bit of information, and we'll want to find out how to use that information effectively.

Let's consider `List`, now.
We have two bits of information: the number of elements in a list, and the order of elements in the list.
If we use the number of elements in the list as our extra information, then we can combine the number of elements in some way in the result list.
If we use the order of elements, then we can zip the two lists together with function application, pairing the function at index `i` with the value at index `i`.
Let's implement both!

For a ZipList, we have to remember that `pure f <*> a === fmap f a` when considering our `pure` definition.
If we have a single function that we want to zip a list with, then we need to make an infinite list of that function.

```haskell
newtype ZipList a = ZipList { unZipList :: [a] }

instance Applicative ZipList where
  pure a = ZipList (repeat a)
```

The only sensible behavior when zipping two lists together is to truncate the result list to the length of the shorter list, so we'll return `Nil` if we reach the end of either list.

```haskell
  ZipList Nil <*> _           = Nil
  _           <*> ZipList Nil = Nil
```

Finally, if we have a function and an element, we apply the function to the element and continue zipping.

```haskell
  ZipList (Cons f fs) <*> ZipList (Cons a as) =
    ZipList (Cons (f a) (fs <*> as))
```

Here, we're using the *ordering* of the lists to inform our application.
If either list is shorter than the other, then we truncate the list.
While we're using the ordering of the lists to do application, we're not *changing* the ordering of the list.
This doesn't seem like we're fully taking advantage of the information present in the list.

Perhaps if we use the *count* of elements rather than their order, we can have a more powerful impact.

Since the length of the list is a number, we can potentially do numeric operations with the two numbers.
We can't have negative or fractional numbers, so we're back to natural numbers, and that means we can choose between addition, multiplication, and exponentiation.
We also have to consider that `pure f <*> as === fmap f as`.

For that reason, we're limited in which operation we can choose.
A `pure f <*> as` can't change the length of the list, otherwise it violates the law above.
That means that, whatever operation we do, the `pure` function must correspond to a unit.
There is another law which states that applicatives must be associative, which rules out exponentiation.
So the combination of the *operation using the information of the context* and the *means of lifting a value* form a monoid.

With `+`, the unit is `0`, and a list of length `0` is `Nil`.
`pure _ = Nil` does not satisfy `pure f <*> as === fmap f as`, so `+` can't be used.
What about multiplication?

Multiplication with natural numbers forms a monoid, where the unit is 1.
This means that our `pure` function will return a list of length 1.

```haskell
pure :: a -> List a
pure a = Cons a Nil
```

We'll multiply the lengths of the two lists together to form the new list.
For `<*>`, note that `Nil` corresponds to 0, and 0*x = 0.
If either argument is Nil, then we return `Nil`.

```haskell
(<*>) :: List (a -> b) -> List a -> List b
Nil <*> _   = Nil
_   <*> Nil = Nil
```

If we have a single function, this corresponds to `pure f`, and so must be equivalent to `fmap f`.

```haskell
Cons f Nil <*> Cons a as =
  Cons (f a) (fmap f as)
-- or, since fmap/<*> neeed to be equivalent,
  Cons (f a) (pure f <*> as)
-- or,
  Cons (f a) (Cons f Nil <*> as)
```

if we have multiple functions... then we'll need to fmap each function over the target list and combine the results.

```haskell
fs <*> as =
  concatMap (\f -> fmap f as) fs
```

We've now mathematically derived both of the Applicative instances for lists, based on an understanding of the information content available in the type.

Woah!

What about Reader and State?
How can they take advantage of their contextual information to do neat stuff?

Let's review `Reader`, and get the `Applicative` instance:

```haskell
newtype Reader r a = Reader { runReader :: r -> a }

instance Applicative (Reader r) where
  pure a = Reader (\_ -> a)
  rf <*> ra =
    Reader (\r ->
      let f = runReader rf r
          a = runReader ra r
       in f a
    )
```

This allows the two functions to *share* an environment context.
How much information is contained here?

The `rf` value is `Reader r (a -> b)`,
which is a newtype for `r -> (a -> b)`,
which is equivalent to `r -> a -> b`,
which we can express as `(a -> b) :^ r`,
or `(b :^ a) :^ r`.
If we've got `Reader Circuit (Bool -> Circuit)`, then that's
(3^2)^3 = 729 possible implementations.

In concrete implementations of the functions, however, we're going to see essentially three paths:

```haskell
readerFn :: Circuit -> (Bool -> Circuit)
readerFn High =
  \b -> if b then Low else High
readerFn Low =
  \b -> if b then Disconnected else Low
readerFn Disconnected =
  \b -> if b then Disconnected else High
```

Reader, in this case, seems to be a set of contingency plans.
"If the environment is high voltage, then do this.
If it is low voltage, then do that.
If the environment is disconnected, then do this other thing."
An *applicative* chain of Readers is like a decision tree, where the path through the tree is entirely decided by a single decision.

We can write a function to express travel and dress plans based on the weather like this. Take:

```haskell
data Weather = Sunny | Raining | Cold
data Transport = Bike | Car
data Clothing = BigCoat | Jacket | StretchyPants

chooseTransport :: Reader Weather Transport
chooseTransport =
  Reader (\env ->
    case env of 
         Sunny -> Bike
         Cold -> Bike
         Raining -> Car
  )

chooseClothing :: Reader Weather Clothing
chooseClothing =
  Reader (\env ->
    case env of
         Sunny -> StretchyPants
         Raining -> Jacket
         Cold -> BigCoat
  )

data Plan = Plan Transport Clothing

whatDo :: Weather -> Plan
whatDo =
  runReader (Plan <$> chooseTransport <*> chooseClothing)
```


We provide an environment, and this makes the choice for *all* of the Reader functions.

How about State?

```haskell
newtype State s a = State { runState :: s -> (a, s) }

instance Applicative (State s) where
  pure a = State (\s -> (a, s))
  sf <*> sa =
    State (\s ->
      let (f, s')  = runState sf s
          (a, s'') = runState sa s'
       in (f a, s'')
    )
```

We thread some state through, allowing the `sf` state function to alter the state as necessary before passing it into the `sa` state function.
Finally, we apply the resulting function to the resulting value.

Reader had 729 implementations, but that didn't really affect much.
Instead, it was nicer to think about it in terms of a decision tree that was ruled by a single decision.

State is similar, but since we are able to update the state as we go along, we're able to build a decision tree on the fly, where each function in the applicative sequence gets to update the state.

{{ write an example where the state changes }}

To wrap up, a functor is a context where we can map over, but we can't look at the context, and we can't change the context.
An Applicative allows our contexts to interact independently of the values contained in the contexts.
This is a lot more power! 
But it's still perhaps a bit more limited than we might want.

What if we want to alter the context based on the values produced by the computation?

## Monad

## fmap, Join, Return
