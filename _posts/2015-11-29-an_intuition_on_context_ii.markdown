---
title: "An Intuition on Context II"
date: 2015-11-29
layout: post
categories: programming
---

## Recapping...

[The previous post](http://www.parsonsmatt.org/2015/11/24/an_intuition_on_context.html) discussed how information can be stored in types.
This information forms a "computational context" that we can take advantage of in new and interesting ways.
Now, we're going to see how that relates to the functor/applicative/monad stuff.

## Functor

A functor is a combination of some context and a function `fmap` that allows us to lift a function into that context.
The `fmap` function can't access any information in the context, and it's not allowed to change the context.
A more formal way of expressing that are the *Functor Laws*:

1. `fmap id === id`
2. `fmap (f . g) === fmap f . fmap g`

(where `id` is the identity function and returns its argument unchanged)

The functor type class definition is presented here:

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
  -- or, since function arrows associate to the right,
  fmap :: (a -> b) -> (f a -> f b)
```

We define a function that takes a normal function as input, and returns a new function that operates over the context.

We can write an `fmap` for all of the contexts in the prior post:

```haskell
fmap :: (a -> b) -> List a -> List b
fmap _ Nil              = Nil
fmap f (Cons head rest) = Cons (f head) (fmap f rest)

fmap :: (a -> b) -> Maybe a -> Maybe b
fmap _ Nothing  = Nothing
fmap f (Just a) = Just (f a)

fmap :: (a -> b) -> Either e a -> Either e b
fmap _ (Left e)  = Left e
fmap f (Right a) = Right (f a)

fmap :: (a -> b) -> (e, a) -> (e, b)
fmap f (e, a) = (e, f a)
```

There's an intuition of functors/monads as *containers*, and this intuition works for the above types.
The intuition breaks down when we start thinking about Reader and State, which are functions.
The 'container' intuition also breaks down with certain containers, like `Set`.
Thinking of a function as a container is pretty difficult, and there are other functors where the container metaphor breaks down completely.

First, why isn't `Set` a valid functor?
A Set is an unordered collection of elements where duplicates are discarded.
"Duplicates are discarded" sounds like a likely plan of attack!
[Michael Snoyman's code snippet](https://www.schoolofhaskell.com/user/chad/snippets/random-code-snippets/set-is-not-a-functor) shows the simplest case.
This violation of the Functor laws is enough to say that a Set can't be a functor, despite being a container.

Now, for the function functors!
Let's define `fmap` for Reader.
It's not as trivial as the above, where we just pattern match directly on the constructor and apply the `f`.
We'll take a little detour of using typed-holes to discover the implementation.
We know already that we'll be constructing a `Reader` as the return type, so we can go ahead and fill that in.

```haskell
fmap :: (a -> b) -> Reader r a -> Reader r b
fmap f readFn = Reader _f
```

That `_f` has the type `r -> b`, as GHC is happy to tell us.
`readFn` is a `Reader r a`, and we've got a `runReader` function with the signature `Reader r a -> (r -> a)`.
Lastly, we have an `a -> b`.
We'll introduce an `r` using a lambda!

```haskell
fmap f readFn = Reader (\r -> _f)
```

Now we can do `runReader readFn` to get an `r -> a`, apply the `r` to the function to get an `a`, and apply `a` to the `f` to get a `b`.

```haskell
fmap f readFn = Reader (\r -> f (runReader readFn r))
-- or,
fmap f readFn = Reader (f . runReader readFn)
```

Note that the function we're mapping over `Reader` doesn't get to see the environment.
It has no access to the *context* of the computation, just the result value.

`State` is like:

```haskell
fmap :: (a -> b) -> State s a -> State s b
fmap f oldState =
  State (\stateValue -> 
    let (result, newStateValue) = runState oldState stateValue
     in (f result, newStateValue)
  )
```

We start with the `State` constructor, and since we know we need a function of `s -> (b, s)` at the end, we introduce the lambda.
We run the `oldState` with the `stateValue` from the lambda, and apply the `f` to the result in the return tuple.

The `f` function in all of these functors is completely unaware of the context.
It's not allowed to alter the context, and it's not allowed to be informed by the context.

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

The class definition looks like this:

```haskell
class (Functor f) => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
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
Additionally, being able to apply a function in a functor to values in a functor lets us curry things and work across many values.
The idiom is very common, and uses the fmap infix operator `<$>`.
If we have some function that takes a bunch of parameters, and a bunch of `Maybe` values, we can apply the function over the Maybes and get a result back only if they're all `Just`.

```haskell
data SomeType = SomeType Int String Char

isJust = 
  SomeType <$> Just 2 <*> Just "ASdf" <*> Just 'a'
-- Just (SomeType 2 "ASdf" 'a')

isNothing = 
  SomeType <$> Just 6 <*> Nothing <*> Just 'b'
-- Nothing
```

The contextual application says "If any of these contexts is `Nothing`, then the whole context is Nothing."

The `Either` functor has a similar Applicative instance.
There's a functor very similar to `Either` called `Validation` which has an interesting `Applicative` instance.
Here is the definition and Functor instance:

```haskell
data Validation e a 
  = Correct a
  | Errors [e]

instance Functor (Validation e) where
  fmap f (Correct a) = Correct (f a)
  fmap _ (Errors e)  = Errors e

instance Functor (Either e) where
  fmap f (Right a) = Right (f a)
  fmap _ (Left e)  = Left e
```

They look nearly the same.
For reasons that we'll come to when looking at monads, `Validation` cannot form a monad, while `Either` can.
Despite this, Validation has some nice properties that Either can't have.

```haskell
instance Applicative (Validation e) where
  pure = Correct
  Correct f <*> val       = fmap f val
  Errors as <*> Correct _ = Errors as
  Errors as <*> Errors bs = Errors (as ++ bs)

instance Applicative (Either e) where
  pure = Right
  Right f <*> a = fmap f a
  Left e  <*> _ = Left e
```

The `Either` Applicative short circuits when it gets a `Left` value.
It discards everything that comes next.
The `Validation` applicative collects all the error messages in a list.
Let's see how this plays out:

```haskell
data Person = Person Int String

type Error = String

valGood :: Validation Error Person
valGood = Person <$> Correct 27 <*> Correct "Matt"
-- Correct (Person 27 "Matt")

eitherGood :: Either Error Person
eitherGood = Person <$> Right 25 <*> Right "Alice"
-- Right (Person 25 "Alice")

valError :: Validation Error Person
valError = Person <$> Errors ["Age too low"] <*> Errors ["Empty name"]
-- Errors ["Age too low", "Empty name"]

eitherError :: Either Error Person
eitherError = Person <$> Left "Age too low" <*> Left "Empty name"
-- Left "Age too low"
```

## The List Applicative

Let's consider `List`, now.
We have two bits of information in the context of the list: the number of elements in a list, and the order of elements in the list.
If we use the number of elements in the list as our extra information, then we can combine the number of elements in some way in the result list.
If we use the order of elements, then we can zip the two lists together with function application, pairing the function at index `i` with the value at index `i`.
Let's implement both!

For a ZipList, we have to remember that `pure f <*> a === fmap f a` when considering our `pure` definition.
If we have a single function that we want to zip a list with, then we need to make an infinite list of that function.
Otherwise, we might come up short, and then we'd truncate the list, and that would break the law.

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
We can't have negative or fractional lengths of lists, so we're back to natural numbers, and that means we can try addition and multiplication.
Exponentiation might be a thing, but let's not get too crazy.
We also have to consider that `pure f <*> as === fmap f as`.

A `pure f <*> as` can't change the length of the list, otherwise it violates the law above.
This means that our operation (`+` or `*`) determines how `pure` behaves.
If you're familiar with the idea of a monoid, you might notice that this seems really familiar.
In fact, one approach to Applicatives is that they are a: 

> \*drumroll please\*



> (you don't actually need to know this, but the paper might be fun!)



> [strong lax monoidal endofunctor](http://strictlypositive.org/Idiom.pdf)

A monoid is the combination of an associative operator `<>` and an identity element (called unit, or `mempty` in Haskell), where the following laws hold:

- `unit <> x === x` and `x <> unit === x`
- `x <> (y <> z) === (x <> y) <> z`

Fortunately, addition and multiplication both form a monoid with natural numbers. Observe!

- `1 * x === x`, `x * 1 === x`, and `x * (y * z) === (x * y) * z`
- `0 + x === x`, `x + 0 === x`, and `x + (y + z) === (x + y) + z`

So the combination of the *operation using the information of the context* and the *means of lifting a value* form a monoid.

With addition, the unit is `0`, and a list of length `0` is `Nil`.
So, our `pure` function with the `+` operation would be:

```haskell
pure _ = Nil
```

Unfortunately, this does not satisfy `pure f <*> as === fmap f as`, so `+` can't be used.
Our next choice is multiplication, which has a unit of 1.
This means that our `pure` function will return a list of length 1.

```haskell
pure a = Cons a Nil
```

We'll multiply the lengths of the two lists together to form the new list.
For `<*>`, note that `Nil` corresponds to 0, and 0*x = 0.
Therefore, if either argument is Nil, then we return `Nil`.

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
```

if we have multiple functions... then we'll need to fmap each function over the target list and combine the results.

```haskell
fs <*> as =
  concatMap (\f -> fmap f as) fs
```

We've now mathematically derived both of the Applicative instances for lists, based on an understanding of the information content available in the type.
As it happens, this second method is really handy for modeling non-deterministic computation, if we imagine a list as being a bunch of possible values that the variable can take on.

If we want to add two numbers, but we're not sure exactly which numbers we have, we could do:

```haskell
(+) <$> [1, 4, 3] <*> [6, 8, 7]
```

representing that our first number could be either 1, 4, or 3, and our second number could be 6, 8, or 7.
Since we don't know exactly which numbers we have, we want to get all possible results.

What does this end up looking like? Well, `<$>` is map, so we'll do `+` to all the elements in the first list.

```haskell
[(1 +), (4 +), (3 +)] <*> [6, 8, 7]
```

Now, we pair of each function with each possible element in the result list, and calculate all of the possibilities for the number.

## The Reader Applicative 

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
An *applicative* chain of Readers is like a single contingency plan, where the `r` value in `Reader r a` determines the entire plan.

As a bit of an aside, since the `r` value can't be changed, then `<*>` with Reader is commutative -- the ordering of `<*>` doesn't matter as far as the end result is concerned.
This means we can potentially execute all the `Reader` functions in parallel without changing the return calculation.
If the various functions had dependencies on each other, then we wouldn't be able to execute them in parallel.
If we can structure our code like this, then we could possibly easily guarantee that the code we write could be executed in parallel!
Later, we'll learn that adding `Monad` powers to `Reader` are precisely what make the guarantee of parallelism impossible.

Let's do an example of the Reader Applicative.
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


## The State Applicative 

How about State?

```haskell
newtype State s a = State { runState :: s -> (a, s) }

instance Applicative (State s) where
  pure a = State (\s -> (a, s))
```

`pure`, for a State Applicative, says "Whatever state value you end up giving me, I'll return this value."

```haskell
  (<*>) :: State s (a -> b) -> State s a -> State s b
  sf <*> sa =
    State (\s ->
      let (f, s')  = runState sf s
          (a, s'') = runState sa s'
       in (f a, s'')
    )
```

Alright! So first, we construct our `State` value.
We use a lambda to introduce the first `s` state value.
We run the `sf` State to get the `f` function and the new `s'` state.
We run the `sa` State to get the `a` value, and another new `s''` state value.
Finally, we apply the function to the value and return the `s''` state.

Reader had 729 implementations, which tells us that the complexity expands a lot.
It didn't really give us much intuition about how the context worked.
Instead, it was nicer to think about it in terms of a set of plans that shared a single decision.

State is similar, but since we are able to update the state as we go along, we're able to build a decision tree on the fly, where each function in the applicative sequence gets to update the state.
This allows previous stateful things in the application context to have an effect on decisions that happen later.
Let's revisit the weather travel plans and use a stateful approach.

State is pretty awkward to use without the `get` and `put` functions, so we'll define those and discuss their utility.
I'll also cheat a little bit in the example by using `do` notation for the decision functions, but they won't do anything an Applicative couldn't do.

```haskell
get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State (\_ -> ((), s)
```

`get` lets us retrieve the state we're passed easily, and `put` lets us set a new state and ignore the old one entirely.
Ok, on to the example:

```haskell
type Decision a = State WeatherState a
type WeatherState = (Weather, Grumpiness)
type Grumpiness = Int

data Plan = Plan Transport Clothing Food

data Food = HotSoup | SubSandwich | IceCream

chooseTransport' :: Decision Transport
chooseTransport' = do
  (weather, grumpy) <- get

  if grumpy > 5 then
    return Car
  else
    case weather of
         Sunny -> do
           put (weather, grumpy - 2)
           return Bike
         _ -> do
           put (weather, grumpy + 6)
           return Car
         
```

So if our grumpiness level is over 5, we just take the car.
Otherwise, if the weather is Sunny, we'll reduce our grumpiness a bit, and take the bike.
If the weather is anything else, we upgrade grumpiness and take the car.

```haskell
chooseClothing' :: Decision Clothing
chooseClothing' = do
  (weather, _) <- get
  return (runReader chooseClothing weather)
```

Here, we just use the same plan on the `Reader`.

```haskell
chooseFood :: Decision Food
chooseFood = do
  (weather, grumpy) <- get

  if grumpy > 7 then do
    put (weather, grumpy - 1)
    return IceCream
  else
    case weather of
         Cold -> return HotSoup
         _ -> return SubSandwich

```

Now, if we're really grumpy, then we'll eat ice cream.
That cheers us up, so update our grumpiness.
Otherwise, we get a soup or sandwich depending on the weather.

```haskell
makePlan :: Decision Plan'
makePlan = Plan' <$> chooseTransport' 
                 <*> chooseClothing' 
                 <*> chooseFood

runPlan :: WeatherState -> Plan'
runPlan state = evalState makePlan state 
```

We're using the Applicative instance in `makePlan`.
So far, we've been working with the intuition that applicative application allows contexts to interact in the final value.
The context of State is the stateful information surrounding the computation.
Using functor to map over the stateful computation didn't touch the state, it just altered the return value.
Applicative application, on the other hand, altered the state that was being passed around.
The sequencing of function matters here, and unlike Reader, we're not free to reorganize the functions however we please.
That means we couldn't let the computer easily decide how to parallelize the operations!

We've seen that `Either` had to short-circuit, and only pick up the first error, while `Validation` can collect *all* of the errors, and this behavior is more useful in some contexts.
`Either` can be made into a Monad, while `Validation` can't be.
Reader (and similar structures) can be easily parallelized, while `State` can't be.
There are trade-offs here -- sometimes, when we add more power, we lose something else.

To summarize Applicatives and Functors: a functor is a context where we can map over, but we can't look at the context, and we can't change the context.
An Applicative allows our contexts to interact independently of the values contained in the contexts.

What if we want to alter the context based on the values produced by the computation?

## The Dread Pirate Monad

For instance, if the result of `chooseTransportation` is `Bike`, then I need to wear bike-suitable clothing, regardless of the weather.
We could model that with Applicative, but we'd have to keep track of every intermediate result in our state, and that sounds pretty awful.
State is a pretty powerful functor, though.
Weaker ones like Reader, List, and Maybe can't simulate that level of decisive power.

Let's review the signatures for the class functions we've seen so far:

```haskell
fmap  ::   (a -> b) -> f a -> f b
(<*>) :: f (a -> b) -> f a -> f b
```

`fmap` lifts a normal function into the `f` context.
`<*>` takes a function in the `f` context and a value in the `f` context, and combines the two contexts while applying the function to the value.
So we can include contextual information by putting a value in the `f` context.
So if we want a function that takes a value `a` and returns some contextual information along with a result `b`, then we're looking at:

```haskell
context :: a -> f b
```

What does this mean for our functors?
Let's specialize the signature:

```haskell
ctxMaybe  :: a -> Maybe b
ctxList   :: a -> [a]
ctxReader :: a -> Reader r b
           ~ a -> r -> b
ctxState  :: a -> State s b
           ~ a -> s -> (b, s)

```

For `Maybe`, it means: "Give me an `a`, and I'll either return a result `Just b` or `Nothing`."
For `List`, it means: "Give me an `a`, and I'll return a list results `b`."
For `Reader`, it means: "Give me an `a`, and I'll return a computation that depends on the environment."
For `State`, it means: "Give me an `a`, and I'll return a stateful computation with some result `b`."

Let's assume we have a function `ctx :: a -> f b`.
What if we use that with `fmap`?

```haskell
-- fmap with b specialized to `f b`
fmap :: (a -> f b) -> f a -> f (f b)
```

We're close to what we want, but we have two layers of `f`.
If we have some way to join two layers of structure, then we'd be set.

## Join/Bind

And, in fact, that's what a monad is.
We can write `bind` and `join` in terms of each other:

```haskell
(>>=) :: f a -> (a -> f b) -> f b
ma >>= f = join (fmap f ma)

join :: f (f a) -> f a
join ma = ma >>= id
```

Rather than explaining it, I want you to play with it. Implement it yourself. Get a good feeling for it.
This isn't a monad tutorial, so I don't want to explain too much, and instead I'd like to focus on how this new found power affects our interaction with the context.

Let's write the `Monad` instance for `Either`, and discover why we can't write one for `Validation`!

```haskell
instance Monad (Either e) where
  Right a >>= f = f a
  Left e  >>= _ = Left e

instance Monad (Validation e) where
  Correct a >>= f = f a
  Errors e  >>= f = ... 
```

Well... What do we want to happen?
To be faithful to the Applicative instance, we'd like to collect the errors as we go.
We don't have an `a`, so we can't apply the `f` to anything.
If we can't apply the `f` to anything, then we can't get any more errors.
So all we can do is short-circuit.

Since the Monad is tied up in the sequence of events, there's no way for a monad to consider the entire structure of computation.
It has the power to short circuit.
It has the power to branch based on results of computations.
This added power is also a limitation -- there are fewer guarantees you can make about monads.
Since we can short circuit, we *can't* consider the entire computation at once.
We *must* approach it sequentially.

Monads are often seen as *strictly more powerful*, and people understand that to mean *strictly more useful*.
That's not quite the case.
