---
title: "An Intuition on Context II"
date: 2015-11-19
layout: post
categories: programming
---

## Not a monad tutorial!

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
As it happens, this second method is really handy for modeling non-deterministic computation, if we imagine a list as being a set of possible values that the variable can take on.

If we want to add two numbers, but we're not sure exactly which numbers we have, we could do:

```haskell
(+) <$> [1, 4, 3] <*> [6, 8, 7]
```

representing that our first number could be either 1, 4, or 3, and our second number could be 6, 8, or 7.
And we don't know.
So we want to get all possible values that the result *could* be.

What does this end up looking like? Well, `<$>` is map, so we'll do `+` to all the elements in the first list.

```haskell
[(1 +), (4 +), (3 +)] <*> [6, 8, 7]
```

Then we do `(1+)` to each of the possible values, and combine the resulting lists together.

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

Reader had 729 implementations, which tells us that the complexity expands a lot.
It didn't really give us much intuition about how the context worked.
Instead, it was nicer to think about it in terms of a decision tree that was ruled by a single decision.

State is similar, but since we are able to update the state as we go along, we're able to build a decision tree on the fly, where each function in the applicative sequence gets to update the state.
This allows previous stateful things in the application context to have an effect on decisions that happen later.
Let's revisit the weather travel plans and use a stateful approach.

```haskell
type Decision a = State WeatherState a
type WeatherState = (Weather, Grumpiness)
type Grumpiness = Int

data Plan' = Plan' Transport Clothing Food

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
  weather <- gets fst
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
So far, we've been working with the intuition that applicative application allows contexts to affect each other.
The context of State is the state surrounding the computation.
Using functor to map over the stateful computation didn't touch the state, it just altered the return value.
Using applicative application is allowing the previous functions in the sequence to affect the state -- and thus return value -- of future computations.

To wrap up, a functor is a context where we can map over, but we can't look at the context, and we can't change the context.
An Applicative allows our contexts to interact independently of the values contained in the contexts.
This is a lot more power! 
But it's still perhaps a bit more limited than we might want.

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
So if we want a function that takes a value `a` and returns some contextual information, then we're looking at:

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

## fmap, Join, Return
