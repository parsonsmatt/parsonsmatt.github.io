---
title: "Recursion Excursion"
date: 2015-09-24
layout: post
categories: programming
---

Recursive definitions are a lot of fun.
The typical example of a recursive definition is the natural numbers:

> A natural number is either 0 or the successor of a natural number.

Expressed in Haskell, this is:

```haskell
data Nat = Zero | Succ Nat
```

Zero is `Zero`, as you'd expect. One is `Succ Zero`, two is `Succ (Succ Zero)`, etc.
The natural numbers can be recursively defined like this.

## Extension One:

Lists are extremely similar to the natural numbers -- we just attach something to each `Succ`, and now we've got a linked list of things.

```haskell
data List a = Nil | Cons a (List a)
```

And, interestingly enough, you can encode the natural numbers as lists. We can replace the `a` with the unit type `()` and the list is equivalent to the natural number representing it's length:

```haskell
Cons () (Cons () (Cons () Nil ))
Succ    (Succ    (Succ    Zero))
```

Cool! But now that I've seen one example of this sort of progression, I want to see what else can happen.
For lists, we added a type variable `a` to the definition of the natural numbers.
Let's try adding another type variable:

```haskell
data TwoList a b = Z | S a b (TwoList a b)
```

But this is boring. It's basically the exact same thing as `List (a, b)`.
And it's easy to see that adding more of these variables is the same as adding elements to the tuple, so a `FiveList a b c d e` is really just a `List (a, b, c, d, e)`.
So, we don't want our new type variable to be trivially expressible by a regular list.
What's next?

We could add the type variable to the `Nil` constructor, but that's also pretty uninteresting.
`List' b a = Nil' b | Cons' a (List' b a)` doesn't buy any real expressive power.

Well, let's differentiate that new type variable `b` in some way. Since it's a type variable, we can really only talk about the kind that it has. `b :: *` didn't work, so let's try `b :: * -> *`.

```haskell
data ListB a b = NilB | ConsB (b a) (ListB a b)
```

Ah, but this is also trivial!
A `ListB Int Maybe` is the same as a `List (Maybe Int)`.
We have bought nothing with this arrangement.

Well, we can't just apply `a` to `b`, we have to try something else.
The remaining list is the only thing left, so let's try that.

## Extension Two:

```haskell
data ListF a b = NilF | ConsF b (a (ListF a b))
```

There we go.
This is not trivially expressible as a `List`!
And I really felt weird writing `ListF Int Maybe` so I swapped the type variables: `b` is now the value, and `a` is the new thing we're playing with.
But... What does it look like? Let's try a `ListF Maybe Int`:

```haskell
-- specialized type: List = Nil | Cons Int (Maybe List)
nil  = NilF
one  = ConsF 1 Nothing
one' = ConsF 1 (Just NilF)
two  = ConsF 1 (Just (ConsF 2 Nothing))
```
Interesting! We have two termination cases here.
The ordinary `NilF` that we get from the list definition, and `Nothing` we get from using `Maybe` as the `f`.
What about `Either`?
Let's see what `ListF (Either String) Int` looks like:

```haskell
-- specialized: List = Nil | Cons Int (Either String List)
oneR = ConsF 1 (Right NilF)
oneL = ConsF 1 (Left "asdf")
twoR = ConsF 1 (Right (ConsF 2 (Right NilF)))
twoL = ConsF 1 (Right (ConsF 2 (Left "nope")))
```

We've got two ways to terminate again.
The first is the `NilF` constructor.
The second is the string in the Either definition.

Is there an interesting thing of kind `* -> *` can we try this with?
We've tried `Maybe` and `Either`, which were both basically the same thing.
We can figure out why by inspecting their definitions:

```haskell
data Maybe    a = Nothing | Just  a
data Either b a = Left b  | Right a
```

They're both sum types.
When we have `Maybe ()`, we have two values: `Nothing` and `Just ()`.
We've only added 1 to the number of values in the type variable.
`Maybe Bool` has `Nothing`, `Just True`, and `Just False`.
Again, just one more value than `Bool` itself.

When we use these in our augmented list, the `a` variable is fixed to `ListF f a`.
So anything that doesn't carry the last type variable is just going to terminate the list.
Either can therefore terminate the list with a different type, but this isn't fundamentally very interesting.

Since simple sum types don't seem to be terribly interesting, let's try a product type!

```haskell
type With a = (,) a
```

is a convenient synonym we'll use.
To keep things simple, we'll stick with `Bool` for now.

```haskell
oneL :: ListF (With Bool) Int
oneL = ConsF 1 (True, NilF)

twoL :: ListF (With Bool) Int
twoL = ConsF 1 (False, ConsF 2 (True, NilF))
```

Well, this feels just like a `List (a, b)`, but laid out differently.
So far, product and sums haven't really bought us anything.

Fortunately, there are exponential types, or functions as they're more commonly known.
Reader is defined as `type Reader r = (->) r`.
It's a functor, like `Maybe` and `Either`, and it has the right kind when we supply the type to be read.
Let's... give that a shot?

We'll specialize the Reader to Bool because there's only two possible values of type `Bool`. This makes it a bit easier to deal with.

```haskell
-- specialized: List = Nil | Cons Int (Reader Bool List)
```

Ok, wait, what does this mean again?
A `ListF (Reader Bool) Int` means that the next item in the list is a function that takes a Boolean value and returns a List.
Ok. Let's do this.

```haskell
one = ConsF 1 (\c -> NilF)
two = ConsF 1 (\c -> if c then NilF else ConsF 2 (const NilF))
```

Wait, what? We're expressing control flow and branching!
Oh, this is just too cool.
How do we evaluate that?
We'll pass a single in the thing and see what it does, and collect the results into a regular list.

```haskell
single :: Bool -> ListF (Reader Bool) Int -> [Int]
single _ NilF        = []
single c (ConsF n f) = n : single c (f c)

single False two == [1, 2]
single True two  == [1]
```

Cool!
By using the Reader functor we've turned the List into a binary tree.
Observe:

```haskell
binTree =
  ConsF 1 $ \a ->
    if a then branchOne
         else branchTwo

branchOne =
  ConsF 10 $ \a ->
    if a then binTree
         else branchTwo

branchTwo =
  ConsF 2 $ \a ->
    if a then NilF
         else branchTwo
```

Check this craziness out. What even is it? It's some kind of graph/maze thing. Let's write a function to traverse this business:

```haskell
traverseBool :: [Bool] -> ListF (Reader Bool) Int -> [Int]
traverseBool _      NilF       = []
traverseBool []     (Cons n _) = [n]
traverseBool (b:bs) (Cons n f) = n : traverseBool bs (f b)
```

Alright, so given `[True, True, False, True, True, False]`, we'll generate `[1, 10, 1, 2]`. Given a different list of Bools, it'd traverse differently.

Very interesting!
We can represent binary trees using `Reader Bool`.
Bool has two possible values, and we've got trees with two branches.
It seems that the number of branches we'll get is dependent on how many inhabitants there are in the type.
If that's the case, then `Reader ()` will simply be a linked list.

```haskell
fancyList :: ListF (Reader ()) Int
fancyList = ConsF 1 (\() -> ConsF 2 (\() -> NilF))
```

So `List a` can be expressed as `ListF (Reader ()) a`, and the natural numbers are then `ListF (Reader ()) ()`.

This is the first really interesting functor we've used here.
Either and Maybe simply provided extra termination options.
Tupling just added a bit of extra information at each cell.
Reader gives us branching.

Finally, let's try another more interesting functor: State!

```haskell
newtype State s a
  = State
  { runState :: s -> (a, s) }

-- specialized type: ListF (State Bool) Int
one = ConsF 1 (State (\s -> (NilF, s)))
two =
  ConsF 1 (State (\s -> (ConsF 2 (State (\s' -> (NilF, not s') )), s)))
```

Well, this is kind of weird. Let's traverse it and see what happens.

```haskell
traverseState :: [Bool] -> ListF (State Bool) Int -> [Int]
traverseState _      NilF        = []
traverseState []     (ConsF n _) = [n]
traverseState (b:bs) (ConsF n s) =
    n : traverseState bs (fst (runState s b))
```

So, this will go down the list, applying the Bool values to the states in turn, extracting the Ints, and discarding the result states.
This isn't any different from Reader yet.
It seems like we should be able to kick off the computation with a single Bool and let it carry on down.
Let's try passing the state down.

```haskell
-- specializing the type:
-- traverseState' :: Bool -> ListF (State Bool) Int -> [Int]
traverseState' :: s -> ListF (State s) a -> [a]
traverseState' _ NilF        = []
traverseState' b (ConsF n s) =
    let (list, state) = runState s b
     in n : traverseState' state list
```

And if we want to remember the state history,

```haskell
-- specialized:
-- traverseState'' :: Bool -> ListF (State Bool) Int -> [(Int, Bool)]
traverseState'' :: s -> ListF (State s) a -> [(a, s)]
traverseState'' _ NilF = []
traverseState'' b (ConsF n s) =
    let (list, state) = runState s b
     in (n, b) : traverseState' state list
```

Cool!
So now we can drop a seed value into a chain of stateful computations and the result will be a list of the return values of each computation.
Those type signatures are actually a bit more specific than they need to be: we can replace the signature with `traverseState' :: a -> ListF (State a) b -> [b]`
Let's do something a tiny bit more interesting with it:

```haskell
collatz :: Int -> [Int]
collatz x = traverseState' x (chain x)
  where
    chain n = ConsF n (State go)
    go y    =
      if y == 1
         then (NilF, y)
         else let val = if even y
                           then y `div` 2
                           else 3 * y + 1
               in (chain val, val)
```

This will evaluate the Collatz Conjecture for a given number.

## What is it?

Now, what is `ListF`?
It's kind of similar to the Free monad, but there's an important distinction:

```haskell
data Free  f a = Pure a | Free    (f (Free  f a))
data ListF f a = Nil    | ConsF a (f (ListF f a))
```

Free only carries values at the leaves or end of the chain.
ListF carries values at each step.
The common structure seems like it could be extracted out like so:

```haskell
data DualFree f b a = DPure a | DFree b (f (DualFree f b a))
```

Using the above definition of `DualFree`, the free monad can be thought of as a list of computations in the `f` functor with some return value `b` where `a` is `()`.
`ListF` on the other hand is a list of values `a` along with a list of computations in the `f` functor.

`ListF` over `Reader r` gives us trees of values.
`ListF` over `State s` gives us unfolding structures.

In fact...
This is (almost!) the list monad transformer "done right!"
The canonical implementation is:

```haskell
newtype ListT m a =
    ListT { unListT :: m (Maybe (a, ListT m a)) }
```

This uses the `Maybe` to avoid having the `Nil` data constructor, which allows it to use the `newtype` declaration.
This also ensures that even the first element in the list is wrapped in the monad.
The `ListF` version I wrote associates the monadic actions with the links between values, and this version associates the monadic action with the values themselves.

It's also pretty similar to the cofree comonad:

```haskell
newtype Cofree h a =
    Cofree { unCofree :: (a, h (Cofree h a)) }
```

If you took out the `NilF` constructor, anyway.

> Thanks to Gary Fixler for notifying me that the type signature in `traverseState` was wrong!
