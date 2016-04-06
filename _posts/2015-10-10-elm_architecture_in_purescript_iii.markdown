---
title: "Elm Architecture in PureScript III"
date: 2015-10-10
layout: post
categories: programming
---

## Dynamic Lists of Counters

On the [last post](http://www.parsonsmatt.org/2015/10/05/elm_vs_purescript_ii.html), we implemented a pair of counters.
Now, we'll generalize that out to a dynamic list of counters, and later, give them all remove buttons.
In the process, we'll learn how to combine components, stack them, peek on them, and otherwise deal with them appropriately.

The code for this is available in [this repository](https://github.com/parsonsmatt/purs-architecture-tutorial).

Let's get started! We want a list of counters, a button to add a counter, and a button to remove a counter. Let's define our state and inputs:

```haskell
type StateP =
  { counterArray :: Array Int
  , nextID :: Int
  }

initialState :: StateP
initialState =
  { counterArray: []
  , nextID: 0
  }

data Input a
  = AddCounter a
  | RemoveCounter a
```

Another quick detour to define our parent-level state and query types:

```haskell
type State g =
  InstalledState StateP Counter.State Input Counter.Input g CounterSlot

type Query =
  Coproduct Input (ChildF CounterSlot Counter.Input)
```

And, our UI function:

```haskell
ui :: forall g. (Plus g)
   => Component (State g) Query g
ui = parentComponent render eval
  where
    render state = 
      H.div_ 
        [ H.h1_ [ H.text "Counters" ]
        , H.ul_ $ map (\i -> mslot (CounterSlot i) Counter.ui (Counter.init 0)) state.counterArray
        , H.button [ E.onClick $ E.input_ AddCounter ]
                   [ H.text "Add Counter" ]
        , H.button [ E.onClick $ E.input_ RemoveCounter ]
                   [ H.text "Remove Counter" ]
        ]

    eval :: EvalParent Input StateP Counter.State Input Counter.Input g CounterSlot
    eval (AddCounter next) = do
      modify addCounter
      pure next
    eval (RemoveCounter next) = do
      modify removeCounter
      pure next

mslot :: forall s f g p i. p -> Component s f g -> s -> HTML (SlotConstructor s f g p) i
mslot slot comp state = H.slot slot \_ -> { component: comp, initialState: state }
```

Basically the same thing we've been working with already! 
Instead of keeping a `CounterSlot 0` and `CounterSlot 1` around, we've got an array of integers.
When we want to render them, we map over them with the slot type constructor and the `H.slot` to give them a place to go.
Halogen figures out all of the event routing for us.

## Removing a Counter

Alright, it's time to give counters their own remove button.
Rather than touch the counter at all, we're simply going to wrap the existing counter component in a new component.
The sole responsibility of this component will be handling the removal of counters.

There's a bit of boiler plate around the State and Query, but after that, the result is pretty tiny!

```haskell
-- src/Example/CounterRem.purs
data Input a = Remove a

type State g =
  InstalledState Unit Counter.State Input Counter.Input g CounterSlot
type Query =
  Coproduct Input (ChildF CounterSlot Counter.Input)

ui :: forall g. (Plus g)
   => Component (State g) Query g
ui = parentComponent render eval
  where
    render _ =
        H.div_ 
          [ mslot (CounterSlot 0) Counter.ui (Counter.init 0)
          , H.button [ E.onClick $ E.input_ Remove ]
                     [ H.text "Remove" ]
          ]
    eval :: EvalParent Input Unit Counter.State Input Counter.Input g CounterSlot
    eval (Remove a) = pure a
```

Since we're not maintaining any state, we'll just use the `Unit` type to signify that.
Our `eval` function is going to punt the behavior to the parent component.

Now... Halogen does some *impressive* type trickery.
Coproducts, free monads, query algebrae... it can be pretty intimidating.
There's a decent amount of associated boilerplate as well.
We're about to get into some of that.

Let's look at `InstalledState` in the [Halogen documentation](https://github.com/slamdata/purescript-halogen/blob/master/docs/Halogen/Component.md#installedstate): 

```haskell
type InstalledState s s' f f' g p = 
  { parent   :: s
  , children :: Map p (Tuple (Component s' f' g) s')
  , memo     :: Map p (HTML Void (Coproduct f (ChildF p f') Unit)) 
  }
```

It's a record with a parent state, a map from child slots to child states, and a map from child slots to memoized HTML.

But what is all of this `coproduct` stuff again?
A `Coproduct` is defined like this:

```haskell
newtype Coproduct f g a = Coproduct (Either (f a) (g a))
```

It's a way of saying "I have a value of type a inside of a functor. That functor is either f or g."
We know we can specialize `f` in the `InstalledComponent` to our `Input` query algebra.
And `ChildF p f'` is a given child's identifier and the child's query algebra.
Halogen is using the coproduct structure to keep track of the children's query algebra inputs.

Revisiting our type synonyms again, we have:

```haskell
type State g =
  InstalledState Unit Counter.State Input Counter.Input g CounterSlot
```

The true state of this component isn't just `Unit` -- it's the result of installing the `Counter.State` into this component.
We're giving that a name we can reference, and allowing the caller to provide the functor.

```haskell
type Query =
  Coproduct Input (ChildF CounterSlot Counter.Input)
```

Finally, our `QueryMiddle` just fills in the types for the combined query algebra.

Alright! Awesome! We've augmented a component with a `Remove` button.
Let's embed that into a list.
We'll actually get to reuse almost everything from example three!

```haskell
-- src/Example/Four.purs
data Input a = AddCounter a

type State g =
  InstalledState StateP (Counter.State g) Input Counter.Query g CounterSlot

type Query =
  Coproduct Input (ChildF CounterSlot Counter.Query)

ui :: forall g. (Plus g)
   => Component (State g) Query g
ui = parentComponent' render eval peek
  where
```

Ah! We're peeking! I can tell because of the `peek` function.
And also the `'` on the end of `parentComponent'`.
The `'` indicates peeking.

Peeking is the way to inspect child components in purescript-halogen.
So when a child component of a peeking parent is done with an action, then the parent gets a chance to see the action and act accordingly.

```haskell
    render state =
      H.div_ 
        [ H.h1_ [ H.text "Counters" ]
        , H.ul_ (map (mapSlot CounterSlot Counter.ui (installedState unit)) state.counterArray)
        , H.button [ E.onClick $ E.input_ AddCounter ]
                   [ H.text "Add Counter" ]
        ]

    eval :: EvalParent _ _ _ _ _ g CounterSlot
    eval (AddCounter next) = do
      modify addCounter
      pure next

mapSlot slot comp state index = mslot (slot index) comp state
```

Rendering and evalling work exactly as you'd expect. Let's look at peeking!

```haskell
    peek :: Peek (ChildF CounterSlot Counter.Query) StateP (Counter.State g) Input Counter.Query g CounterSlot
    peek (ChildF counterSlot (Coproduct queryAction)) =
      case queryAction of
        Left (Counter.Remove _) ->
          modify (removeCounter counterSlot)
        _ ->
          pure unit
```

So this is kind of a more complex `peek` than you'd normally start with.
My bad.
Generally, the `peek` function has a definition that'd look like:

```haskell
peek (ChildF childSlot action) =
  case action of
       DoThing next -> -- ...

```

But we're working with the installed/child components who manage their state using the coproduct machinery, and as of now, we have to manually unwrap the coproduct and pattern match on the `Either` value inside.
When we match on the `Left` value, we get to see the immediate child's actions.
If we were to match on the `Right` value, then we'd get to inspect children's of children's actions.

In any case, we `peek` on the child component, and if it just did a `Remove` action, then we modify our own state.
Otherwise, we ignore it.

```haskell
-- src/Main.purs
main = ... do
  app <- runEx4
  appendToBody app.node

runEx4 = runUI Ex4.ui (installedState (Ex3.initialState))
```

And now we've got our dynamic list of removable embedded counters going.

Next up, we'll be looking at AJAX, effects, and other fun stuff.

## UPDATE: Modularize me, cap'n!

Ok, so I wasn't happy with how unmodular the above example was.
We had to redefine a whole component just to add a remove button.
If I wanted another component that had a remove button, I'd have to redo all that work!
No thanks.
Instead, I made a higher order component out of it.

There's no meaning for distinguishing between children, because it only has one.
There's no state involved either, so we'll use Unit for both of them.
The only query is Remove.
So let's put that all together!

```haskell
-- src/Example/RemGeneric.purs
data QueryP a = Remove a

type State s f g =
  InstalledState Unit s QueryP f g Unit

type Query f =
  Coproduct QueryP (ChildF Unit f)

addRemove :: forall g s f. (Plus g)
          => Component s f g
          -> s
          -> Component (State s f g) (Query f) g
addRemove comp state = parentComponent render eval
  where
    render _ =
        H.div_ 
          [ H.slot unit \_ -> { component: comp, initialState: state } 
          , H.button [ E.onClick $ E.input_ Remove ]
                     [ H.text "Remove" ]
          ]
    eval :: EvalParent QueryP Unit s QueryP f g Unit
    eval (Remove a) = pure a
```

Easy! We've got a few extra type variables to represent where the child state and query will go.
Fairly standard type synonym definitions for use in client components.
The only kinda tricky part is rendering: we accept a component and initial state as parameters.

Cool! Let's see what the definition for the counter looks like with the remove button added:

```haskell
-- src/Example/CounterRemPrime.purs
type State g = Rem.State Counter.State Counter.Input g
type Query = Rem.Query Counter.Input

ui :: forall g. (Plus g)
   => Component (State g) Query g
ui = Rem.addRemove Counter.ui (Counter.init 0)
```

More type synonyms! And a fairly nice one liner function to wrap the counter.

The code for the list itself is essentially unchanged.
We do have to import the `RemGeneric` as well as the `CounterRemPrime` module to be able to use the `RemGeneric.Input` type, but the type declarations hardly change at all.

All in all, this level of componentiziation is fairly easy! Defining the type synonyms is a bit of a pain, but you'll likely be writing a lot fewer of them when you have more involved components.

### Other posts in the series:

1. [Elm vs PureScript I: War of the Hello, Worlds]({ post_url 2015-10-03-elm_vs_purescript })
2. [Elm vs PureScript II]({ post_url 2015-10-05-elm_vs_purescript_ii })
3. [Elm Architecture in PureScript IV: Effects]({ post_url 2015-10-11-elm_architecture_in_purescript_iv_effects })
