---
title: "Elm Architecture in PureScript III"
date: 2015-10-10
layout: post
categories: programming
---

## Dynamic Lists of Counters

On the [last post](http://www.parsonsmatt.org/programming/2015/10/05/elm_vs_purescript_ii.html), we implemented a pair of counters.
Now, we'll generalize that out to a dynamic list of counters, and later, give them all remove buttons.
In the process, we'll learn how to combine components, stack them, peek on them, and otherwise deal with them appropriately.

The code for this is available in [this repository](https://github.com/parsonsmatt/purs-architecture-tutorial).

Let's get started! We want a list of counters, a button to add a counter, and a button to remove a counter. Let's define our state and inputs:

```haskell
type State =
    { counterArray :: Array Int
    , nextID :: Int
    }

initialState :: State
initialState =
    { counterArray: []
    , nextID: 0
    }

data Input a
    = AddCounter a
    | RemoveCounter a
```

And, our UI function:

```haskell
listUI :: forall g p. (Functor g)
       => ParentComponent State Counter.State Input Counter.Input g CounterSlot p
listUI = component render eval
  where
    render :: Render State Input CounterSlot
    render state = 
      H.div_ [ H.h1_ [ H.text "Counters" ]
             , H.ul_ $ map (H.slot <<< CounterSlot) state.counterArray
             , H.button [ E.onClick $ E.input_ AddCounter ]
                        [ H.text "Add Counter" ]
             , H.button [ E.onClick $ E.input_ RemoveCounter ]
                        [ H.text "Remove Counter" ]
             ]
    eval :: EvalP Input State Counter.State Input Counter.Input g CounterSlot p
    eval (AddCounter next) = do
      modify addCounter
      pure next
    eval (RemoveCounter next) = do
      modify removeCounter
      pure next
```

Basically the same thing we've been working with already! 
Instead of keeping a `CounterSlot 0` and `CounterSlot 1` around, we've got an array of integers.
When we want to render them, we map over them with the slot type constructor and the `H.slot` to give them a place to go.
Halogen figures out all of the event routing for us.
How nice!

Let's look a bit more at the type signatures:

```haskell
listUI :: forall g p. (Functor g)
       => ParentComponent
          State         -- parent component state
          Counter.State -- child component state
          Input         -- parent input
          Counter.Input -- child input
          g             -- Functor we're operating in
          CounterSlot   -- type to identify child elements
          p             -- Type to identify child slot for child elements
```

Pretty intense, right? It gets worse!
The type signatures are a bit unwieldy, and the type errors are terrifying.
They give Halogen a pretty impressive ability to compose elements effectively and safely.

## Removing a Counter

Alright, it's time to give counters their own remove button.
Rather than touch the counter at all, we're simply going to wrap the existing counter component in a new component.
The sole responsibility of this component will be handling the removal of counters.

The resulting code is pretty tiny!

```haskell
-- src/Example/CounterRem.purs
data Input a = Remove a

withRemove :: forall g p. (Functor g)
           => ParentComponent Unit Counter.State Input Counter.Input g CounterSlot p
withRemove = component render eval
    where
        render :: Render Unit Input CounterSlot
        render _ =
            H.div_ [ H.slot (CounterSlot 0)
                   , H.button [ E.onClick $ E.input_ Remove ]
                              [ H.text "Remove" ]
                   ]
        eval :: EvalP Input Unit Counter.State Input Counter.Input g CounterSlot p
        eval (Remove a) = pure a
```

Since we're not maintaining any state, we'll just use the `Unit` type to signify that.
Our `eval` function is going to punt the behavior to the parent component.

Now... Halogen does some *impressive* type trickery.
Coproducts, free monads, query algebrae... it can be pretty intimidating.
We're about to get into some of that.

When you're installing components into other components, you use the `InstalledComponent` type synonym.
But if you're dealing with multiple levels of nesting, then the type synonym no longer works.
It's time to define our own type synonym!

Let's look at `InstalledComponent` and `InstalledState` in the [Halogen documentation](https://github.com/slamdata/purescript-halogen/blob/master/docs/Halogen/Component.md#installedcomponent): 

```haskell
type InstalledComponent s s' f f' g p p' 
  = Component 
    (InstalledState s s' f f' g p p') 
    (Coproduct f (ChildF p f')) 
    g 
    p'
```

Remember that Component is `state input functor childslots`.
We'll get to the coproduct.
InstalledState is pretty easy:

```haskell
type InstalledState s s' f f' g p p' = 
  { parent   :: s
  , children :: Map p (ChildState s' f' g p')
  , memo     :: Map p (HTML p' (Coproduct f (ChildF p f') Unit)) 
  }
```

It's a record with a parent state, a map from child slots to child states, and a map from child slots to memoized HTML.

But what is all of this `coproduct` stuff?
A `Coproduct` is defined like this:

```haskell
newtype Coproduct f g a = Coproduct (Either (f a) (g a))
```

It's a way of saying "I have a value of type a inside of a functor. That functor is either f or g."
We know we can specialize `f` in the `InstalledComponent` to our `Input` query algebra.
And `ChildF p f'` is a given child's identifier and the child's query algebra.
Halogen is using the coproduct structure to keep track of the children's query algebra inputs.

Let's define the type synonyms now.

```haskell
type StateMiddle g p =
    InstalledState Unit Counter.State Input Counter.Input g CounterSlot p
```

The true state of this component isn't just `Unit` -- it's the result of installing the `Counter.State` into this component.
We're giving that a name we can reference, and allowing the caller to provide the functor and types of child-child slots.

```haskell
type QueryMiddle = Coproduct Input (ChildF CounterSlot Counter.Input)
```

Finally, our `QueryMiddle` just fills in the types for the combined query algebra.
These synonyms allow us to define the `ui` data a bit more concisely:

```haskell
ui :: forall g p. (Plus g)
   => Component (StateMiddle g p) QueryMiddle g p
ui = install withRemove mkCounter
  where
    mkCounter (CounterSlot _) = 
      createChild Counter.ui (Counter.init 0)
```

Alright! Awesome! We've augmented a component with a `Remove` button.
Let's embed that into a list.
We'll actually get to reuse almost everything from example three!

```haskell
-- src/Example/Four.purs
data Input a = AddCounter a

listRemUI :: forall g p. (Functor g)
          => ParentComponentP State (Counter.StateMiddle g p) Input Counter.QueryMiddle g CounterSlot p
listRemUI = component' render eval peek
  where
```

AH! What is this PEEK? and that `P` on the end of `ParentComponent`? What's going on here?And `component'`??

Peeking is the way to inspect child components in purescript-halogen.
If a component has the `P` at the end, that means it can peek.
So when a child component of a peeking parent is done with an action, then the parent gets a chance to see the action and act accordingly.

We're using the type synonyms that we defined above to talk about the child states and queries.
To be completely honest, PureScript is capable of inferring the types with a type wildcard, so the following signature also works:

```haskell
listRemUI :: forall g p. (Functor g)
          => ParentComponentP State _ Input _ g CounterSlot p
```

provided that you also wildcard the signatures in the `eval` and `peek` functions.

We use `component'` because it's a peeking component.

```haskell
    render :: Render State Input CounterSlot
    render state =
      H.div_ [ H.h1_ [ H.text "Counters" ]
             , H.ul_ (map (H.slot <<< CounterSlot) state.counterArray)
             , H.button [ E.onClick $ E.input_ AddCounter ]
                        [ H.text "Add Counter" ]
             ]

    eval :: EvalP Input State (Counter.StateMiddle g p) Input Counter.QueryMiddle g CounterSlot p
    eval (AddCounter next) = do
      modify addCounter
      pure next
```

Rendering and evalling work exactly as you'd expect. Let's look at peeking!

```haskell
    peek :: Peek State _ Input _ g CounterSlot p
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

I talked briefly with a halogen developer, and he said that you're not *supposed* to do it, and the API will likely be simplified so all `peek` functions will look like the simpler `ChildF` one I defined above, and you won't be able to inspect more than a level deep.

In any case, we `peek` on the child component, and if it just did a `Remove` action, then we modify our own state.
Otherwise, we ignore it.

Making a renderable component is:

```haskell
ui :: forall g p. (Plus g)
   => InstalledComponent State (Counter.StateMiddle g p) Input Counter.QueryMiddle g CounterSlot p
ui = install' listRemUI mkCounter
  where
    mkCounter (CounterSlot _) =
      createChild Counter.ui (installedState unit)
```

Like `component'`, `install'` is used because it is a peeking component. Finally, running the example is:

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

type ChildSlot = Unit

type StateP = Unit

withRemove :: forall g p s' f'. (Functor g)
           => ParentComponent StateP s' QueryP f' g ChildSlot p
withRemove = component render eval
    where
        render :: Render StateP QueryP ChildSlot
        render _ =
            H.div_ [ H.slot unit
                   , H.button [ E.onClick $ E.input_ Remove ]
                              [ H.text "Remove" ]
                   ]
        eval :: EvalP QueryP StateP s' QueryP f' g ChildSlot p
        eval (Remove a) = pure a
```

Easy! We've got a few extra type variables to represent where the child state and query will go.

Now we'll define some type synonyms to make using the component easier higher up:

```haskell
type State s f g p =
    InstalledState StateP s QueryP f g ChildSlot p

type Query f =
    Coproduct QueryP (ChildF ChildSlot f)
```

And finally, a function that takes a component, that component's initial state, and returns a new component with a remove button installed into it.

```haskell
addRemove :: forall s f g p. (Plus g)
          => Component s f g p 
          -> s 
          -> Component (State s f g p) (Query f) g p
addRemove comp state = install withRemove mkChild
    where
        mkChild _ = createChild comp state
```

Cool! Let's see what the definition for the counter looks like with the remove button added:

```haskell
-- src/Example/CounterRemPrime.purs
type State g p = Rem.State Counter.State Counter.Input g p
type Query = Rem.Query Counter.Input

ui :: forall g p. (Plus g)
   => Component (Rem.State Counter.State Counter.Input g p)
                (Rem.Query Counter.Input) g p
ui = Rem.addRemove Counter.ui (Counter.init 0)
```

More type synonyms! And a fairly nice one liner function to wrap the counter.

The code for the list itself is essentially unchanged.
We do have to import the `RemGeneric` as well as the `CounterRemPrime` module to be able to use the `RemGeneric.Input` type, but the type declarations hardly change at all.
Running a diff on the two files gets us these changes:

```bash
λ diff Four.purs FourPrime.purs
13c13,14
< import qualified Example.CounterRem as Counter
---
> import qualified Example.CounterRemPrime as Counter
> import qualified Example.RemGeneric as Rem
39c40
<                  Left (Counter.Remove _) ->
---
>                  Left (Rem.Remove _) ->
```

(omitting the parts where the `State` vs `StateMiddle` were all that changed)


