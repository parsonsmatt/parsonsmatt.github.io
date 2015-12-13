---
title: "Elm vs PureScript II"
date: 2015-10-05
layout: post
categories: programming
---

## The Elm Architecture, In PureScript

There's a fantastic introduction to the Elm programming style and application architecture called, as you might expect, [The Elm Architecture](https://github.com/evancz/elm-architecture-tutorial/).
The tutorial begins with a rather trivial application, and demonstrates how to extend the application to be more useful via composition of components and managing signals.

In the previous post, I compared "Hello World" with PureScript and Elm.
I'd like to compare some fairly trivial programs, to get an idea on what simpler programs in the two languages look and feel like.
Since The Elm Architecture already does a fantastic job of doing that for Elm, I've decided to simply recreate it in PureScript.

As it happens, `purescript-thermite` is very much like React, and relies a lot on internal state.
As a result, it doesn't work quite as naturally with the Elm architecture examples, especially for using things as nested components.
`purescript-halogen` seems to have an easier way to handle events and inputs, so I've decided to focus on that library instead.

The repository with the code is available [here](https://github.com/parsonsmatt/purs-architecture-tutorial).

# 0. Hello World!

Since I didn't do it in the previous post, here's "Hello World!" in Halogen:

```haskell
-- src/Example/Zero.purs
data Input a = Input a

ui :: forall g. (Functor g) => Component Unit Input g
ui = component render eval
  where
    render state =
      H.div_ [ H.h1_ [ H.text "Hello, World!" ] ]
    eval :: Eval Input Unit Input g
    eval (Input a) = pure a

-- src/Main.purs
import qualified Example.Zero as Ex0

main = runAff throwException (const (pure unit)) $ do
    node <- runUI Ex0.ui unit
    appendToBody node.node
```

Halogen expects the Input type to be have kind `* -> *`, and refers to it as a query algebra.
We'll get more into the details of that in the later examples, but we aren't really using it at the moment.
I find it helpful to think of the query algebra as a "public interface" for the component.

The `ui` function defines the component that we'll be using, and has two parts: `render` and `eval`.
`render` defines the layout in terms of the current state.
`eval` uses the `Input` query algebra to determine how to modify the state.

The type signature for `ui` is a bit intimidating, but it's not too bad:

* `Unit` is the state that this component is responsible for.
* `Input` is the query algebra we'll be using in the component.
* `g` is the functor/monad that the component operates in (like `Eff`, `Aff`, etc.)

This is easy enough, so let's move on to the first interactive example: Counter!

# 1. Counter

First, the necessary imports:

```haskell
-- src/Example/One.purs
module Example.One where

import Prelude

import Halogen
import qualified Halogen.HTML.Indexed as H
import qualified Halogen.HTML.Events.Indexed as E
```

Now, we'll define the type of our state.
In this case, it's a type alias to a record with a count field.
Our query algebra is just like Elm but with the extra type parameter.

```haskell
type State =
  { count :: Int
  }

data Input a
  = Increment a
  | Decrement a
```

And now, the `ui` function!

```haskell
ui :: forall g. (Functor g) => Component State Input g
ui = component render eval
  where
    render state =
      H.div_ 
        [ H.button [ E.onClick $ E.input_ Decrement ] 
                   [ H.text "-" ]
        , H.p_ [ H.text (show state.count)]
        , H.button [ E.onClick $ E.input_ Increment ] 
                   [ H.text "+" ]
        ]
```

We're using `E.input_` to send an event to the `eval` function.
If we cared about the event itself, then we could use `E.input` and provide a function that would accept the event information and provide a value on the `Input`.
We don't, so we'll skip that for now.

```haskell
    eval :: Eval Input State Input g
    eval (Increment next) = do
      modify (\state -> state { count = state.count + 1 }) 
      pure next
    eval (Decrement next) = do
      modify (\state -> state { count = state.count - 1 })
      pure next
```

Halogen has `get` and `modify` functions for use in the eval functions, which let us either view the current state or modify it.
Halogen uses the type variable associated with our query algebra to type the eval function.
Even though we're not using it yet, we still need for the function to evaluate to something of the same type.
That's why we pass `next` along.

Running the UI is essentially the same:

```haskell
-- src/Main.purs
main = runAff throwException (const (pure unit)) $ do
    -- node <- runUI Ex0.ui unit
    node <- runUI Ex1.ui { count: 0 }
    appendToBody node.node
```
We use `runUI` with our `ui` definition and an initial state, and append that to the body. Nice!

# 2. Counter Pair

This is where the two examples begin to differ.
In Elm, the render function has an address to send actions to, which are then evaluated later.
This makes it very easy to lift a child component's layout and rendering logic into a parent component:
just provide a forwarding address and an `Input` constructor, and update the state.
The state is all kept in the top level component, and passed down to children.

As a result, the parent component has access to all of the state of the application, and can inspect it at will. 
Both Thermite and Halogen instead encapsulate the state, such that parents don't know about the internal state of their children.
Halogen's query algebra (the `Input a` type we've been using) is meant to provide an API for the components, allowing them to be interacted with.

So, let's get started!

```haskell
-- src/Example/Two.purs
-- .. imports ..

newtype CounterSlot = CounterSlot Int
-- ord/eq instances for CounterSlot

type StateP =
  { topCounter :: CounterSlot
  , bottomCounter :: CounterSlot
  }

init :: StateP
init = { topCounter: CounterSlot 0, bottomCounter: CounterSlot 1 }
```

Since we're now talking about a component that contains other components, we want some way to talk about how it contains them.
Halogen carries around a lot more information in the type system about what's going on with each components state, effects, etc.
So now, the state of our CounterPair is just going to be a pair of slots for counters.

The slot is used to give an identifier to the element that the component contains.

The query algebra is much simpler:

```haskell
data Input a = Reset a
```

Since the counters are keeping track of their own internal state, all we need to do is know when to reset them.

Before we write the parent component, we'll want to define some type synonyms to make it easier to refer to the component.

```haskell
type State g =
  InstalledState StateP Ex1.State Input Ex1.Input g CounterSlot
```

So, the real state of our component is going to be an `InstalledState` of our newly defined `StateP` type on top of the `Ex1.State` type.
We'll also have the `Input` over the `Ex1.Input` query types.
Finally, we'll mention the `g` functor, and then refer to the `CounterSlot` as the type of slot that the counters will go in.
Now, to recap:

```haskell
type State g
  = InstalledState -- we're installing a state into another state,
    StateP         -- our parent state
    Ex1.State      -- child state
    Input          -- parent query
    Ex1.Input      -- child query
    g              -- functor variable
    CounterSlot    -- slot for child components
```

Alright! Next up, we've got our query type synonym.
It's using some fanciness in the form of coproduct, but it's not too complex.

```haskell
type Query =
  Coproduct Input (ChildF CounterSlot Ex1.Input)
```

Actually, this one is a lot simpler!
A Coproduct is `newtype Coproduct f g a = Coproduct (Either (f a) (g a))`.
In actual English, a coproduct is a way of saying "I have a value of type a, and it's either in a functor f or a functor g."
So, our type synonym is saying something like "The query type is either some value inside the Input functor, or a value inside the slot-indexed child input functor."

It's pretty complex, but the safety and composability make it worth it.
I promise!

Now, let's write the component function!
We'll use the type synonyms to simplify the component type:

```haskell
ui :: forall g. (Plus g) 
   => Component (State g) Query g
```

Not bad! Note that we're using `Plus g` instead of just `Functor` because we're doing a parent component now.

On to the function body!

```haskell
ui = parentComponent render eval
  where
    render state =
      H.div_
        [ H.slot state.topCounter mkCounter
        , H.slot state.bottomCounter mkCounter
        , H.button [ E.onClick $ E.input_ Reset ]
                   [ H.text "Reset!" ]
        ]
 
    mkCounter :: Unit -> { component :: Component Ex1.State Ex1.Input g, initialState :: Ex1.State }
    mkCounter _ = { component: Ex1.ui, initialState: Ex1.init 0 }
``` 

We use `parentComponent` now

The `H.slot` function accepts two arguments:

1. Some value of our `CounterSlot` type that we're using to identify child components,
2. A function from `Unit` to a record containing the component and initial state.

Halogen uses the function to lazily render the page.

The button sends the `Reset` message, which will get handled by our `eval` function.
Finally, the fun stuff!

```haskell
    eval (Reset next) = 
      -- ... wait, how do we change the child state?
```

Ah! We can't! Not quite yet, anyway.
The parent component has no way to directly set the state of the child component.
Our counter in `Example.One` only supports the following actions: `Increment` and `Decrement`.

If we want to reset the counter, we'll have to add that to the list of actions our `Counter` supports.

```haskell
-- src/Example/Counter.purs
-- mostly unchanged, but the query algebra:
data Input a
  = Increment a
  | Decrement a
  | Reset a

-- ... and the eval function:
    eval :: Eval Input State Input g
    eval (Increment next) = ...
    eval (Decrement next) = ...
    eval (Reset next) = do
      modify (const (init 0))
      pure next
```

`modify (const (init 0))` is equivalent to `modify \state -> init 0`, so we're -- as expected -- resetting the counter state to 0.

Now, the counters themselves don't have a control for `Reset`ing themselves.
Fortunately, we can easily send actions to child components from the parent component.
Let's get back to that `eval` function from the counter pair:

```haskell
-- src/Example/Two.purs
-- first, change the import to the new counter:
import qualified Example.Counter as Ex1

-- ... the rest of the file...

    eval (Reset next) = do
      query (CounterSlot 0) (action Ex1.Reset)
      query (CounterSlot 1) (action Ex1.Reset)
      pure next
```

`query` allows us to use the query algebra (or, public interface) that our components define.
We provide an identifier for the query, so we know where to look, and an action.
The action in this case is simply `Reset`, and we don't care about the return value.
Halogen also defines `request`, which we can use to get some information out of the component.

Finally, running the counter works pretty smooth:

```haskell
-- src/Main.purs
runEx2 = runUI Ex2.ui (installedState Ex2.init)

main = -- boilerplate elided...
  app <- runEx2
  appendToBody app.node
```

We have to use `installedState` since we're dealing with parent/children components.

Alright, that's the first two examples from Elm Architecture in PureScript Halogen!
I'll be covering the rest in a future installment.

### Other Posts in the series:


1. [Elm vs PureScript I: War of the Hello, Worlds](http://www.parsonsmatt.org/2015/10/03/elm_vs_purescript.html)
2. [Elm vs PureScript II](http://www.parsonsmatt.org/2015/10/05/elm_vs_purescript_ii.html)
2. [Elm Architecture in PureScript III: Dynamic Lists of Counters](http://www.parsonsmatt.org/2015/10/10/elm_architecture_in_purescript_iii.html)
3. [Elm Architecture in PureScript IV: Effects](http://www.parsonsmatt.org/2015/10/11/elm_architecture_in_purescript_iv:_effects.html)
