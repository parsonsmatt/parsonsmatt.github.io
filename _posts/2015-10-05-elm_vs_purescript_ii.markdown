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

# Pre-0: Getting Halogen Running

OK, so Halogen doesn't -- quite work by default! The version available via Bower doesn't work right due to a problem in a dependency of a dependency not being updated for the 0.7 PureScript compiler release, so you have to do the following:

1. Do `pulp dep install --save purescript-halogen#master`, to install Halogen from master branch.
2. Then do `npm install --save virtual-dom` to get the `virtual-dom` dependency in your project. You should probably have a `package.json` file so this can be more-or-less automated.

This tutorial is going to target Halogen v0.5, which is not released yet, but seems to only need a bit of optimization and bug fixes before release.

# 0. Hello World!

Since I didn't do it in the previous post, here's "Hello World!" in Halogen:

```haskell
-- src/Example/Zero.purs
data Input a = Input a

ui :: forall g p. (Functor g) => Component Unit Input g p
ui = component render eval
    where
        render state =
            H.div_ [ H.h1 [] [ H.text "Hello, World!" ] ]
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

The `ui` function defines the component that we'll be using, and has two parts: `render` and `eval`.
`render` defines the layout in terms of the current state.
`eval` uses the `Input` query algebra to determine how to modify the state.

The type signature for `ui` is a bit intimidating, but it's not too bad:

* `Unit` is the state that this component is responsible for.
* `Input` is the query algebra we'll be using in the component.
* `g` is the functor that the component operates in (like `Eff`, `Aff`, etc.)
* `p` is the type of slots of children components.

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
ui :: forall g p. (Functor g) => Component State Input g p
ui = component render eval
    where
        render :: Render State Input p
        render state =
            H.div_ [ H.button [ E.onClick $ E.input_ Decrement ] 
                              [ H.text "-" ]
                   , H.p_ [ H.text (show state.count)]
                   , H.button [ E.onClick $ E.input_ Increment ] 
                              [ H.text "+" ]
                   ]
```

We're using `E.input_` to send an event to the `eval` function.
If we cared about the event itself, then we could use `E.input` and provide a function that would accept the event information and provide a value on the `Input`.

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

type State =
    { topCounter :: CounterSlot
    , bottomCounter :: CounterSlot
    }

init :: State
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

Now, let's write the component function! Since we're working with a Parent component, we'll want to use a type signature that represents that information:

```haskell
pairUI :: forall g p. (Functor g) 
       => ParentComponent State Ex1.State Input Ex1.Input g CounterSlot p
```

The `ParentComponent` type constructor takes a mess of arguments:

* `State` is the state of the parent component.
* `Ex1.State` is the state of the child component.
* `Input` is the query algebra for the parent component.
* `Ex1.Input` is the query algebra for the child component.
* `g` is the functor the whole thing is running on top of
* `CounterSlot` is the type of the slots of child components in the component.
* `p` is the type of the slots of child components for the child component.

On to the function body!

```haskell
pairUI = component render eval
  where
    render state =
        H.div_ [ H.slot $ state.topCounter
               , H.slot $ state.bottomCounter
               , H.button [ E.onClick $ E.input_ Reset ]
                          [ H.text "Reset!" ]
               ]
``` 

To put the child components in, we use the `H.slot` function, which accepts something of our `CounterSlot` type.
That identifies a place in the component where we'll put the child.
The button sends the `Reset` message, which will get handled by our `eval` function.

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

We need to define a way for the parent component to make children components:

```haskell
-- src/Example/Two.purs
ui :: forall g p. (Plus g) 
   => InstalledComponent State Ex1.State Input Ex1.Input g CounterSlot p
ui = install pairUI mkCounter
    where
        mkCounter (CounterSlot _) = createChild Ex1.ui (Ex1.init 0)
```

Since we don't care about the id of the counter when creating the counters, we can ignore it.
We use `createChild` to put a component as a child and provide the initial state.

Finally, running the counter works like you might expect:

```haskell
-- src/Main.purs
runEx2 = runUI Ex2.ui (installedState (Ex2.init))

main = -- boilerplate elided...
  app <- runEx2
  appendToBody app.node
```

We have to use `installedState` since we're dealing with parent/children components.

Alright, that's the first two examples from Elm Architecture in PureScript Halogen!
I'll be covering the rest in a future installment.
