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
`purescript-halogen` seems to be a good bit closer, so I'll use that one instead.

The repository with the code is available [here](https://github.com/parsonsmatt/purs-architecture-tutorial).

# 0. Hello World!

Since I didn't do it in the previous post, here's "Hello World!" in Halogen:

```haskell
-- src/Example/Zero.purs
data Id a = Id a

ui :: forall g p. (Functor g) => Component Unit Id g p
ui = component render eval
    where
        render _ =
            H.div_ [ H.h1 [] [ H.text "Hello, World!" ] ]
        eval :: Eval Id Unit Id g
        eval (Id a) = pure a

-- src/Main.purs
import qualified Example.Zero as Ex0

main = runAff throwException (const (pure unit)) $ do
    node <- runUI Ex0.ui unit
    appendToBody node.node
```

Halogen expects the Input type to be have kind `* -> *`.

# 1. Counter
