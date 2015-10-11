---
title: "Elm Architecture in PureScript IV: Effects"
date: 2015-10-11
layout: post
categories: programming
---

## The Final Chapter-ing

In the last post, I covered higher order components and making dynamic lists of components.
We're going to get into effects and AJAXing with this.
It's almost entirely like you might expect, given the previous posts, but we'll finally start to specialize that `g` functor!

As always, the code is available in [this repository](https://github.com/parsonsmatt/purs-architecture-tutorial)

## Gif loader!

I'm pretty excited for this. Counters are boring and now I can get cat gifs delivered?
This is precisely what I want from all web UIs, really.

So, we're going to have a topic and a URL for the current gif.
The only input we'll need to handle is requesting a new gif.
We'll be interacting with the giphy public API to do this.

Let's define our state and inputs:

```haskell
-- src/Example/Five.purs

type State =
  { topic :: String
  , gifUrl :: String
  }

initialState :: State
initialState = { topic: "cats", gifUrl: "" }

data Input a
  = RequestMore a
```

STANDARD SUPER BORING, you knew it was coming.
Since we know we'll be using effects, we'll also define a type for the effects our component will be using:

```haskell
type GifEffects eff = HalogenEffects (ajax :: AJAX | eff)
```

Halogen defines a type of effects that it normally uses, and we're just adding the `AJAX` effect on top of that.

The `ui` component is pretty standard.
We've replaced the `g` functor with `Aff (GifEffects ())` to indicate that we'll be using the asynchronous effects monad.
The render function is boring, so we'll get right to the `eval` function.

```haskell
ui :: forall p. Component State Input (Aff (GifEffects ())) p
ui = component render eval
  where
    render :: -- ..
    -- *yawn* let's skip to eval
    eval :: Eval Input State Input (Aff (GifEffects ()))
    eval (RequestMore a) = do
      state <- get
      newGifUrl <- liftFI (fetchGif state.topic)
      modify \s -> if newGifUrl /= "error" 
                      then s { gifUrl = newGifUrl  }
                      else s { gifUrl = s.gifUrl }
      pure a
```

`liftFI` is a function that lifts effects from the free monad.
So we can phone home, launch missiles, write to the console, or do AJAX all from the `liftFI` function.
Well, to be precise, we can only do those things if they're included in the `Aff (GifEffects ())` effects type!
(I haven't checked `HalogenEffects`...)

`fetchGif` uses the `Affjax` library to make the request, read the JSON, and return the url or `"error"`.
(yes i know i should just handle the left case but whatever)


```haskell
fetchGif :: forall eff. String -> Aff (ajax :: AJAX | eff) String
fetchGif topic = do
    result <- AJ.get (giphyRequestUrl topic)
    let url = readProp "data" result.response >>= readProp "image_url"
    pure (either (const "error") id url)
```

`readProp` tries to read the JSON property of the object passed, and either returns the result or a `Left` error type if it wasn't successful.
That can be a quick way of dealing with data if you don't want to write a full JSON parser.

And that's it! We've got effects. NBD. Running the code in `main` looks the same as we'd expect:

```haskell
runEx5 = runUI Ex5.ui Ex5.initialState
```

## Multi Gif Loaders?!

Alright, how about a pair of gif loaders?
This is very similar to the pair of counters we had in two, but we don't need to worry about resetting them.

In fact, the entire bit of code (imports and all!) is 28 lines!

```haskell
module Example.Six where

import Prelude
import Control.Plus (Plus)
import Control.Monad.Aff (Aff())

import Halogen
import qualified Halogen.HTML.Indexed as H

import qualified Example.Five as Gif

data Input a = NoOp a

pairUI :: forall p. ParentComponent Unit Gif.State Input Gif.Input (Aff (Gif.GifEffects ())) Boolean p
pairUI = component render eval
  where
    render _ =
      H.div_
        [ H.slot true
        , H.slot false
        ]
    eval :: EvalP Input Unit Gif.State Input Gif.Input (Aff (Gif.GifEffects ())) Boolean p
    eval (NoOp a) = pure a

ui :: forall p. InstalledComponent Unit Gif.State Input Gif.Input (Aff (Gif.GifEffects ())) Boolean p
ui = install pairUI mkGif
  where
    mkGif _ = createChild Gif.ui (Gif.initialState)
```

I'm using `Boolean` as the slot type because it naturally only has two elements, and any type that just has two elements is equivalent to `boolean`, and this way I don't have to make ord/eq instances...

## List of Gifs

Next up is a list of gif downloaders.
But wait.
Instead of making a list of gif downloaders, let's just make another higher order component that contains a list of other components.

We'll model it off of `Example.Three`, so much of the code should look pretty familiar.
First we'll need to define state, query, child slots, etc...

```haskell
type StateP =
  { itemArray :: Array Int
  , nextID :: Int
  }

initialStateP :: StateP
initialStateP =
  { itemArray: []
  , nextID: 0
  }

data QueryP a
  = AddItem a
  | RemItem a

newtype Slot = Slot Int
```

We use the `P` suffix because we'll want to create type synonyms for the installed state and child query stuff.

The `Slot` type needs an instance of the Eq and Ord type classes.
Fortunately, the newer versions of PureScript include a mechanism for generically deriving these.
We have to import `Data.Generic`, and then we get to do:

```haskell
derive instance genericSlot :: Generic Slot

instance ordSlot :: Ord Slot where
  compare = gCompare

instance eqSlot :: Eq Slot where
  eq = gEq
```

Nice! Much less tedious than writing the instances out manually.
(Here's hoping that `deriving (Eq, Ord)` makes it into the language soon...)

Now we'll define the `listUI`.
Like we did with the higher-order "add a remove button" component, we'll use two type variables for the child state and child query.

```haskell
listUI :: forall g p s' f'. (Functor g)
       => ParentComponent StateP s' QueryP f' g Slot p
listUI = component render eval
  where
    render :: Render StateP QueryP Slot
    render state =
      H.div_
        [ H.button [ E.onClick $ E.input_ AddItem ]
                   [ H.text "+" ]
        , H.button [ E.onClick $ E.input_ RemItem ]
                   [ H.text "-" ]
        , H.ul_ (map (H.slot <<< Slot) state.itemArray)
        ]

    eval :: EvalP QueryP StateP s' QueryP f' g Slot p
    eval (AddItem next) = modify addItem $> next
    eval (RemItem next) = modify remItem $> next
``` 

The only new thing about this is the `$>` operator, but it does what you'd expect given it's place in the function.

Next, we'll define the type synonyms for convenience, and create the `makeList` function which takes a component definition and an initial state.

```haskell
type State s f g p =
  InstalledState StateP s QueryP f g Slot p

type Query f =
  Coproduct QueryP (ChildF Slot f)

initialState :: forall s f g p. State s f g p
initialState = installedState initialStateP

makeList :: forall s f g p. (Plus g)
         => Component s f g p
         -> s
         -> Component (State s f g p) (Query f) g p
makeList comp state = install listUI mkChild
  where
    mkChild (Slot _) = createChild comp state
```

And we're done with the component definition!
Let's run it and see where we go:

```haskell
-- src/Main.purs
runEx7 = runUI (Ex7.makeList Ex5.ui Ex5.initialState) Ex7.initialState
```

We don't even need a type signature. Nice!

And thus concludes my tutorial series on the Elm Architecture in PureScript.
I'm not going to cover animation because I don't know how it works, and that's beyond the scope of the Halogen framework.

### Other posts in the series:

1. [Elm vs PureScript I: War of the Hello, Worlds](http://www.parsonsmatt.org/programming/2015/10/03/elm_vs_purescript.html)
2. [Elm vs PureScript II](http://www.parsonsmatt.org/programming/2015/10/05/elm_vs_purescript_ii.html)
3. [Elm Architecture in PureScript III: Dynamic Lists of Counters](http://www.parsonsmatt.org/programming/2015/10/10/elm_architecture_in_purescript_iii.html)
