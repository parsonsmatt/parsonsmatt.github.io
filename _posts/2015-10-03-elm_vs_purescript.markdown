---
title: "Elm vs PureScript I"
date: 2015-10-03
layout: post
categories: programming
---

## War of the Hello, Worlds

I'm building a web application with Haskell, and I'd like the front end to be functional as well.
I could write it all in JavaScript, but that sounds boring.
I could go the other direction and write it all in Haskell, but I can't figure out how to build GHCjs (and have concerns about performance of compiled Haskell).
I could learn ClojureScript, but that's a big investment, and I mostly want to get something built.

That leaves Elm and PureScript.
Elm is a fairly simple functional language with a focus on developing browser UIs and making it easy to learn and use.
PureScript is an advanced functional language that is quite a bit more general and powerful.
Where Elm prioritises easy development, PureScript prioritizes powerful language features and abstractions. 
PureScript's libraries and frameworks for developing applications are a bit more immature than Elm, but that's somewhat to be expected given the relative age of the languages.

This post seeks to evaluate both PureScript and Elm for the purpose of building a single page application from the perspective of a relative newbie.

## "Hello, World"

### Elm:

Elm's CLI is rather nice. Getting started with a project is just:

```bash
$ mkdir elm-project && cd elm-project
$ elm package install
```

And we can get "Hello World" on the screen with a few commands:

```bash
$ cat > Main.elm
module Main where
import Html exposing (..)
main = h1 [] [text "Hello World!"]
^C
$ elm package install evancz/elm-html --yes
$ elm reactor
```

`http://localhost:8000`, click on `Main.elm` and you'll see Hello World. Nice!

Getting trivial functionality in applications via signals is pretty easy, as demonstrated by the excellent [Elm Architecture](https://github.com/evancz/elm-architecture-tutorial/) tutorial.
Elm's `reactor` server is *very* fast, which makes for a rather nice development cycle.

### PureScript:

The [pulp](https://github.com/bodil/pulp) build tool is excellent, and we can get a project started pretty easily:

```bash
$ mkdir purs-project && cd purs-project
$ pulp init
$ cat > index.html
<head><script src="app.js"></script></head>
^C
$ pulp server
```

Now we can go to `http://localhost:1337`, open the console and see that `"Hello sailor!"` has been logged. 

PureScript, being more general and modular by default than Elm, requires a bit more work before you can have something up on the screen.
There are a few frameworks for PureScript apps:

#### `purescript-react` 

is a library for low level React bindings.


#### `purescript-halogen`

is a high level framework based on `virtual-dom` that seems extremely advanced and powerful.
Unfortunately, the power comes with complexity, and the documentation, API, and examples seem to be in a state of flux.

#### `purescript-thermite`

is a higher level framework based on `purescript-react`.
It looks nicer to use and more abstract, but at the cost of some missing features.
The examples and documentation are kept up to date and are quite readable.
For this reason, I'll go with it!

### Getting Hello World on the screen...

First, we need to install our dependencies:

```bash
$ pulp dep install --save purescript-thermite
```

Well, there's quite a bit of boilerplate...

```haskell
module Main where

import Prelude

import Data.Maybe
import Control.Monad.Eff
import Data.Maybe.Unsafe
import Data.Nullable (toMaybe)

import qualified Thermite as T
import qualified Thermite.Action as T

import qualified React as R
import qualified React.DOM as R
import qualified React.DOM.Props as RP
import qualified DOM as DOM
import qualified DOM.HTML as DOM
import qualified DOM.HTML.Document as DOM
import qualified DOM.HTML.Types as DOM
import qualified DOM.HTML.Window as DOM
```

(see what I meant about the granularity and modularity?)

The actual code to get "Hello World" up isn't so bad:

```haskell
render :: T.Render _ {} _ {}
render _ _ _ _ = R.div' [ R.h1' [ R.text "Hello world!" ] ]

spec :: T.Spec _ {} _ {}
spec = T.simpleSpec {} perfAction render

perfAction :: T.PerformAction _ {} _ {}
perfAction _ _ = T.modifyState (const {})

main :: forall eff. Eff (dom :: DOM.DOM | eff) R.ReactElement
main = body >>= R.render (R.createFactory (T.createClass spec) {})
    where
        body = do
            win <- DOM.window 
            doc <- DOM.document win
            elm <- fromJust <$> toMaybe <$> DOM.body doc
            return $ DOM.htmlElementToElement elm
```

Thermite requires that we declare a `State` and action handlers.
I used `Unit` for everything, and just rendered those divs to the screen.
The `index.html` page needs to be modified to include a link to `React` in the head, and link to `app.js` *after* the body loads.

So that's a comparison on "Hello World" for the two.
Elm's quite a bit simpler, but we'll see how PureScript's more powerful language plays out in the more complex examples.

### Other posts in this series:


1. [Elm vs PureScript II](http://www.parsonsmatt.org/2015/10/05/elm_vs_purescript_ii.html)
2. [Elm Architecture in PureScript III: Dynamic Lists of Counters](http://www.parsonsmatt.org/2015/10/10/elm_architecture_in_purescript_iii.html)
3. [Elm Architecture in PureScript IV: Effects](http://www.parsonsmatt.org/2015/10/11/elm_architecture_in_purescript_iv_effects.html)
