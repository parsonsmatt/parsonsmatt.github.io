---
title: "purescript-routing Tutorial"
date: 2015-10-22
layout: post
categories: programming
---

Not only has SlamData came up with [purescript-halogen](https://github.com/slamdata/purescript-halogen), they've also got a nice routing library [`purescript-routing`](https://github.com/slamdata/purescript-routing).
While I'll be demonstrating it with the `purescript-halogen` library, it's actually library agnostic and should work with anything.
Let's dive in and learn how to use it!

Now, fair warning, this is alpha software and bleeding edge.
This tutorial may be out of date by the time I post it!

## Defining Routes

The first step is defining our routes. We're making a website for logging weightlifting sessions, so we're concerned with three things:

1. Getting home. Safety is important and it's a dangerous world out there.
2. Logging sessions. That's literally the point, right?
3. Viewing our own profile. Only our own. Vanity is key to success in lifting weights.

```haskell
data Routes
  = Profile
  | Sessions
  | Home
```

Now that we've defined the data type, we need to write a matcher.
This is a function that takes the stuff after the `#` in the URL and figures out what item in our `Routes` is the right thing.
For this super basic example, we're just going to have the three pages above, so we'll just parse literals:

```haskell
routing :: Match Routes
routing = Profile <$ lit "" <* lit "profile"
      <|> Sessions <$ lit "" <* lit "sessions"
      <|> Home <$ lit ""
```

"What's that `lit ""` business?" Well, the routing library strips out all of the slashes, so if we want to refer to a single slash, we have to use the `lit ""` bit.

Let's define our Halogen component that will be in charge of routing.
Right now, it'll simply be a bit of text telling us which page we're on.
We'll keep track of the current page in our state, and use the input query algebra to change.

```haskell
type State = { currentPage :: String }

data Input a
  = Goto Routes a

ui :: forall g. (Functor g) => Component State Input g
ui = component render eval
  where
    render st =
      div_
        [ H.h1_ [ H.text (st.currentPage) ]
        , H.p_ [ H.text "Routing!!" ]
        ]

    eval :: Eval Input State Input g
    eval (Goto (Sessions next)) = do
      modify (_{ currentPage = "Sessions" })
      pure next
    eval (Goto (Home next)) = do
      modify (_{ currentPage = "Home" })
      pure next
    eval (Goto (Profile next)) = do
      modify (_{ currentPage = "Profile" })
      pure next
```

Cool! Now, we can use these `Goto` queries to have our application "go to" a certain route.
We've got our route matching defined, and a way for our component to react to routes.
Let's run our component:

```haskell
main = runAff throwException (const (pure unit)) $ do
  app <- runUI R.ui R.init
  appendToBody app.node
```

When we do `runUI`, we get a record back.
The node is the most obvious thing.
It's how we mount components to the DOM.
The `app` record also includes a `driver` field, which is a function that takes data in the query algebra.
We can use that to send messages to our routing component.
Let's write a function that accepts the driver, matches the route, and sends messages to our component.

```haskell
type Effects e = (dom :: DOM, avar :: AVAR, err :: EXCEPTION | e)

routeSignal :: forall eff. Driver Input eff
            -> Aff (Effects eff) Unit
routeSignal driver = do
  Tuple old new <- matchesAff routing
  redirects driver old new
```

`matchesAff` is a function that takes our `routing` definition, watches the URL, and returns a tuple of `Maybe oldRoute` and `newRoute`.
It runs asynchronously and will kick off the redirect function every time the URL changes.
We want to have `routeSignal` be it's own function in the event that we need to do some additional work here.
Now, it's time for `redirects`:

```haskell
redirects :: forall eff. Driver Input eff
          -> Maybe Routes
          -> Routes
          -> Aff (Effects eff) Unit
redirects driver _ Sessions = do
  driver (action (Goto Sessions))
redirects driver _ Profile = do
  driver (action (Goto Profile))
redirects driver _ Home = do
  driver (action (Goto Home))
```

Finally, we're using the `action` to send messages to our driver.
We could have expressed that as a one liner `redirects driver _ = driver <<< action <<< Goto`, but we'll be wanting to do some more work here pretty quick.

We'll want to "fork" a process in our main function to run the `routeSignal` function.
The `purescript-aff` package simulates forking with asynchronous code.
We'll add a line to our `main` function, and when we run it, we can watch it match routes!

```haskell
main :: forall eff. Eff (R.Effects eff) Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI R.ui R.init
  appendToBody app.node
  forkAff $ R.routeSignal app.driver
```

Now we can `pulp server`, open the browser, and sure enough, `localhost:1337/#/profile` causes the title to show "Profile".
Very cool! Let's put some links in our component and see how it can drive the global state:

```haskell
ui :: forall g. (Functor g) => Component State Input g
ui = component render eval
  where
    render st =
      div_
        [ H.h1_ [ H.text (st.currentPage) ]
        , H.ul_ (map link ["Sessions", "Profile", "Home"])
        ]
    link s = H.li_ [ H.a [ P.href ("#/" ++ toLower s) ] [ H.text s ] ]
```

So URLs and plain anchor tags can now act as a way to drive our application.
The routing library is pretty low level still -- there's a good bit of room available for a higher level routing library specifically for Halogen.

Note that the Home link still goes to the home page, even though the link is `#/home`.
That's because it goes to the last defined route in the event that no routes match.
It's a good idea to make the last route a catch-all 404 type thing.

Now, we've got a basic Sessions route.
Let's expand that to have some basic CRUD actions: index and show.
Show takes an identifier (`Int` in this case), while Index just shows everything.
We'll update the Sessions route to also take this as a parameter.

```haskell
data CRUD
  = Index
  | Show Number

data Routes
  = Home
  | Profile
  | Sessions CRUD
```

Immediately, `pulp -w build` complains.
We need to update our `matches` function to take into account the `CRUD` parameters.
We also need to update our component's `eval` function.

First, let's just recover our original index behavior in the routing function.
We'll need to match the slash, the sessions literal, and finally apply it to `pure Index`.

```haskell
routing :: Match Routes
routing = Profile <$ lit "" <* lit "profile"
      <|> Home <$ lit ""
      <|> Sessions <$> (lit "" *> lit "sessions" *> pure Index)
```

Now, we'll want to use the `Alternative` to allow it to choose between either `Show Number` or `Index`:

```haskell
routing :: Match Routes
routing = Profile <$ lit "" <* lit "profile"
      <|> Home <$ lit ""
      <|> Sessions <$> (lit "" *> lit "sessions" *> (Show <$> num <|> pure Index)
```

Except, man, that's kind of ugly... Let's make that a bit nicer:

```haskell
routing :: Match Routes
routing = profile
      <|> sessions
      <|> home
  where
    route str = lit "" *> lit str
    parseCRUD = Show <$> num <|> pure Index
    profile = Profile <$ route "profile"
    home = Home <$ lit ""
    sessions = Sessions <$> (route "sessions" *> parseCRUD)
```

Much nicer! It's starting to become clear that there's a lot of room for making conveniences on top of this, especially for a routing component library... 

Now we need to update the route matching function:

```haskell
redirects :: forall eff. Driver Input eff
          -> Maybe Routes
          -> Routes
          -> Aff (Effects eff) Unit
redirects driver _ = driver <<< action <<< Goto
```

Yeah, that's actually nicer... for now! Let's check the `eval` function:

```haskell
    eval :: Eval Input State Input g
    eval (Goto Profile next) = do
      modify (_ { currentPage = "Profile" })
      pure next
    eval (Goto (Sessions view) next) = do
      modify case view of
                  Index -> (_ { currentPage = "Sessions" })
                  Show n -> (_ { currentPage = "View Session " ++ show n })
      pure next
    eval (Goto Home next) = do
      modify (_ { currentPage = "Home" })
      pure next
```

Now, we can type `localhost:1337/#/sessions/2` and it'll change the title to "View Session 2.0".

This is all very cool.
We have URL-driven state in our Halogen app.
But we're managing everything in a single top level component, and that `eval` function is already getting hairy.
What we really want to do is have the routing component simply select the appropriate component and render that.
We'll define two new components: `Profile` and `Sessions` to handle the respective pages.

```haskell
-- src/Components/Profile.purs
data Input a = Noop a

type State = Unit

data Slot = Slot

ui :: forall g. (Functor g) => Component State Input g
ui = component render eval
  where
    render _ =
      H.div_
        [ H.h1_ [ H.text "Your Profile" ]
        , H.p_ [ H.text "what a nice profile!" ]
        ]

    eval :: Eval _ _ _ g
    eval (Noop n) = pure n
```

The `Sessions` component is the same for now, but it has slightly different text.
Now we're about to get into `purescript-halogen`'s machinery for having a parent component with multiple types of child components.
We have to define a way for Halogen to know how to route the inputs, and how to get at the child states.
Halogen uses `Coproduct` to route queries (`Coproduct f g a` is a newtype around `Either (f a) (g a)`), and `Either` to route states.

First, we'll define our child state:

```haskell
type ChildState = Either Profile.State Sessions.State
```

If we have more than one child component, then we can nest `Eithers`:

```haskell
type Ex whatever = Either Profile.State (Either Sessions.State whatever)
```

The child query is essentially the same thing.
We have to ensure that the components states and queries have the same "paths".

```haskell
type ChildQuery = Coproduct Profile.Input Sessions.Input
```

Like above, we can nest Coproducts to route more than two kinds of input to their respective query.
Next up is a type for the slot. We'll use Either again, making sure that the types line up.

```haskell
type ChildSlot = Either Profile.Slot Sessions.Slot
```

We'll want to define some convenience functions to route the actions appropriately from the router.

```haskell
pathToProfile :: ChildPath
                 Profile.State
                 ChildState
                 Profile.Input
                 ChildQuery
                 Profile.Slot
                 ChildSlot
pathToProfile = cpL

pathToSessions :: ChildPath
                  Sessions.State
                  ChildState
                  Sessions.Input
                  ChildQuery
                  Sessions.Slot
                  ChildSlot
pathToSessions = cpR
```

Another giant type signature! `ChildPath` wants to know state, input, and slot for the child and containing components.

Two more type aliases and we'll be done with the boilerplate.

```haskell
type StateP g
  = InstalledState State ChildState Input ChildQuery g ChildSlot

type QueryP
  = Coproduct Input (ChildF ChildSlot ChildQuery)
```

Ok, with all that out of the way, it's time to revise our router component definition.
We'll use our new type synonyms and make it a parent component.

```haskell
ui :: forall g. (Plus g) 
   => Component (StateP g) QueryP g
ui = parentComponent render eval
  where
    render state =
      L.defaultLayout
        [ H.h1_ [ H.text state.currentPage ]
        , H.p_ 
          [ H.text "QuickLift is a quick and easy way to log your weightlifting sessions."
          ]
        , viewPage state.currentPage
        ]
```

We'll use `viewPage` as a helper function to select the correct page from our various UIs.
It's pretty hacky.

```haskell
    viewPage :: String -> HTML (SlotConstructor ChildState ChildQuery g ChildSlot) Input
    viewPage "Sessions" =
      H.slot' pathToSessions Sessions.Slot \_ -> { component: Sessions.ui, initialState: unit }
    viewPage "Profile" =
      H.slot' pathToProfile Profile.Slot \_ -> { component: Profile.ui, initialState: unit }
    viewPage _ =
      H.div_ []

    eval :: EvalParent Input State ChildState Input ChildQuery g ChildSlot
    eval = ...
```

The type signature of `eval` is all that changed, so I'll elide the definition.

There are two remaining adjustments to make:

Change the `redirects` and `routeSignal` functions to account for the new types and `Coproduct` stuff:

```haskell
routeSignal :: forall eff. Driver QueryP eff
            -> Aff (Effects eff) Unit
routeSignal driver = do
  Tuple old new <- matchesAff routing
  redirects driver old new

redirects :: forall eff. Driver QueryP eff
          -> Maybe Routes
          -> Routes
          -> Aff (Effects eff) Unit
redirects driver _ =
  driver <<< Coproduct <<< Left <<< action <<< Goto
-- or, if you prefer writing it all out,
-- redirects driver _ Home = 
--   driver (Coproduct (Left (action (Goto Home))))
-- etc...
```


Change the `main` definition to use `installedState` instead of normal state:

```haskell
main :: forall eff. Eff (R.Effects eff) Unit
main = runAff throwException (const (pure unit)) $ do
  app <- runUI R.ui (installedState R.init)
  appendToBody app.node
  forkAff $ R.routeSignal app.driver

```

In any case, this works!
It correctly chooses the right component based on the current URL state.

So, to review, we can now:

- Define routes
- Define helpers and CRUD actions for routes
- Use the router in a single component to manage component state
- Use the router among multiple components to direct which component renders.

This should be enough to get you started with `purescript-routing`.
