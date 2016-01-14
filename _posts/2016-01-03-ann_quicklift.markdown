---
title: "ANN: QuickLift"
date: 2016-01-03
layout: post
categories: programming
---

I'm happy to announce that my project QuickLift has finally reached the point where I can start using it for myself!
It's still *definitely* alpha level software, so I don't really recommend anyone else using it right now.
Not that there's any real risk of that, I don't think, since it has basically zero features.

QuickLift is a weightlifting logging web application.
I've developed the back end in Haskell using the Servant framework, and the front end currently in PureScript using the Halogen framework.
That I'm intending to make it a weightlifting application is less cool than that I've intentionally built it thus far to be a reasonably useful scaffold for building functional single page applications.

I've made a git branch for the state of the repositories as of these blogposts, so that code will be available.

This post will serve as a bit of a walkthrough of the QuickLift application.
Along the way, I'll make notes on what will likely be factored out into it's own libraries, and where parts could be improved a lot.

Here are the relevant links:

- [QuickLift](http://quicklift.parsonsmatt.org) can be checked out here.
- [The backend repository](https://github.com/parsonsmatt/quicklift/tree/blogpost)
- [The PureScript frontend repository](https://github.com/parsonsmatt/ql-purs/tree/blogpost)

# The Backend

The QuickLift backend is fairly standard for a Haskell Servant application.
If you've seen my [servant-persistent]({% post_url 2015-06-07-servant-persistent %}) tutorial, then you've seen most of what the back end is all about.
The monad and configuration I'm using are presented here:

```haskell
type AppM = ReaderT Config (EitherT ServantErr IO)

data Config
    = Config
    { getPool :: ConnectionPool
    , getEnv  :: Environment
    }
```

I'll cover some of the other items here:

## User Authentication

User authentication is currently an open problem in Servant, though they're working on getting a [blessed solution](https://github.com/haskell-servant/servant/pull/311) for the v0.5 release.
I'm doing the bare minimum to work, with the minimum of magic.
I'm also leveraging the excellent [`users`](https://hackage.haskell.org/package/users-0.3.0.0) library to handle the boilerplate around user management.

I'm encapsulating the authentication and registration handlers in a `UserAPI` type, presented here:

```haskell
type UserAPI = Get '[JSON] [Person]
    :<|> ReqBody '[JSON] Registration :> Post '[JSON] (Either Text.Text Int64)
    :<|> "login" :> ReqBody '[JSON] Auth :> Post '[JSON] (Maybe AuthResponse)
    :<|> "verify" :> ReqBody '[JSON] Text :> Post '[JSON] (Maybe AuthResponse)
```

The first endpoint returns a list of all users.
The second is used to register a new user account, returning either a text error message or the user ID.
The third is used to log a user in, and returns an `AuthResponse` if the login was successful.
`verify` is used to get the user account information for a given authentication token.

I've used some Template Haskell [as documented in this tutorial](http://www.parsonsmatt.org/2015/11/15/template_haskell.html) to make the Users code available in my `AppM` monad, so `getUsers` is just:

```haskell
-- Get '[JSON] [Person]
getUsers :: AppM [Person]
getUsers = do
    users <- listUsers Nothing
    return (map (uncurry userToPerson) users)
```

I put the Servant API type description for the handler above the function to make it a bit more clear what is going on.

Registering a new user account is also pretty easy:

```haskell
-- ReqBody '[JSON] Registration :> Post '[JSON] (Either Text.Text Int64)
registerUser :: Registration -> AppM (Either Text.Text Int64)
registerUser reg = do
    user <- createUser (convertRegistration reg)
    return $ either (Left . Text.pack . show) (Right . fromSqlKey) user

data Registration
    = Registration
    { regName :: Text
    , regEmail :: Text
    , regPassword :: Text
    , regConfirmation :: Text
    } deriving (Eq, Show)
```

I could have done `bimap (Text.pack . show) (fromSqlKey) user` to save a few characters, but bifunctors are kinda scary right!
The `Registration` data type is just a dumb data type that I made to serve as the endpoint request.
This pattern is pretty common -- have a datatype corresponding to the input I expect from the endpoint, and a function to convert it to whatever internal format I need.
In this case, I use `convertRegistration` to convert a `Registration` value into a `User` value as expected by the `users` library.
This separation of concerns made it really easy to switch to the `users` library in the first place.

Since `servant-0.5` is going to come out with real legit authentication support soon, I went with something somewhat janky.

```haskell
-- "login" :> ReqBody '[JSON] Auth :> Post '[JSON] (Maybe AuthResponse)
authenticateUser :: Auth -> AppM (Maybe AuthResponse)
authenticateUser auth = runMaybeT $ do
    sessionId <- MaybeT $ authUser (authEmail auth) (WU.PasswordPlain $ authPassword auth) 1200000
    person <- lift $ getUser (authEmail auth)
    return $ AuthResponse sessionId person

data Auth
    = Auth
    { authEmail :: Text
    , authPassword :: Text
    , authConfirmation :: Text
    } deriving (Eq, Show)

data AuthResponse
    = AuthResponse
    { sessionId :: SessionId
    , person :: Person
    } deriving (Eq, Show, Generic)
```

Right now, it's sending a `Maybe AuthResponse`, though doing `AppM AuthResponse` and using an HTTP error code might be more appropriate to indicate a failed login.
We want to respond with both the session information as well as the actual user profile, so we pack them both in the `AuthResponse` value.
`getUser` is actually another function in the API -- this is a fantastic display of the composability of Servant handlers.

```haskell
-- This is actually for a different part of the API
getUser :: Text -> AppM Person
getUser k = do
    person <- runMaybeT $ do
        userid <- MaybeT $ getUserIdByName k
        user <- MaybeT $ getUserById userid
        return $ userToPerson userid user
    case person of
         Nothing -> lift $ left err404
         Just person -> return person
```

This function returns a user, or errors out with a 404.
If someone tries to login with a user that doesn't exist, then they get a 404 error.
Nice!

```haskell
verifyToken :: Text -> AppM (Maybe AuthResponse)
verifyToken sid = runMaybeT $ do
    let session = WU.SessionId sid
    userId <- MaybeT $ verifySession session 12000
    user <- MaybeT $ getUserById userId
    return (AuthResponse session (userToPerson userId user))
```

Verifying a token is similar.
We take an authentication token, ask `users` to verify the session.
If it's valid, we then get the user by ID and return an `AuthResponse`.

## Composable Handlers

So, `getUser` is a composable handler, and I'm reusing it all over the place in my app.
Here's the API it is defined in:

```haskell
type LifterAPI = Get '[JSON] [Person]
    :<|> Capture "name" Text :> (Get '[JSON] Person
                            :<|> "sessions" :> SessionAPI)

type SessionAPI = Get '[JSON] [Entity Liftsession]
    :<|> Header "auth" Text :> ReqBody '[JSON] Liftsession :> Post '[JSON] (Either Text Int64)
```

Now, I've got a sub-API that handles getting a person, and then a bunch of stuff for them to handle their weightlifting sessions.
(I know, weightlifting session, browser session, sigh)
At first, I was dismayed at the thought of writing repetitive `lift $ left err404` code to check for the user not being present.
Naturally, there's a better way, and the current implementation of the `lifters` logic is great:

```haskell
sessionServer :: Text -> ServerT SessionAPI AppM
sessionServer username = getSessions' :<|> createSession'
  where
    -- Get '[JSON] [Entity Liftsession]
    getSessions' :: AppM [Entity Liftsession]
    getSessions' = getUser username >>= getSessions


    -- Header "auth" Text
    -- :> ReqBody '[JSON] Liftsession
    -- :> Post '[JSON] (Either Text Int64)
    createSession' :: Maybe Text -> Liftsession -> AppM (Either Text Int64)
    createSession' Nothing _ = lift $ left err401
    createSession' (Just sid) s = do
        loginId <- verifySession (WU.SessionId sid) 10
        user <- getUser username
        if loginId == Just (personId user)
           then createSession s user
           else lift $ left err401
```

And by 'great', I mean "this could be way cooler but wow compared to Rails/Express..."
`createSession'` is actually using the authentication mechanism in an ad-hoc way.
API clients are required to put a header "auth" with their request.
If they don't send anything, then I `lift $ left err401` and their party is over.

If they do send a header, then I verify that it is a proper authentication token.
This gives me a `Maybe LoginId`.
Next, I `getUser`, and if the user's ID matches up with the one provided from the session, then I create the session.
Otherwise they get booted.

```haskell
getSessions :: Person -> AppM [Entity Liftsession]
getSessions Person {..} =
    runDb $ selectList [ LiftsessionUser ==. personId ] []

createSession :: Liftsession -> Person -> AppM (Either Text Int64)
createSession ls person = do
    let ls' = ls { liftsessionUser = personId person }
    key <- runDb $ insert ls'
    return . return . fromSqlKey $ key
```

`getSessions` doesn't have to worry about the user not being present, because `getUser` already dealt with that.
Likewise, `createSession` doesn't have to worry about authorizing the user because we took care of that upstream.

Servant's composable handlers are *really* cool.

In any case, that's the entire back end as it differs from the `servant-persistent` tutorial.
Let's check out the front end.

# The Frontend

The front end of QuickLift is currently written in PureScript using the Halogen UI library.
I'm planning on doing another front end in GHCjs with Reflex-dom, and perhaps another with Elm (and maybe even one with ClojureScript if I've got enough time).
All of the various ML-inspired JavaScript languages are fairly bleeding edge, with their own sets of trade offs.

PureScript has a very nice position, and has the advantage of being designed from the ground up to be great at compiling to JavaScript and avoiding making some of the same mistakes that Haskell has made.
The typeclass hierarchy is better.
The record system is awesome.
Some syntax is much better (no `$` needed before `do` or lambdas!), some is much worse (`<<<` is the default composition operator, though the lens package exports `..`).

PureScript also has a really good router, which Elm and GHCJS didn't really have at the time I started.
Routers are important for making SPAs useful and not counterintuitive -- the back button is your friend, and URLs are what makes the web great.

I'm using [Halogen](https://github.com/slamdata/purescript-halogen), which is a *beast* of a library.
I'm going to briefly cover the architecture and design, but you'll want to refer to my [Elm Architecture in PureScript](http://www.parsonsmatt.org/2015/10/05/elm_vs_purescript_ii.html) series, the [official introduction](https://github.com/slamdata/purescript-halogen#introduction), and the [excellent set of examples](https://github.com/slamdata/purescript-halogen/tree/master/examples) if you want to know what's going on in more depth.

### Note to the future:

The PureScript ecosystem is evolving extremely rapidly, and it's likely that the code in the `blogpost` branch will bitrot.
I've tightened the dependencies in the `bower.json` file, but PureScript and `pulp` themselves might evolve and break the project.
I'll try to keep it updated and building, but for posterity, this is the version information that makes it go:

- PureScript v0.7.6.3 or v0.8 RC 1
- `pulp` versions 4.4 and 7.0.0 tested

## `Main.purs`

The Main module kicks off the application, router, and digs an auth token out of LocalStorage.

```haskell
-- ...

import QuickLift as Q
import QuickLift.State as S
import Router as R
import Types (QL())

main :: forall eff. Eff (QL eff) Unit
main = do
    token <- WS.getItem WS.localStorage "auth"
    runAff throwException (const (pure unit)) do
        app <- runUI Q.ui S.initialState { authToken = token }
        appendToBody app.node
        forkAff $ R.routeSignal app.driver
```

The router is the next interesting bit of the application.

## `Router.purs`

I wrote [an introductory tutorial](http://www.parsonsmatt.org/2015/10/22/purescript_router.html) on using `purescript-routing` with `purescript-halogen`.
If you're wanting more detail, check that out.
I'll briefly cover the main differences here.

```haskell
routing :: Match Routes
routing = profile
      <|> sessions
      <|> register
      <|> login
      <|> logout
      <|> home
  where
    login = Login <$ route "login"
    logout = Logout <$ route "logout"
    register = Registration <$ route "register"
    profile = Profile <$ route "profile"
    home = Home <$ lit ""
    sessions = Sessions <$> (route "sessions" *> parseCRUD)
    route str = lit "" *> lit str
    parseCRUD = Show <$> int
            <|> New <$ lit "new"
            <|> pure Index
    int = floor <$> num
```

I'm actually using an ADT in this implementation of the router, which is much nicer than a stringly typed one in the tutorial.
The routes library includes an Applicative-style parser, which should be right at home to anyone who's used Parsec or related.

I've also got a type class setup for generating URLs from the routes, and a link convenience function for my templates:

```haskell
class HasLink a where
    link :: a -> String

instance routesHasLink :: HasLink Routes where
    link Profile = "#/profile"
    link (Sessions crud) = "#/sessions" ++ link crud
    link Home = "#/"
    link Registration = "#/register"
    link Login = "#/login"
    link Logout = "#/logout"

instance crudHasLink :: HasLink CRUD where
    link Index = ""
    link New = "/new"
    link (Show n) = "/" ++ show n

(</>) :: forall a b. (HasLink a, HasLink b) => (a -> b) -> a -> b
(</>) = ($)

linkTo :: Routes -> String -> HTML _ _
linkTo r t = H.a [ P.href (link r) ] [ H.text t ]
```

I'd like to get this split off as a library for use with `halogen` and `routing`, but I'd like to get something that can generate both the route parsing and URL generation from a single source of truth.

## `QuickLift.purs`

Next up, we'll check out the component definition.
I started the app going _crazy_ with subcomponents, but ended up finding that it was massive unweildy to have so many coproducts floating around, and ended up refactoring everything and bringing it back into a single component.
As it happens, a single component is not annoying or painful at all to deal with yet, so this has been a great shift.

Like any good developer, I spread all my code far-and-wide:

```haskell
ui :: forall eff. Component State Input (QLEff eff)
ui = component render eval
  where
      render state =
          L.defaultLayout state
              [ renderView state.currentPage state
              ]
```

`L.defaultLayout` and `renderView` are in different modules.
I'm experimenting with different ways to template, and this has been pretty good so far.
Let's check out the `eval` logic:

```haskell
      eval :: Eval Input State Input (QLEff eff)
      eval (Goto route next) = do
          modify (_ { currentPage = route })

          case route of
               Registration -> modify (stCurrentUser .~ Just emptyUser)
               Sessions Index -> eval (LoadSessions next) $> unit
               Logout -> eval (UserLogout next) $> unit
               _ -> pure unit

          st <- get

          unless (isJust st.currentUser) do
              for_ st.authToken \auth -> do
                  res <- liftAff' (API.verifySession auth)
                  case res of
                       Nothing ->
                           modify (stAuthToken .~ Nothing)
                       Just (Tuple session user) -> do
                           liftEff' (WS.setItem WS.localStorage "auth" session)
                           modify (stErrors ?~ [])
                           modify (stCurrentUser ?~ user)
                           modify (stAuthToken ?~ session)

          liftAff' (updateUrl route)

          pure next
```

This is the code that handles the routes.
The `State` type for QuickLift keeps track of the current page.
Then, depending on what the route is, we do some other stuff.
In two of those cases, we're punting to other cases of the `eval` function.
The rest of the function retrieves the user from the server if there's an auth token available.

```haskell
      eval (LoadSessions a) = do
          u <- gets _.currentUser
          for_ u \user -> do
              s <- liftAff' (API.getUserSessions user)
              modify .. set stLoadedSessions .. concat .. maybeToArray $ s
              pure a
```

Stateful asynchronous code with potential `null` values is so nice with Haskell, er, PureScript.
`Maybe`'s `Traversable` instance works to great effect here.
And not having to write `$` looks so much nicer.
Getting to use lenses is fun too.
This is what programming should be like!

Handling forms is good too:

```haskell
      handleNewSession (Edit fn) = modify (stCurrentSession %~ fn)
      handleNewSession Submit = do
          auth <- gets (\s -> Tuple <$> s.authToken <*> s.currentUser)
          for_ auth \(Tuple token user) -> do
              sess <- gets _.currentSession
              result <- liftAff' (API.postSession token user sess)
              for_ result \n -> do
                  let saved' = sess # _Session .. id_ .~ n
                      rt = Sessions </> Show n
                  modify (stCurrentSession .~ saved')
                  modify (stLoadedSessions %~ (saved' :))
                  eval (Goto rt unit)
```

This comes from my `Form` module, which I'm planning on spinning off into a library.
We get more fun with the `Traversable` instance of `Maybe`, first checking to see if a user and authentication token are available.
If they both are, then we get the current session from the state, post it to the API, and if there's a result, we save it.
`eval (Goto (Sessions </> Show n)) unit)` lets us send redirects from within the application handlers.

## `QuickLift/View.purs`

Let's check out the view.
The layout is mostly boring code that I'm reusing to wrap all the views in, so we can skip it.
The `renderView` function is defined here:

```haskell
renderView :: Routes -> State -> ComponentHTML Input
renderView Home _ =
    H.div_
        [ H.h1_ [ H.text "QuickLift" ]
        , H.p_ [ H.text "Welcome to QuickLift" ]
        ]
```

We're pattern matching on the `Routes` to determine which page to visit, and we have access to the `State` to render data.

For creating sessions, there's some CRUD to deal with:

```haskell
renderView (Sessions Index) st =
  let sessions = case map linkSession st.loadedSessions of
                      [] -> H.p_ [ H.text "No sessions." ]
                      xs -> H.ul_ (map (H.li_ <<< pure) xs)
   in H.div_
    [ newButton
    , loadButton
    , sessions
    ]


renderView (Sessions (Show n)) st =
  let maybeIndex = findIndex (eq n .. view (_Session .. id_)) st.loadedSessions
      session = maybeIndex >>= index (st ^. stLoadedSessions)
   in showPage n session

-- ... later ...
showPage :: forall a. Int -> Maybe Session -> HTML a Input
showPage n (Just (Session s)) =
  H.div_
    [ H.h1_ [ H.text $ yyyy_mm_dd s.date ]
    , H.p_ [ H.text s.text ]
    , newButton
    ]
showPage n Nothing =
  H.div_
    [ H.h2_ [ H.text "hmm, not found... load it?" ]
    , loadButton
    ]

```

lol @ mapping over the sessions in a case statement what am i doing.
In any case, the views here are pretty non-remarkable if you're used to Lucid or other Haskell/Elm templating solutions.

The `New` route is interesting:

```haskell
renderView (Sessions New) st =
  H.div_
    [ F.form (NewSession Submit)
      [ F.textarea "session" "Session:"
        (st.currentSession ^. _Session .. text_)
        (NewSession .. Edit .. set (_Session .. text_))
      , F.date "date" "Date:"
        (yyyy_mm_dd (st.currentSession ^. _Session .. date_))
        (NewSession .. Edit .. edDate)
      ]
    ]
  where
    edDate :: String -> Session -> Session
    edDate str sess =
      let d = fromMaybe (sess ^. _Session .. date_) (dateFromString str)
       in sess # _Session .. date_ .~ d
```

I'm using a form abstraction that's based on `lens` here.
`F.textarea` is a function that takes an ID, a label, an initial value, and a function that takes a String and the target of the lens and updates the state.
I'm intending to explore that abstraction more, but haven't had the chance to build it out.

## Forms

I also built out a `Writer` based form that takes advantage of lenses for user stuff:

```haskell
renderView Registration st =
  H.div_ $ errs st.errors :
    WF.renderForm st.registration Register do
      WF.textField "name" "User name:" (_UserReg .. name) urlSafe
      WF.emailField "email" "Email:" (_UserReg .. email) validEmail
      WF.passwordField "password" "Password:" (_UserReg .. password) validPassword
      WF.passwordField "confirm" "Confirmation:" (_UserReg .. confirmation) validConfirmation
  where
    validPassword str
      | Str.length str < 6 = Left "Password must be at least 6 characters"
      | otherwise = Right str
    validConfirmation str
      | str == st ^. stRegistration .. _UserReg .. password = Right str
      | otherwise = Left "Password must match confirmation"
    validEmail str =
      maybe (Left "Must have @ symbol") (const (Right str)) (Str.indexOf "@" str)
    urlSafe str =
      case Reg.match (Reg.regex "^[\\w\\d-_]*$" Reg.noFlags) str of
           Just _ -> Right str
           Nothing -> Left "Only alphanumeric characters, '_', and '-' are allowed."
```

So the `WF` WriterForm is a bit clever, though I need to finish it.
`WF.renderForm` takes 1) a thing to operate on, 2) an action in the Halogen query algebra which has a constructor which accepts a `FormInput` data, and 3) a series of input fields in the `WForm` monad (which is just a ReaderT Writer).
Each field accepts an ID, a label, and a lens into the object, and a validation function with type `String -> Either String a` where `a` is the thing being constructed.
(todo: construct more than just strings and i guess have a show function?)

The query algebra I've got setup looks like this:

```haskell
data Input a
    = Goto Routes a
    | GetUser Int a
    | LoadSessions a
    | NewSession (FormInput Session) a
    | Register (FormInput UserReg) a
    | Authenticate (FormInput UserAuth) a
    | UserLogout a
```

So, for `Register`, we've got an action that takes (as first argument) a `FormInput UserReg`.
A `FormInput` is simply:

```haskell
data FormInput a
  = Submit
  | Edit (a -> a)
```

This ends up working quite well!
I do need to integrate server received errors better somehow.

## Models

There's a decent amount of boilerplate involved with PureScript right now.
Generic deriving arrived in 0.7.3, which has helped tremendously, but it's not all there yet unfortunately.

There are [85 lines in this module](https://github.com/parsonsmatt/ql-purs/blob/blogpost/src/QuickLift/Model/Registration.purs), almost all of which are boilerplate.
This wasn't much fun to write, and I'll have to do it again for each and every one of my objects, it seems!
I'll likely write an abstraction or type class of my own to make this more convenient, but there's a surprising amount one has to write in order to have convenient serialization and deserialization of types.

Also I might be doing this *totally wrong* and would love if someone could show me a better way!
Deriving lenses automatically would be great too...

## API

Affjax makes dealing with the API requests really great.

```haskell
postRegistration :: forall eff. UserReg -> Aff (ajax :: AJAX | eff) (Either String Int)
postRegistration u = do
    { response: res } <- qlPost "users" u
    pure $ joinForeign show res


verifySession
    :: forall eff
     . String
    -> Aff (ajax :: AJAX | eff) (Maybe (Tuple String User))
verifySession token = do
    { response: res } <- qlPost "users/verify" (show token)
    pure <<< eitherToMaybe $
        Tuple <$> readProp "sessionId" res <*> readProp "person" res
```

`qlPost` and `joinForeign` are defined in the [`QuickLift.API.Util`](https://github.com/parsonsmatt/ql-purs/blob/blogpost/src/QuickLift/Api/Util.purs) module.

# The Future...

Well, that's QuickLift right now.
Here's what you can look out for later:

## An actually useful app

that's right! It'll be way cooler when I've had more time to implement features and improvements.

## An easily packaged SPA scaffold

You can copy the repositories as-is right now and modify them to your own desires.
Authentication is covered, as well as super basic CRUD.
I want to make a `stack` template for easy distribution, and make this as simple as possible to get running.

## More libraries spun out

moar sharing

moaaarrrrr

I want to polish up the forms library, my utilities for routing, and my utilities for dealing with JSON/API stuff.
They're all general enough that I don't think they belong in app code.

## Deployment with Keter

I need to get this deployed with Keter.
Manual deployment isn't bad but automation is great.
