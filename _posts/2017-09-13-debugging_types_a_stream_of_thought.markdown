---
title: "Debugging Types: A Stream of Thought"
date: 2017-09-13
layout: post
categories: programming
---

At the day job, we've got a whole bunch of database models.
And it's somewhat easy to accidentally query the wrong database.
Fortunately, Persistent has a mechanism for fixing that -- using type-specific backends!
However, Persistent's mechanism was *not* designed around this sort of use case, so I've had to work around it.

I wrote a wrapper library called [`persistent-typed-db`](https://github.com/parsonsmatt/persistent-typed-db) to enable type safe access.
It's almost entirely vendored code from `persistent` with a phantom type variable for the database you're accessing.
I got to work on integrating the library into the work codebase, and ran into a bunch of road blocks.

As part of the debugging process at work, we've started writing stream-of-thought "as it happens" debugging logs.
They've been tremendously helpful for sharing workflow, thought processes, and "why is this path a dead end?" which doesn't typically make it's way in process documentation.

Since this debugging workflow was mostly for open source stuff (Esqueleto, Persistent, and my wrapper library), I figured I'd post the entire flow here.
It's mostly stream-of-thought and the direction isn't great, but it pretty closely mirrors the work and research I had to do to solve the problem.

(For best accuracy, read along while listening [to some fine Ghibli tunes](https://www.youtube.com/watch?v=3jWRrafhO7M))

# `persistent-typed-db`

The library will allow us to have type safety when running database queries, so that we don’t accidentally issue a texas-toast account query on an FBG master database (as an example).

The library needs to be compatible with Persistent and Esqueleto to be useful.
Currently:

- `persistent-typed-db` + `persistent:` great!
- `persistent-typed-db` + `esqueleto:` incompatible

Why is persistent-typed-db incompatible with Esqueleto?
Let's dig into the error we receive:

```
/home/matt/Projects/sellerlabs-hs/texas-toast/src/Texas/Query/Venue.hs:60:5: error:
    • Couldn't match type ‘persistent-typed-db-0.0.1.0:Database.Persist.Typed.SqlFor
                             TexAcctDb’
                     with ‘SqlBackend’
        arising from a use of ‘from’
    • In the expression:
        from
        $ \ (v `LeftOuterJoin` vs)
            -> do { on (just (v ^. VenueId) ==. vs ?. VenueSettingVenue);
                    pure (v, vs) }
      In an equation for ‘venueWithSettings’:
          venueWithSettings
            = from
              $ \ (v `LeftOuterJoin` vs)
                  -> do { on (just (v ^. VenueId) ==. vs ?. VenueSettingVenue);
                          .... }
```

The error comes from this code:

```haskell
venueWithSettings
    :: SqlQuery (SqlExpr (Entity Venue), SqlExpr (Maybe (Entity VenueSetting)))
venueWithSettings =
    from $ \(v `LeftOuterJoin` vs) -> do
        on (just (v ^. VenueId) ==. vs ?. VenueSettingVenue)
        pure (v, vs)
```

So, the error indicates that GHC is trying to unify `SqlFor TexAcctDb ~ SqlBackend` due to a use of `from`.
What is the type of `from`, and how is it specifying `SqlBackend`?

If we dig into `esqueleto`, we'll find `from` at line 935 in Database.Esqueleto.Internal.Language:

```haskell
from :: From query expr backend a => (a -> query b) -> query b
from = (from_ >>=)
```

Now we need to know what `From` is all about.

```haskell
-- | (Internal) Class that implements the tuple 'from' magic (see
-- 'fromStart').
class Esqueleto query expr backend => From query expr backend a where
  from_ :: query a
```

So `From` is a class that explains how to select a value of type `a` using a `query` that has an instance from `Esqueleto` class.
We need to dig intot he Esqueleto class to identify why it's coercing the backend.

Here is the class definition for Esqueleto:

```haskell
class (Functor query, Applicative query, Monad query) =>
      Esqueleto query expr backend | query -> expr backend, expr -> query backend where
```

The `->` arrows in the class definitions are "functional dependencies."
A simpler example is this guy:

```haskell
class Container container element | container -> element where
  toList :: container -> [element]

instance Container [a] a where
  toList = id

instance Container (Set a) a where
  toList = Set.toList

instance Container Text Char where
  toList = Text.unpack
```

This class and instances say: "For a given type `container`, the `element` type of that container is fully determined by the container."
For `[a]` and `Set a`, the element type of the container is the type that it is polymorphic over.
For `Text`, the element type is fixed to be `Char`.

Back to the Esqueleto class definition!
The functional dependencies state that the type of `query` is enough to select the type of `expr` and `backend`, and that the type of `expr` is sufficient to select the type of `query` and `backend`.
Practically, this means we can only have one instance for a given `query` *or* `expr` type -- we may not vary the `backend` and reuse query/expr types.

Our type signature for `venueWithSettings` *fixes* the type of `SqlQuery` and `SqlExpr`:

```haskell
venueWithSettings
    :: SqlQuery (SqlExpr (Entity Venue), SqlExpr (Maybe (Entity VenueSetting)))
```

When we do that, that tells GHC that it can *also* unambiguously select the backend: `SqlBackend`!

But, why does it complain that the backend is `SqlBackend`?
It must be asking GHC what the `PersistEntityBackend` is for the records, and when that doesn't line up with `SqlBackend`, it throws a type error.

Unfortunately, GHC's type checker does not include a step-through debugger.
So we have to prod it manually.

I replaced the type signature with a more polymorphic one, which should cause the compiler to defer making that selection for a bit.
That might give us some clues on how we can proceed.

```haskell
venueWithSettings
    :: Esqueleto q e b
    => q (e (Entity Venue), e (Maybe (Entity VenueSetting)))
```

Now, the query, expression, and backend are polymorphic again.
When we attempt to compile, we get more errors:

```
/home/matt/Projects/sellerlabs-hs/texas-toast/src/Texas/Query/Venue.hs:19:20: error:
    • Couldn't match type ‘persistent-typed-db-0.0.1.0:Database.Persist.Typed.SqlFor
                             TexAcctDb’
                     with ‘SqlBackend’
        arising from a use of ‘select’
    • In the first argument of ‘(.)’, namely ‘select’
      In the second argument of ‘(.)’, namely ‘select . venueById’
      In the expression: fmap convert . select . venueById
```

This one suggests to me that `select` is responsible for selecting `SqlBackend`, so we'll make a note to investigate that next.

```
/home/matt/Projects/sellerlabs-hs/texas-toast/src/Texas/Query/Venue.hs:36:26: error:
    • Couldn't match type ‘persistent-typed-db-0.0.1.0:Database.Persist.Typed.SqlFor
                             TexAcctDb’
                     with ‘SqlBackend’
        arising from a use of ‘select’
    • In the second argument of ‘(<$>)’, namely
        ‘select venueWithSettings’
      In the expression: toList . convert <$> select venueWithSettings
      In an equation for ‘getVenuesWithSettings’:
          getVenuesWithSettings
            = toList . convert <$> select venueWithSettings
            where
                convert ::
                  [(Entity Venue, Maybe (Entity VenueSetting))]
                  -> Map (Key Venue) (Entity Venue, Map Text (Maybe Text))
                convert
                  = fmap (fmap (venueSettingsToMap . fmap entityVal))
                    . foldr
                        (\ (evenue, evenueSetting)
                           -> Map.insertWith
                                (\ (ev, es1) (_, es2) -> ...)
                                (entityKey evenue)
                                (evenue, maybeToList evenueSetting))
                        Map.empty
```

This appears to be the same thing: `select` seems to be looking up the record backend and complaining when the type doesn't line up.

```
/home/matt/Projects/sellerlabs-hs/texas-toast/src/Texas/Query/Venue.hs:61:5: error:
    • Overlapping instances for Database.Esqueleto.Internal.Language.FromPreprocess
                                  q e b (e (Entity Venue))
        arising from a use of ‘from’
      Matching instances:
        instance (Esqueleto query expr backend, PersistEntity val,
                  PersistEntityBackend val ~ backend) =>
                 Database.Esqueleto.Internal.Language.FromPreprocess
                   query expr backend (expr (Entity val))
          -- Defined in ‘Database.Esqueleto.Internal.Language’
        instance (Esqueleto query expr backend,
                  Database.Esqueleto.Internal.Language.FromPreprocess
                    query expr backend a,
                  Database.Esqueleto.Internal.Language.FromPreprocess
                    query expr backend b,
                  Database.Esqueleto.Internal.Language.IsJoinKind join) =>
                 Database.Esqueleto.Internal.Language.FromPreprocess
                   query expr backend (join a b)
          -- Defined in ‘Database.Esqueleto.Internal.Language’
      (The choice depends on the instantiation of ‘q, b, e’
       To pick the first instance above, use IncoherentInstances
       when compiling the other instance declarations)
    • In the expression:
        from
        $ \ (v `LeftOuterJoin` vs)
            -> do { on (just (v ^. VenueId) ==. vs ?. VenueSettingVenue);
                    pure (v, vs) }
      In an equation for ‘venueWithSettings’:
          venueWithSettings
            = from
              $ \ (v `LeftOuterJoin` vs)
                  -> do { on (just (v ^. VenueId) ==. vs ?. VenueSettingVenue);
                          .... }
```

This error mentions `Database.Esqueleto.Language.FromPreprocess`, which I'm not familiar with, so I'll need to look at.
It is also complaining about `from`.
The first instance mentioned looks promising:

```haskell
        instance (Esqueleto query expr backend, PersistEntity val,
                  PersistEntityBackend val ~ backend) =>
                 Database.Esqueleto.Internal.Language.FromPreprocess
                   query expr backend (expr (Entity val))
          -- Defined in ‘Database.Esqueleto.Internal.Language’
```

This instance requires that `Esqueleto query expr backend` is satsified, and that `val` is a `PersistEntity` and that the `PersistEntityBackend val ~ backend` for Esqueleto.
So, it can't solve this type class instance unless the record's backend is an instance of Esqueleto.
We know that this forces it to `SqlExpr`, `SqlQuery`, and `SqlBackend` thanks to the functional dependencies (also: fundeps if you're lazy, like me).

K, back to the issues with `select`.
Let's look at it's type:

```haskell
select :: ( SqlSelect a r
          , MonadIO m )
       => SqlQuery a -> SqlReadT m [r]
```

Alright, so `select` is also taking a `SqlQuery`, which forces `expr ~ SqlExpr` and `backend ~ SqlBackend`.
But it doesn't appear to be using the backend type specifically yet.

What is SqlReadT?
Doing a quick [hoogle](http://hoogle.haskell.org) search, I get [these results](http://hoogle.haskell.org/?hoogle=SqlReadT), which points to the [this type signature](https://hackage.haskell.org/package/persistent-2.7.0/docs/Database-Persist-Sql.html#t:SqlReadT):

```haskell
type SqlReadT m a = forall backend. SqlBackendCanRead backend => ReaderT backend m a
```

This type signature is abstracting the backend, and saying that "this query will work for all `backend`s, provided that the `backend` is an instance of `SqlBackendCanRead`."
If we give our `SqlFor a` type an instance of `SqlBackendCanRead` then we'll be set there.

So, `select` doesn't appear to to care about the records.
Let's look at the `from` problem again:

```haskell
        instance (Esqueleto query expr backend, PersistEntity val,
                  PersistEntityBackend val ~ backend) =>
                 Database.Esqueleto.Internal.Language.FromPreprocess
                   query expr backend (expr (Entity val))

```

The instance is saying:

> Given an instance `Esqueleto query expr backend`, and an instance for `PersistEntity` val, *and* requiring that `PersistEntityBackend val` have the same type as `backend`, we can provide an instance for `FromPreprocess`.

We can open `stack ghci texas-toast-models` and ask what the type of `PersistEntityBackend Venue` is:

```
λ> :kind! PersistEntityBackend Venue
PersistEntityBackend Venue :: *
= Database.Persist.Typed.SqlFor TexAcctDb
```

So *this* is exactly the problem.
We need to provide an alternative way for Esqueleto to do this.
Mostly, it just needs to accept that two backends are *compatible*.
So a `SqlFor a` is compatible with `SqlBackend`, even if they're not the same.
If we replace `PersistEntityBackend val ~ backend` with `BackendCompatible val backend` for a suitable definition of `BackendCompatible`, then that should fix the issue.

# Modifying Esqueleto

I prepared [a patch for Esqueleto](https://github.com/bitemyapp/esqueleto/pull/53).
I added a class and some instances:

```haskell
class BackendCompatible sup sub

instance BackendCompatible SqlBackend SqlBackend
instance BackendCompatible SqlBackend SqlReadBackend
instance BackendCompatible SqlBackend SqlWriteBackend
```

then, in the texas-toast-models repository, added an instance:

```haskell
instance BackendCompatible SqlBackend (SqlFor TxMasterDb)
instance BackendCompatible SqlBackend (SqlFor TxAcctDb)
```

I replaced the `PersistEntityBackend val ~ backend` constraints in the library
with `BackendCompatible backend (PersistEntityBackend val)`, which solved the
issues with `FromPreprocess`.

However, we're still getting an error:

```
/home/matt/Projects/sellerlabs-hs/texas-toast/src/Texas/Query/Venue.hs:19:20: error:
    • Couldn't match type ‘persistent-typed-db-0.0.1.0:Database.Persist.Typed.SqlFor
                             TexAcctDb’
                     with ‘SqlBackend’
        arising from a use of ‘select’
    • In the first argument of ‘(.)’, namely ‘select’
      In the second argument of ‘(.)’, namely ‘select . venueById’
      In the expression: fmap convert . select . venueById

/home/matt/Projects/sellerlabs-hs/texas-toast/src/Texas/Query/Venue.hs:36:26: error:
    • Couldn't match type ‘persistent-typed-db-0.0.1.0:Database.Persist.Typed.SqlFor
                             TexAcctDb’
                     with ‘SqlBackend’
        arising from a use of ‘select’
    • In the second argument of ‘(<$>)’, namely
        ‘select venueWithSettings’
      In the expression: toList . convert <$> select venueWithSettings
      In an equation for ‘getVenuesWithSettings’:
          getVenuesWithSettings
            = toList . convert <$> select venueWithSettings
            where
                convert ::
                  [(Entity Venue, Maybe (Entity VenueSetting))]
                  -> Map (Key Venue) (Entity Venue, Map Text (Maybe Text))
                convert
                  = fmap (fmap (venueSettingsToMap . fmap entityVal))
                    . foldr
                        (\ (evenue, evenueSetting)
                           -> Map.insertWith
                                (\ (ev, es1) (_, es2) -> ...)
                                (entityKey evenue)
                                (evenue, maybeToList evenueSetting))
                        Map.empty
```

Same stuff as before: `select` is somehow trying to make the backend a
`SqlBackend` instead of a `SqlFor TexAcctDb`.
So, let's dig into the implementation of `select`:

```haskell
select :: ( SqlSelect a r
          , MonadIO m )
       => SqlQuery a -> SqlReadT m [r]
select query = do
    res <- rawSelectSource SELECT query
    conn <- R.ask
    liftIO $ with res $ flip R.runReaderT conn . runSource

```

This delegates to `rawSelectSource`, which should have an answer.

```haskell
rawSelectSource :: ( SqlSelect a r
                   , MonadIO m1
                   , MonadIO m2 )
                 => Mode
                 -> SqlQuery a
                 -> SqlReadT m1 (Acquire (C.Source m2 r))
rawSelectSource mode query =
      do
        conn <- persistBackend <$> R.ask
        res <- run conn
        return $ (C.$= massage) `fmap` res
```

This calls `persistBackend <$> ask`.
What's `persistBackend`?
[Hoogle gives us](http://hoogle.haskell.org/?hoogle=persistBackend) this [top result](https://hackage.haskell.org/package/persistent-2.7.0/docs/Database-Persist-Class.html#v:persistBackend): it's a method on the `HasPersistBackend` class.
It takes a value of some type that has a `BaseBackend`, and returns the
`BaseBackend` for that type.
This works for `SqlReadBackend` and `SqlWriteBackend` because their backends are
simply `SqlBackend`.

That's no good!
The `persistent` library *really* wants for `BaseBackend backend ~ PersistEntityBackend record`.
If we want type safety, then `BaseBackend backend ~ SqlFor DbName ~ PersistEntityBackend record` for the records we care about.
So, we need to change the method to be something that grabs the `SqlBackend` out
of whatever is passed on in.

# Extending the Class

The class we introduced to Esqueleto provides a natural way to solve this.
Rather than use `persistBackend`, which returns the `BaseBackend`, we can add a new method: `projectBackend`, which returns the large backend that the smaller backend is compatible with.
This should backwards-compatibly fix the issue.

I extended the class with:

```haskell
class BackendCompatible sup sub where
  projectBackend :: sub -> sup

instance BackendCompatible SqlBackend SqlBackend where
  projectBackend = id

instance BackendCompatible SqlBackend SqlReadBackend where
  projectBackend = unSqlReadBackend
```

This allows us to acquire a `SqlBackend` from any compatible backend.

The definition for `select` and friends is also changed:

```haskell
select :: ( SqlSelect a r
          , MonadIO m
          , SqlBackendCanRead backend
          , BackendCompatible SqlBackend backend
          )
       => SqlQuery a -> R.ReaderT backend m [r]
select query = do
    res <- rawSelectSource SELECT query
    conn <- R.ask
    liftIO $ with res $ flip R.runReaderT conn . runSource

rawSelectSource :: ( SqlSelect a r
                   , MonadIO m1
                   , MonadIO m2
                   , SqlBackendCanRead backend
                   , BackendCompatible SqlBackend backend)
                 => Mode
                 -> SqlQuery a
                 -> R.ReaderT backend m1 (Acquire (C.Source m2 r))
rawSelectSource mode query =
      do
        conn <- projectBackend <$> R.ask
        let _ = conn :: SqlBackend
        res <- run conn
        return $ (C.$= massage) `fmap` res
  where
    ...
```

`select` mostly just needed to change the constraints on the `backend` type.
Beforehand, it was using `SqlBackendCanRead backend => ReaderT backend m [r]`.
Now, it's using that, provided that we've also constrained the backend to be
compatible with `SqlBackend`.

`rawSelectSource` has the same constraint differences.
We also need to use `projectBackend` instead of `persistBackend` to convert it
to the backend we want.

# sigh

Unfortunately, we're still running into the same issue.
Here's the error output we get with those chagnes to the library:

```haskell
/home/matt/Projects/sellerlabs-hs/texas-toast/src/Texas/Query/Venue.hs:19:20: error:
    • Couldn't match type ‘persistent-typed-db-0.0.1.0:Database.Persist.Typed.SqlFor
                             TexAcctDb’
                     with ‘SqlBackend’
        arising from a use of ‘select’
    • In the first argument of ‘(.)’, namely ‘select’
      In the second argument of ‘(.)’, namely ‘select . venueById’
      In the expression: fmap convert . select . venueById

/home/matt/Projects/sellerlabs-hs/texas-toast/src/Texas/Query/Venue.hs:36:26: error:
    • Couldn't match type ‘persistent-typed-db-0.0.1.0:Database.Persist.Typed.SqlFor
                             TexAcctDb’
                     with ‘SqlBackend’
        arising from a use of ‘select’
    • In the second argument of ‘(<$>)’, namely
        ‘select venueWithSettings’

```

This is exactly the problem we had before generalizing the constraint.
What gives?!

# type inference RULES

Whenever Haskell types are confusing, it can be helpful to just blow the type
signature away and see what GHC comes up with.

So I deleted the type signatures that were throwing errors, and asked GHC what it thought the types should be.
GHC very helpfully gave me the following:

```
/home/matt/Projects/sellerlabs-hs/texas-toast/src/Texas/Query/Venue.hs:18:1: warning: [-Wmissing-signatures]
    Top-level binding with no type signature:
      getVenueById :: (BaseBackend backend ~ SqlBackend,
                       PersistUniqueRead backend, PersistQueryRead backend,
                       IsPersistBackend backend,
                       Database.Esqueleto.Internal.Language.BackendCompatible
                         SqlBackend backend,
                       MonadIO m) =>
                      VenueId
                      -> ReaderT backend m (Maybe (Entity Venue, Map Text (Maybe Text)))

/home/matt/Projects/sellerlabs-hs/texas-toast/src/Texas/Query/Venue.hs:35:1: warning: [-Wmissing-signatures]
    Top-level binding with no type signature:
      getVenuesWithSettings :: (BaseBackend backend ~ SqlBackend,
                                PersistUniqueRead backend, PersistQueryRead backend,
                                IsPersistBackend backend,
                                Database.Esqueleto.Internal.Language.BackendCompatible
                                  SqlBackend backend,
                                MonadIO m) =>
                               ReaderT backend m [(Entity Venue, Map Text (Maybe Text))]
```

Ah HAH!
The synonym `SqlBackendCanRead` must be carrying around that `BaseBackend backend ~ SqlBackend` constraint.
That's what's borking this.

Indeed, [the Hackage docs for SqlBackendCanRead](https://hackage.haskell.org/package/persistent-2.7.0/docs/Database-Persist-Sql.html#t:SqlBackendCanRead) show that it is a constraint alias:
it aliases all of `IsSqlBackend`, `PersistQueryRead`, `PersistStoreRead`, and `PersistUniqueRead`.

`IsSqlBackend` is also an alias:

```haskell
type IsSqlBackend backend = (IsPersistBackend backend, BaseBackend backend ~ SqlBackend)
```

Well, that's our issue.
So now we need to unwrap all those aliases, toss the `BaseBackend` requirement, and instead use the `BackendCompatible` class.

q
