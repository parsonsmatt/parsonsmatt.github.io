---
title: "Haskell Proposal: Simplify Deriving"
date: 2020-11-10
layout: post
categories: programming
---

Haskell's type classes and deriving facilities are a killer feature for type safety and extensibility.
Over nearly 30 years they've acquired quite a bit of cruft and language extensions.
With `DerivingVia`, we now have the ability to dramatically simplify the deriving story.

This post outlines a change to the *language* that would hopefully be adopted with the next version of the language standard.
They get less reasonable and more dramatic as the post goes on.

# Add to the Stock Deriving Classes

GHC has a ton of extensions that only serve to unlock additional type classes to the "stock" deriving strategy.
`Derive{Functor,Foldable,Traversable,Generic,Lift,etc}`.
We can remove all of these extensions by folding them into the `stock` deriving strategy.

# Remove `DeriveAnyClass`

`DeriveAnyClass` is a footgun.
It allows you to write any type class in a `deriving` clause.
It pastes in an "empty" instance, relying on `DefaultSignatures` to fill in the values.

```haskell
-- With DeriveAnyClass:
data X = X
  deriving ToJSON

-- Without:
data X = X

instance ToJSON X
```

# Remove `DefaultSignatures`

`DefaultSignatures` is used to give a single default implementation of a type class if the underlying type matches a more restrictive constraint.
This is primarily used to provide `Generic`-based implementations with very little syntax.

```haskell
data X = X deriving Generic

-- with DefaultSignatures:

class ToJSON a where
    toJSON :: a -> Value
    default toJSON :: (Generic a, GToJSON (Rep a)) => a -> Value
    toJSON = gtoJSON 

instance ToJSON X

-- without DefaultSignatures:

class ToJSON a where
    toJSON :: a -> Value

instance ToJSON X where
    toJSON = gtoJSON
```

By privileging a single default, it makes any other possible defaults less useful and less discoverable.

The `DeriveAnyClass` utility is subsumed by `DerivingVia`.

```haskell
newtype Generically a = Generically a

instance (Generic a, GToJSON (Rep a)) => ToJSON (Generically a) where
    toJSON (Generically a) = gtoJSON a

data X = X 
    deriving stock Generic
    deriving ToJSON via Generically X
```

# Remove `GeneralizedNewtypeDeriving`

This extension is subsumed by `DerivingVia`, also.

```haskell
-- with GeneralizedNewtypeDeriving
newtype UserId = UserId Text
    deriving newtype (Show, ToJSON)

-- with DerivingVia
newtype UserId = UserId Text
    deriving (Show, ToJSON) via Text
```

# Remove `DerivingStrategies`

Now that there's only two strategies, we can get rid of `DerivingStrategies`.

```haskell
-- Before
data X = X
    deriving stock (Show, Generic)
    deriving (ToJSON, FromJSON) via Generically X

-- After
data X = X
    deriving (Show, Generic)
    deriving (ToJSON, FromJSON) via Generically X
```

# Allow wildcards in deriving clauses

Currently, you must write the complete type in a `DerivingVia` clause.

```haskell
data X = X 
    deriving Generic
    deriving ToJSON via Generically X

newtype Y = Y Text
    deriving ToJSON via Text
```

This can be cumbersome for a very large type.

```Haskell
newtype App a = App (ExceptT () (StateT () (ReaderT () IO)) a)
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadReader ()
        , MonadError ()
        , MonadState ()
        )
      via
        ExceptT () (StateT () (ReaderT () IO))
```

It's also annoyingly repetitive, and can lead to errors.

```haskell
data Foo = Foo 
    deriving Generic
    deriving ToJSON via Generically Foo

-- copy/paste error
data Bar = Bar
    deriving Generic
    deriving ToJSON via Generically Foo
```

A wildcard can be used to indicate either:

a. The underlying type of a `newtype`, or 
b. The type of the `data` declaration.

```haskell
data Foo = Foo
    deriving Generic
    deriving ToJSON via Generically _

-- no more copy paste error
data Bar = Bar
    deriving Generic
    deriving ToJSON via Generically _

-- mmmm nice and clean
newtype App a = App (ExceptT () (StateT () (ReaderT () IO)) a)
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadReader ()
        , MonadError ()
        , MonadState ()
        )
      via _
```

# Remove attached deriving

There are two ways to derive things: `StandaloneDeriving` and attached deriving.
Attached deriving is redundant, but convenient.
`StandaloneDeriving` is more powerful, but less convenient.
Attached deriving clauses don't work with `GADTs`.

```haskell
-- Before:
data Foo = Foo
    deriving Generic
    deriving (FromJSON, ToJSON) via Generically _
    
newtype App a = App (ExceptT () (StateT () (ReaderT () IO)) a)
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadReader ()
        , MonadError ()
        , MonadState ()
        )
      via _

-- Only standalone:
data Foo = Foo

deriving instance Generic Foo
deriving via Generically _ instance ToJSON Foo
deriving via Generically _ instance FromJSON Foo

newtype App a = App ...

deriving via _ instance Functor App
deriving via _ instance Applicative App
deriving via _ instance Monad App
deriving via _ instance MonadReader () App
deriving via _ instance MonadError () App
deriving via _ instance MonadState () App

-- GADT must use standalone to specify a context
data Some f where
    Some :: Show a => f a -> Some f

deriving instance (forall a. Show a => Show (f a)) => Show (Some f)
```

# Lightweight Standalone Syntax

The problem with the above proposal is that it carries a significant syntax cost.
The keyword `deriving` is repeated for each instance, the keyword `instance` is repeated, the `via _` clause is repeated, and the type name is repeated.
Multiple instances should be derivable with the same context.

```haskell
data Foo = Foo

deriving Foo 
    ( Generic
    , via 
        (Generically _)
        ( ToJSON
        , FromJSON
        )
    )
```

In this block, we define the `ToJSON` and `FromJSON` instances using the same `Generically` viatype.
We can still use `_` to refer to the type, since we know the type we're deriving for: `Foo`.
This recovers the syntax convenience of "attached deriving."

```haskell
newtype App a = App ...

deriving App
    via _
        instance 
            ( Functor
            , Applicative
            , Monad
            , MonadReader ()
            , MonadError ()
            , MonadState ()
            )
```

This also recovers the convenience of attached deriving.
Let's look at the main *point* - GADTs.
Otherwise we could just remove `StandaloneDeriving` (with the nice benefit/tragedy of banning orphan derived instance).

```haskell
data Some f where
    Some :: Show a => f a -> Some f

-- old
deriving 
    instance (forall a. Show a => Show (f a)) => Show (Some f)

-- new
deriving Some
    (forall a. Show a => Show (f a)) => Show (_ f)

-- generally,
deriving SomeGadtType
    (SomeContextOn a b c) =>
        ( Show, Eq, ToJSON, FromJSON
        )
        (_ a b c)
```

The `_` refers to the type name, without any variables applied.
So you need to apply the type variables in the instance head.
That's a bit annoying, but maybe it's fine

# Remove Stock Deriving

GHC provides a `newtype Stock a = Stock a` that hooks in to `DerivingVia` somehow.
Now we're down to one deriving strategy.

```haskell
data X = X

deriving X
    via Stock _
        ( Eq, Show, Generic )
    via Generically _
        ( ToJSON, FromJSON )
```

This "deprivileges" the `Stock` deriving classes.

# Remove Standalone Deriving

OK, so maybe you don't like getting rid of attached deriving.
Let's get rid of standalone deriving instead.
We need `StandaloneDeriving` for two reasons:

1. Orphan derived instances (shame on you)
2. Specifying a context for GADTs and allow application of type variables

```haskell
data Some f where
    Some :: Show a => f a -> Some f
    deriving
        ( (forall a. Show a => Show (f a))
            =>
            Show
        , -- generally,
            (SomeContext f)
            =>
            SomeClass
        )
```

The type variable `f` is in scope from the `data` declaration.

# Terrible Post Over

Alright, post is done.
These ideas are certainly controversial and Bad, but *man* wouldn't it be nice to have a simpler story around deriving and type class instances?
The current story is so complex, and I think we can genuinely simplify Haskell-the-language by trimming some fat here.
