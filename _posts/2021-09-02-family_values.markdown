---
title: "Family Values"
date: 2021-09-02
layout: post
categories: programming
---

I wrote a big thread on the company Slack to compare type families: open vs closed vs associated.
I also ended up discussing data families, as well, since they are a good complement to type families.
I'll probably edit this further and include it in my book, Production Haskell, but here's the Slack transcript for now:

An associated type family **is** an open type family, but with the requirement that
it be defined for any type that's also an instance of the type class. Consider
the `mono-traversable` library. We have:

```haskell
type family Element o :: Type

class MonoFunctor o where
  omap :: (Element o -> Element o) -> o -> o

class MonoFoldable o where
  ofoldMap :: Monoid m => (Element o -> m) -> o -> m

class (MonoFunctor o, MonoFoldable o) => MonoTraversable o where
  otraverse :: (Applicative f) => (Element o -> f (Element o)) -> o -> f o
```

If we wanted to put `Element` as an associated type, like:

```haskell
class MonoFunctor o where
  type Element o :: Type
  omap :: (Element o -> Element o) -> o -> o
```

Then we'd be a bit stuck - we need to require `class (MonoFunctor o) => MonoFoldable o`.
This is a stronger requirement than regular Foldable, and
that's a problem. Or we define a **separate** type family on `MonoFoldable`. But then - what do we use with `MonoTraversable`?

If we're desperate to avoid open type families, then we can work around this by
having a dummy class that only carries the type family.

```haskell
class HasElement o where
   type Element o :: Type

class (HasElement o) => MonoFunctor o where ...
class (HasElement o) => MonoFoldable o where ...
class (MonoFunctor o, MonoFoldable o) => MonoTraversable o where ...
```

But "a dummy class that only has a type family" seems obviously worse to me than "a type family" :shrug:

The question: "Should I use an open type family or a closed type family?" has
an analog to simpler language features: type classes and sum types.

If you want a **closed** set, you use a sum type. If you want an **open** set, you use
a type class. So if you're familiar with the trade-offs there, the trade-offs
with open/closed type familes are easier to evaluate

A closed set means you can be exhaustive - "every case is handled." If you're
pattern matching on a datakind, like `type family Foo (x :: Bool)`, then you
can know that handling `Foo 'True` and `Foo 'False` that you've handled all
cases. You don't have to worry that some user is going to add a case and blow
things up.  (Well, you kinda do, because even closed type families aren't
actually closed, due to stuckness semantics, but, uhhhhh, that's a bit of very
advanced trickery, talk to csongor if you're curious)

An open set is a way of allowing easy extensibility. So you're going to accept
something of kind Type or possibly a polymorphic kind variable to allow people
to define their own types, and their own instances of these types.
For example, if I want to associate a type with a string, I can write:

```haskell
type family TypeString (sym :: Symbol) :: Type

type instance TypeString "Int" = Int
type instance TypeString "Char" = Char
```

And that lets me run programs at the type level, that end users can extend.
Much like you can write a type `class` and end users can extend your
functionality.

But ultimately you need to do **something** at the value level. Which means you
need to take some type information and translate it to the value level. This is
precisely what type classes do - they are morally "a function from a type to a
value." We can write a super basic function, like:

```haskell
typeStringProxy :: Proxy sym -> Proxy (TypeString sm)
typeStringProxy _ = Proxy
```

But this is **still** not useful without further classes. The Default class assigns
a special value to any type, and we could do something like this:

```haskell
typeStringDefault :: forall sm. Default (TypeString sm) => Proxy sm -> TypeString sm
typeStringDefault = def @(TypeString sm)
```

Since associating a type class and an open type family is so common, it's
almost always better to use an associated type unless you know that the type
family is going to be shared across multiple type classes.

"So how do you associate a closed type family with values?"
That's a great question.
We can do the same trick with Proxy functions:

```haskell
type family Closed (a :: Bool) where
  Closed 'True = Int
  Closed 'False = Char

closed :: Proxy b -> Proxy (Closed b)
closed _ = Proxy
```

But, until we know what `b` is, we can't figure out what `Closed b` is. To pattern
match on a type, we need a type class.

```haskell
class RunClosed (b :: Bool) where
  runClosed :: Proxy b -> Closed b

instance RunClosed 'True where
    runClosed _ = 3

instance RunClosed 'False where
    runClosed _ = 'a'
```

I'm going to detour a bit here and mention data families. A data family is **like**
a type family, but instead of allowing you to refer to **any** type, you have to
specify the constructors inline. To take an example from persistent,

```haskell
data family Key a

newtype instance Key User = UserKey { unUserKey :: UUID }
type UserId = Key User

newtype instance Key Organization = OrganizationKey { unOrganizationKey :: UUID }
type OrganizationId = Key Organization
```

An advantage of this is that, since you specify the constructors, you can know
the type of `Key a` by knowing the constructor in use - the **value** specifies the
**type**. `OrganizationKey :: UUID -> Key Organization`.

It looks **a lot** like an "open type family," and in fact is completely analogous.
But we don't call them "open data families," even though that's an appropriate
name for it. It should make you wonder - is there such a thing as a **closed** data
family?

Aaaaaand - the answer is "yes", but we call them GADTs instead.

The nice thing about an "open data family" is that you can learn about types by
inspecting values - by knowing a value (like `OrganizationKey uuid`), I can work
'backwards' and learn that I have an `Key Organization`. But, I can't write a
`case` expression over all `Key a` - it's open! and case only works on closed
things. So this code does not work:

```haskell
whatKey :: Key a -> Maybe UUID
whatKey k = case k of
  UserKey uuid -> Just uuid
  OrganizationKey uuid -> Just uuid
  _ -> Nothing
```

Indeed, we need a type class to allow us to write `get :: Key a -> SqlPersistT m (Maybe a)`.

A `GADT` - as a closed data family - allows us to work from a value to a type, and since it is exhaustive, we can write case expressions on them.

```haskell
data Key a where
    UserKey :: { unUserKey :: UUID } -> Key User
    OrganizationKey :: { unOrganizationKey :: UUID } -> Key Organization
```

If I have this structure, then I can actually write `get` without a type class.

```haskell
get :: Key a -> SqlPersistT IO (Maybe a) 
get k = case k of
  UserKey uuid -> do
    [userName, userAge] <- rawSql "SELECT * FROM users WHERE id = ?" [toPersistValue uuid]
    pure User {..}
  OrganizationKey uuid -> ...
```

A GADT is 'basically' a closed type family that gives you constructor tags for
applying that type family,. If we look at `Closed`, we can inline this:

```
type family ClosedTy (b :: Bool) where
  ClosedTy True = Int
  ClosedTy False = Char

data ClosedData (a :: Type) where
  ClosedData :: Proxy b -> ClosedData (ClosedTy b)

-- inlining:
data Closed (a :: Type) where
  ClosedTrue :: Proxy 'True -> Closed Int
  ClosedFalse :: Proxy 'False -> Closed Char
```

When we case on a `Closed` value, we get:

```
runClosed :: Closed a -> a
runClosed closed = 
  case closed of
    ClosedTrue (Proxy :: Proxy 'True) -> 3
    ClosedFalse (Proxy :: Proxy 'False) -> 'a'
```

I'm going to conclude now, with this distillation:

- Open type family + type class = extensible, open programming, but no exhaustivity.
- Closed type family + GADT + functions = exhaustive handling of types, but not extensible
- An open type family + a GADT isn't much fun.
- A closed type family + a type class isn't much fun
