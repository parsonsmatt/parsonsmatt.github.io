---
title: "Unpack your Existentials"
date: 2020-10-13
layout: post
categories: programming
---

I recently wrote a library [`prairie`](https://www.stackage.org/nightly-2020-10-13/package/prairie-0.0.1.0) to have "First Class Record Fields."
The overall gist is that I wanted a pair of functions:

```haskell
diffRecord 
    :: Record rec 
    => rec 
    -> rec 
    -> [Update rec]

updateRecord
    :: Record rec
    => [Update rec]
    -> rec
    -> rec
```

I want to be able to send these `[Update rec]` over the wire, so they needed to be serializable.
The design choice I ended up with borrowed the `EntityField` idea from `persistent`.

```haskell
class Record rec where
    data Field rec a
  
    recordFieldLens :: Field rec a -> Lens' rec a
```

`Field` is an associated data family.
Instances will be a `GADT` where the second type parameter indicates the type of the field.
The [documentation for `Prairie.Class`](https://www.stackage.org/haddock/nightly-2020-10-13/prairie-0.0.1.0/Prairie-Class.html) has a good explainer on what's going on.
That's where I ended up, but the story on how I got there is interesting too.

## Packing GADTs

The definition for `Update` is best represented as a `GADT`:

```haskell
data Update rec where
    SetField :: Field rec a -> a -> Update rec

updateRecord :: Record rec => [Update rec] -> rec -> rec
updateRecord upds rec = foldr updateField rec upds
  where
    updateField (SetField field newVal) = 
        set (recordFieldLens field) newVal
```

The type `a` is existential - we've hidden it from view.
Matching on the `Field` type will expose it to use within the scope of the pattern match.
So let's write a `ToJSON` instance for an `Update`.

```haskell
instance ToJSON (Update rec) where
    toJSON (Update field newVal) =
        object
            [ "field" .= field
            , "value" .= newVal
            ]
```

This fails because we need a `ToJSON (Field rec a)`, and also because we need `ToJSON a`.
We can use a `QuantifiedConstraints` to get the first:

```haskell
instance (forall a. ToJSON (Field rec a)) => ToJSON (Update rec) where
    toJSON (Update field newVal) =
        object
            [ "field" .= field
            , "value" .= newVal
            ]
```

But - how do we get the `ToJSON a` instance?

GADTs allow us to pack up existential types.
More powerfully, they allow us to pack up *constraints*.
So let's just paste it in:

```haskell
data Update rec where
    SetField :: ToJSON a => Field rec a -> a -> Update a
```

Our code now compiles.
Hooray!

Now, let's write `FromJSON`.

```haskell
instance FromJSON (Update rec) where
    parseJSON = withObject "Update" $ \o -> do
        field <- o .: "field"
        newVal <- o .: "value"
        pure $ Update field newVal
```

GHC complains about this.
We can use a `QuantifiedConstraint` again to get the `Field` parsing.
But how are we going to know what type the `value` needs to be?
We need to bring the type into scope.

```haskell
instance (forall a. FromJSON (Field rec a)) => FromJSON (Update rec) where
    parseJSON = withObject "Update" $ \o -> do
        o .: "field" >>= \(field :: Field rec a) -> do
            newVal <- o .: "value" :: Parser a
            pure $ Update field newVal
```

GHC still isn't happy.
We don't have an instance of `FromJSON a` in scope.
We also don't have an instance of `ToJSON a` in scope, either!
GHC is complaining about `ToJSON` because the `Update` constructor requires the `ToJSON a` constraint to work.

This appears to be a bad road.
Writing an instance of `Show (Update rec)` is going to require that we pack another constraint `Show a` in the GADT.
And `Eq (Update rec)` will require even more constraints.

```haskell
instance (forall a. Eq (Field rec a)) => Eq (Update rec) where
    SetField field0 val0 == SetField field1 val1 =
        field0 == field1 
        && val0 == val1
```

This code doesn't work because we have no idea what the types are!
But we can *make* it work by packing some more constraints in the GADT.

```haskell
data Update rec where
    SetField 
        :: (Eq a, Typeable a, ToJSON a, FromJSON a, Show a)
        => Field rec a
        -> a
        -> Update rec
```

Now, we can write that `Eq` instance.
Because we're comparing things with potentially different types, we need to compare their types first using the `Typeable` interface.

```haskell
instance (forall a. Eq (Field rec a)) => Eq (Update rec) where
    sf0 == sf1 =
    case sf0 of
        SetField (f0 :: Field rec a) (v0 :: a) ->
            case sf1 of
                SetField (f1 :: Field rec b) (v1 :: b) ->
                    case eqT @a @b of
                        Just Refl ->
                            f0 == f1 && v0 == v1
                        Nothing ->
                            False
```

Well.
This *works*, but dang is it ugly.
Look at the constructor to `Update` - it has so many constraints!
And, what's worse, we're going to need a new constraint packed in there for every single class we want to write an instance.
Gross!

Fortunately, there's a better way.

## `FieldDict`

We want the ability to say:

> I have a `Field rec a`, and I want to have an instance of `ToJSON a` in scope.

Or, if we generalize, 

> I have a `Field rec a`, and I want to have an instance of `c a` in scope.

That sounds an awful lot like a function:

```haskell
fieldDict 
    :: forall (c :: Type -> Constraint) (rec :: Type) (a :: Type)
     .  Field rec a -> c a
```

Except that doesn't work. We can't have a `Constraint` as the return type of a function.
But we *can* have a `Dict`.
`Dict` is a GADT from the [`constraints`](https://hackage.haskell.org/package/constraints-0.12/docs/Data-Constraint.html) package.

```haskell
data Dict c where
    Dict :: c => Dict c
```

Whenever we unpack a `Dict c`, we get the constraint `c` in scope.
So we might have a `Dict (Eq Int)`. 
And unpacking that will bring `Eq Int` into scope.


```haskell
fieldDict :: Field rec a -> Dict (c a)
```

Now our types and kinds line up.
We might use this like:

```haskell
case fieldDict someField of
  Dict :: Dict (Eq a)-> 
    view (recordFieldLens someField) old
        == view (recordFieldLens someField) new
```

The constraint `Eq a` is in scope there, so we can use `==` with it.

We can write it in a "continuation passing style" to avoid needing to `case` on the GADT every time.

```haskell
withFieldDict :: (c a) => Field rec a -> (c a => r) -> r
withFieldDict f k = case fieldDict f of
    Dict :: Dict (c a) -> k
```

Unfortunately, this isn't doing anything for us.
We need the `c a` dictionary in scope to even call this, which means we're not gleaning any additional information with the `Dict`.
We'll need to replace the `c a` constraint with something that'll allow us to communicate what we need.
We're replacing a `c a :: Constraint`, which suggests we need a type class of our own.
The whole problem is that we don't know how to refer to `a` except through the GADT field, and we keep the `rec` parameter around.

```haskell
class 
    (Record rec)
  =>
    FieldDict 
        (c :: Type -> Constraint) 
        (rec :: Type) 
  where
    fieldDict :: Field rec a -> Dict (c a)

withFieldDict 
    :: forall c rec a r. FieldDict c rec 
    => Field rec a
    -> (c a => r)
    -> r
withFieldDict field cont =
    case fieldDict @c field of
        Dict :: Dict (c a) ->
            cont
```

This compiles.

Defining instances is relatively straightforward.
We write a pattern match on the field type and return `Dict`.

```haskell
instance FieldDict Eq User where
    fieldDict field = case field of
        UserName -> Dict
        UserAge -> Dict
        UserId -> Dict

instance FieldDict Ord User where
    fieldDict field = case field of
        UserName -> Dict
        UserAge -> Dict
        UserId -> Dict

instance FieldDict Show User where
    fieldDict field = case field of
        UserName -> Dict
        UserAge -> Dict
        UserId -> Dict

instance FieldDict ToJSON User where
    fieldDict field = case field of
        UserName -> Dict
        UserAge -> Dict
        UserId -> Dict
```

Uh oh.
This is boring.
Can we do better?

Yes! We can actually be generic over the constraint, provided that it holds for all the types.

```haskell
instance (c String, c Int) => FieldDict c User where
    fieldDict field =
        case field of
            UserName -> Dict :: Dict (c String)
            UserId   -> Dict :: Dict (c Int)
            UserAge  -> Dict :: Dict (c Int)
```

The type signatures are unnecessary - they're just there to show that you are returning different things.

## Back to the JSON

Alright, let's rewrite our `ToJSON` instance using `FieldDict`.

```haskell
data Update rec where
    SetField :: Field rec a -> a -> Update rec

instance
    ( forall a. ToJSON (Field rec a)
    , FieldDict ToJSON rec
    )
  =>
    ToJSON (Update rec)
  where
    toJSON (Update field newVal) = 
        withFieldDict @ToJSON field $
            object
                [ "field" .= field
                , "value" .= newVal
                ]
```

This works.
What about parsing?

```haskell
instance 
    ( forall a. FromJSON (Field rec a)
    , FieldDict FromJSON a
    )
  =>
    FromJSON (Update rec)
  where
    parseJSON = withObject "Update" $ \o -> do
        field <- o .: "field"
        case field of
            (_ :: Field rec a) ->
                withFieldDict @FromJSON field $ do
                    val <- o .: "value"
                    pure (SetField field val)
```

This works too!

If you find yourself packing your existentials to great dismay, consider this approach instead.

### Aside...

I've got a whole chapter dedicated to the design and implementation of the `prairie` record library in my book [Production Haskell](https://leanpub.com/production-haskell).
If you liked what you read here, you'll probably enjoy the rest of the book.
