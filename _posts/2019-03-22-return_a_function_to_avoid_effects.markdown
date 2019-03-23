---
title: "Return a Function to Avoid Effects"
date: 2019-03-22
layout: post
categories: programming
---

To help write robust, reliable, and easy-to-test software, I always recommend purifying your code of effects.
There are a bunch of tricks and techniques to accomplish this sort of thing, and I'm going to share one of my favorites.

I have implemented a pure data pipeline that imports records from one database and puts them in another database with a slightly different schema.
Rather than implement all of the logic for saving each entity individally, I've created a functin `migrate` that is abstract.
The heart of this pipeline is a set of type classes:

```haskell
class LoadData i where
    load :: IO [i]

class ConvertData i o | i -> o where
    convert :: i -> o

class SaveData o where
    save :: [o] -> IO ()

migrate 
    :: (LoadData i, ConvertData i o, SaveData o)
    => IO ()
migrate = do
    old <- load
    save (map convert old)
```

All of the business logic is in the `ConvertData` class.
The `LoadData` and `SaveData` do typically boring things.

A new requirement came in: we are going to import a new record, and we must generate new UUIDs for it.

```haskell
data Old = Old Int Text

data New = New UUID Int Text
```

These `UUID`s must be randomly generated.
The logical place to generate the `UUID` is in the `ConvertData` part of the pipeline.
However, this would require adding `IO` to the method signature, which would make testing and verifying this code more difficult.

Instead, we are going to create a new type:

```haskell
newtype NeedsUUID = NeedsUUID 
    { giveUUID :: UUID -> New
    }
```

Now, our conversion function instances will look like this:

```haskell
instance LoadData Old where
    load = loadOldData

instance ConvertData Old NeedsUUID where
    convert (Old i t) = 
        NeedsUUID (\uuid -> New uuid i t)
```

We have *abstracted* the UUID generation.
Our `ConvertData` type class remains pure, and we've pushed the implementation detail of UUID generation out.

Now, we implement the `SaveData` type class, which already had `IO`.

```haskell
instance SaveData NeedsUUID where
    save needsUUIDs = do
        values <- 
            forM needsUUIDs $ \needsUUID ->
                uuid <- freshUUID
                return (giveUUID needsUUID uuid)
        saveNewValues values
```

# Effects on the Edge

We want to keep effects isolated to the edges of our programs as much as possible.
This allows most of our code to remain pure and easy to test and examine.
I've written about similar topics in my posts [Type Safety Back and Forth]({% post_url 2017-10-11-type_safety_back_and_forth %}) and [Invert Your Mocks!]({% post_url 2017-07-27-inverted_mocking %}).
