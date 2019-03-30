---
title: "Extending the Persistent QuasiQuoter"
date: 2019-03-30
layout: post
categories: programming
---

Haskell's `persistent` database library is convenient and flexible.
The recommended way to define your database entities is the QuasiQuoter syntax, and a complete module that defines some typical entities looks like this:

```haskell
-- src/Models.hs
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import           Database.Persist.TH  
import           Data.Text            (Text)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

User json
    name    Text
    email   Text
    age     Int
    deriving Show Eq

|]
```

The QuasiQuoter does a *ton* of stuff for you.
In this post, we're going to learn how to make it work for you!

# Sharing is Caring

Let's look at the [`share` function](https://www.stackage.org/haddock/lts-13.14/persistent-template-2.5.4/Database-Persist-TH.html#v:share):

```haskell
share :: [[EntityDef] -> Q [Dec]] -> [EntityDef] -> Q [Dec]
share fs x = fmap mconcat $ mapM ($ x) fs
```

It takes a list of functions `[EntityDef -> Q [Dec]` and then runs all of them over the `[EntityDef]` that is provided, and finally joins all the `[Dec]` together.
So, if we want to make the QQ work for us, we need to write a function with that type and add it to our list.

Let's start with a problem: one of the instances that are generated for the `User` table is `PersistEntity`.
`PersistEntity` has an associated data type, called `EntityField`.
It's a sum type which contains all of the fields for the `User` type, and it's a GADT that tells you what the type of the field is.

If we were to write that part of the `PersistEntity` instance by hand, it would look like this:

```haskell
instance PersistEntity User where
    data EntityField User fieldType where
        UserName  :: EntityField User Text
        UserEmail :: EntityField User Text
        UserAge   :: EntityField User Int
```

There are a lot of functions that use the `EntityField` type when doing querying.

This type has *no* instances defined for it!
And you may want to do something interesting with these field types that hasn't been considered.
Let's say we need to have `Show` instances for our record fields.
We can derive them using the `StandaloneDeriving` language extension, so this works:

```haskell
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# LANGUAGE StandaloneDeriving         #-}

module Models where

import           Database.Persist.TH  
import           Data.Text            (Text)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

User json
    name    Text
    email   Text
    age     Int
    deriving Show Eq

|]

deriving instance Show (EntityField User field)
```

The last line is our `StandaloneDeriving` instance.
This works!
However, it's a bit annoying to write out for every single record in a larger schema.
Let's write a function that will do this for us automatically.

# Template Rascal

Let's first review the type of the function we pass to `share`:

```haskell
[EntityDef] -> Q [Dec]
```

The input type is a list of the entity definitions.
This type ([`EntityDef`](https://www.stackage.org/haddock/lts-13.14/persistent-2.9.1/Database-Persist-Types.html#t:EntityDef)) comes from the `persistent` package, and has a ton of information about the entities.

The type `Q` comes from the [`template-haskell`](https://hackage.haskell.org/package/template-haskell-2.14.0.0/docs/Language-Haskell-TH.html#t:Q) package, as do [`Dec`](https://hackage.haskell.org/package/template-haskell-2.14.0.0/docs/Language-Haskell-TH.html#t:Dec).

This blog post isn't going to elaborate too much on `TemplateHaskell` - if you'd like a beginner-friendly tutorial, see [Template Haskell Is Not Scary]({% post_url 2015-11-15-template_haskell %}).

We will begin by creating the skeleton for the function:

```haskell
deriveShowFields :: [EntityDef] -> Q [Dec]
deriveShowFields entities = 
    undefined
```

We know we're going to iterate over all of them, so let's use `forM` - `Q` has a `Monad` instance.

```haskell
deriveShowFields :: [EntityDef] -> Q [Dec]
deriveShowFields entities = 
    forM entites $ \entity ->
        undefined
```

We need to replace `undefined` with an expression of type `Q Dec`.
We could attempt to construct the `Dec` value directly using data constructors.
However, it will be a bit more straightforward to use a QuasiQuoter.

```haskell
deriveShowFields :: [EntityDef] -> Q [Dec]
deriveShowFields entities = 
    forM entites $ \entity ->
        let name = undefined
        [d|deriving instance Show (EntityField $(name) field)|]
```

This fails with a type error.
The `[d| ... |]` quasiquoter returns a value of type `Q [Dec]`.
That means that `forM entities ...` will return `Q [[Dec]]`.
Sowe just need to flatten it:

```haskell
deriveShowFields :: [EntityDef] -> Q [Dec]
deriveShowFields entities = 
    fmap join . forM entites $ \entity ->
        let name = undefined
        [d|deriving instance Show (EntityField $(name) field)|]
```

Alright, now we need to get a `name` that fits in that splice.
What's the type of that splice?
I'm going to throw a `()` in there and see what GHC complains about.

```haskell
deriveShowFields :: [EntityDef] -> Q [Dec]
deriveShowFields entities = 
    fmap join . forM entites $ \entity ->
        let name = ()
        [d|deriving instance Show (EntityField $(name) field)|]
```

This gives us an error:

```
• Couldn't match type ‘()’ with ‘Q Type’
          Expected type: TypeQ
            Actual type: ()
        • In the expression: name
          In a stmt of a 'do' block:
            [d| deriving instance Show (EntityField $(name) x) |]
            pending(rn) [<splice, name>]
          In the expression:
            do let name = ()
               [d| deriving instance Show (EntityField $(name) x) |]
               pending(rn) [<splice, name>]
        |
    ... |   [d|deriving instance Show (EntityField $(name) x)|]
        |                                            ^^^^
```

Cool! We need something of type `Q Type`. 
`Type`, like `Dec`, comes from the [`template-haskell`](https://hackage.haskell.org/package/template-haskell-2.14.0.0/docs/Language-Haskell-TH.html#t:Type) package.

So, we have an `entity :: EntityDef`, and we need a `name :: Q Type`.
The name is the name of the entity.
If we look at the [fields of `EntityDef`](https://www.stackage.org/haddock/lts-13.14/persistent-2.9.1/Database-Persist-Types.html#t:EntityDef) again, we'll see that the first field is `entityHaskell :: HaskellName`.
That is promising.
We can use another [`PersistEntity`](https://www.stackage.org/haddock/lts-13.14/persistent-2.9.1/Database-Persist-Class.html#t:PersistEntity) class function, `entityDef :: (Monad m) => m rec -> EntityDef`, to summon an `EntityDef` in GHCi and see what we get.

```haskell
>>> entityHaskell $ entityDef (Nothing :: Maybe User)
HaskellName {unHaskellName = "User"}
```

What's inside a `HaskellName`? 
Let's find out!

```haskell
>>> :info HaskellName
newtype HaskellName
  = HaskellName {unHaskellName :: Data.Text.Internal.Text}
  	-- Defined in ‘persistent-2.8.2:Database.Persist.Types.Base’
```

So, we have a `Text` representation of the Haskell record name.
And we know we need a `Type` that refers to this name.
If we look at [the data constructors for `Type`](https://hackage.haskell.org/package/template-haskell-2.14.0.0/docs/Language-Haskell-TH.html#t:Type), we'll notice that `ConT` appears to be what we want.

So now we need a `Name` to give to `ConT`.
What is a [`Name`](https://hackage.haskell.org/package/template-haskell-2.14.0.0/docs/Language-Haskell-TH.html#t:Name)?
The linked docs say that it's an abstract type for the Haskell value names.
They also give us a way of creating one: [`mkName :: String -> Name`](https://hackage.haskell.org/package/template-haskell-2.14.0.0/docs/Language-Haskell-TH.html#v:mkName).

The last building block is `Data.Text.unpack :: Text -> String`.
Now, let's plug our legos together:

```haskell
deriveShowFields :: [EntityDef] -> Q [Dec]
deriveShowFields entities = 
    fmap join . forM entites $ \entity ->
        let name = 
                pure . ConT . mkName . Text.unpack 
                . unHaskellName . entityHaskell 
                $ entity
        [d|deriving instance Show (EntityField $(name) field)|]
```

Bingo! Let's pass this to `share` in our model file.
Note that we need to import it from somewhere else due to Template Haskell staging restrictions.

```haskell
share 
    [ deriveShowFields
    , mkPersist sqlSettings
    , mkMigrate "migrateAll"
    ] [persistLowerCase|

User json
    name    Text
    email   Text
    age     Int
    deriving Show Eq

|]
```

And let's try it in GHCi:

```
>>> show UserEmail
"UserEmail"
>>> show UserName
"UserName"
```

Nice.
We've hooked into `persistent`'s QuasiQuoter and provided our own functionality.
