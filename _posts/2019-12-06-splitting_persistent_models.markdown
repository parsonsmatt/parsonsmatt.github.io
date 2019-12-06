---
title: "Splitting Persistent Models"
date: 2019-12-06
layout: post
categories: programming
---

Reddit user /u/qseep [made a comment on my last blog post](https://www.reddit.com/r/haskell/comments/e2l1yj/keeping_compilation_fast/f91hcwm/?context=3), asking if I had any advice for splitting up `persistent` model definitions:

> A schema made using [persistent](https://hackage.haskell.org/package/persistent) feels like a giant Types module. One change to an entity definition requires a recompile of the entire schema and everything that depends on it. Is there a similar process to break up a persistent schema into pieces?

Yes! There is.
In fact, I've been working on this at work, and it's made a big improvement in our overall compile-times.
I'm going to lay out the strategy and code here to make it all work out.

# Starting Point and Background

[`persistent`](https://hackage.haskell.org/package/persistent) is a database library in Haskell that focuses on rapid productivity and iteration with relatively simple types.
It's heavier than a raw SQL library, but much simpler than something like [`opaleye`](https://hackage.haskell.org/package/opaleye) or [`beam`](https://hackage.haskell.org/package/beam).
It also offers less features and type-safety than those two libraries.
Trade-offs abound!

Usually, `persistent` users will define the models/tables for their database using the `persistent` QuasiQuoter language.
The examples in the [Persistent chapeter in the Yesod book](https://www.yesodweb.com/book/persistent) use the QuasiQuoter directly in line with the Haskell code:

```haskell
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int Maybe
    deriving Show
BlogPost
    title String
    authorId PersonId
    deriving Show
|]
```

The Yesod scaffold, however, loads a file:

```haskell
-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
```

For smaller projects, I'd recommend using the QuasiQuoter - it causes less problems with GHCi (no need to worry about relative file paths).
Once the models file gets big, compilation *will* become slow, and you'll want to split it into many files.
I [investigated this slowness to see what the deal was](https://twitter.com/mattoflambda/status/1158853267499057152), initially suspecting that the Template Haskell code was slowing things down.
What I found was a little surprising: for a 1,200 line `models` file, we were spending [less than a second](https://twitter.com/mattoflambda/status/1158853269432651779) doing TemplateHaskell.
The rest of the module would take several minutes to compile, largely because the generated module was over 390,000 lines of code, and GHC is [superlinear in compiling large modules](https://www.parsonsmatt.org/2019/11/27/keeping_compilation_fast.html).

Another reason to split it up is to avoid GHCi linker issues.
GHCi can exhaust linker ticks (or some other weird finite resource?) when compiling a module, and it will do this when you get more than ~1,000 lines of models (in my experience).

# Split Up Approaches

I am aware of two approaches for splitting up the modules - one uses the `QuasiQuoter`, and the other uses external files for compilation.
We'll start with external files, as it works best with `persistent` migrations and requires the least amount of fallible human error checking.

## Separate Files

I prepared a [GitHub pull request](https://github.com/parsonsmatt/split-persistent/pull/1/files) that demonstrates the changes in this section.
Follow along for exactly what I did:

In the Yesod scaffold, you have a `config/models` file which contains all of the entity definitions.
We're going to rename the file to `config/models_backup`, and we're going to create a folder `config/models/` where we will put the new entity files.
For consistency/convention, we're going to name the files `${EntityName}.persistentmodels`, so we'll end up with this directory structure:

```
config
└── models
    ├── Comment.persistentmodels
    ├── Email.persistentmodels
    └── User.persistentmodels
```

Now, we're going to create a Haskell file for each models file.

```haskell
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model.User where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

share [mkPersist sqlSettings]
    $(persistFileWith lowerCaseSettings "config/models/User.persistentmodels")
```

So far, so good!
The contents of the `User.persistentmodels` file only has the entity definition for the `User` table:

```
-- config/models/User.persistentmodels
User
    ident Text
    password Text Maybe
    UniqueUser ident
    deriving Typeable
```

Next up, we'll do `Email`, which is defined like this:

```
Email
    email Text
    userId UserId Maybe
    verkey Text Maybe
    UniqueEmail email
```

`Email` refers to the `UserId` type, which is defined in `Model.User`.
So we need to add that import to the `Model.Email` module.

```haskell
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model.Email where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

import Model.User

share [mkPersist sqlSettings]
    $(persistFileWith lowerCaseSettings "config/models/Email.persistentmodels")
```

We need to do the same thing for the `Comment` type and module.

Now, we have a bunch of modules that are defining our data entities.
You may want to reexport them all from the top-level `Model` module, or you may choose to have finer grained imports.
Either way has advantages and disadvantages.

### Migrations

Let's get those persistent migrations back.
If you're not using `persistent` migrations, then you can just skip this bit.
We'll define a new module, `Model.Migration`, which will load up *all* the `*.persistentmodels` files and make a migration out of them.

```haskell
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model.Migration where

import System.Directory
import ClassyPrelude.Yesod
import Database.Persist.Quasi

mkMigrate "migrateAll" $(do
    files <- liftIO $ do
        dirContents <- getDirectoryContents "config/models/"
        pure $ map ("config/models/" <>) $ filter (".persistentmodels" `isSuffixOf`) dirContents
    persistManyFileWith lowerCaseSettings files
    )
```

Some tricks here:

1. You can write `do` notation in a TemplateHaskell splice, because `Q` is a monad, and a splice only expects that the result have `Q splice` where `splice` depends on syntactically where it's going. Here, we have `Q Exp` because it's used in an expression context.
2. We do a relatively simple scan - get directory contents for our models, then `filter` to the suffix we care about, and then `map` the full directory path on there.
3. Finally we call `persistManyFileWith`, which takes a list of files and parses it into the `[EntityDef]`.

Now we've got migrations going, and our files are split up.
This speeds up compilation quite a bit.

## QuasiQuotes

If you're not using migrations, this approach has a lot less boilerplate and extra files you have to mess about with.
However, the migration story is a little more complicated.

Basically, you just put your QuasiQuote blocks in separate Haskell modules, and import the types you need for the references to work out.
Easy-peasy!

### Migrations

Okay, so this is where it gets kind of obnoxious.

In each quasiquote block, you have it generate migrations for your types.
Then, you'll make a `Model.Migration` file, which will need to import the migrations from each of your `Model` files and then run them in order.
You need to manually topologically sort the migrations based on references, or it will try to create eg foreign keys to tables you haven't created yet, which borks the migrations.

At least, I *think* that's what you'd need to do - that's about where I got when exploring this method on the work codebase, and I decided against it because it seemed less flexible and useful than the above approach since we use the `persistent` migrations.
