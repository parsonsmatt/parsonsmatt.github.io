---
title: "Persistent Models are Views"
date: 2024-02-07
layout: post
categories: programming
---

The Haskell `persistent` library provides a `QuasiQuoter` syntax for defining a Haskell datatype, along with code to convert it into a database table.
However, there's a bit of a subtlety here.

[Here is the documentation for the syntax on the `QuasiQuoter`](https://hackage.haskell.org/package/persistent-2.14.6.0/docs/Database-Persist-Quasi.html).
I'll refer to it throughout this blog post.

The conventional use of this library is to define a bunch of tables that represent the *complete* table.

```haskell
mkPersist sqlSettings [persistLowerCase|
    User sql="users"
        name        Text
        birthday    Day

    Organization
        name        Text
        primaryUser UserId
|]
```

However, a *very* natural thing to do is add `created` and `updated` timestamps.

```haskell
mkPersist sqlSettings [persistLowerCase|
    User        sql="users"
        name        Text
        birthday    Day
        createdAt   UTCTime     default=now()
        updatedAt   UTCtime     default=now()

    Organization
        name        Text
        primaryUser UserId
        createdAt   UTCTime     default=now()
        updatedAt   UTCtime     default=now()
|]
```

The intention is that the *database* supplies these values, but the Haskell code requires you provide them.
This means that your `insert`s are annoying.

```haskell
fakeUTCTime :: UTCTime
fakeUTCTime = UTCTime (fromGregorian 1 1 1) 0

foo :: SqlPersistT IO ()
foo = do
    insert User
        { userName = "Matt Parsons"
        , userBirthday = fromGregorian 1988 09 29
        , userCreatedAt = fakeUTCTime
        , userUpdateAt =  fakeUTCTime
        }
```

You have to provide a `fakeUTCTime` value.
The database will immediately throw it away and not use it.
Wouldn't it be better to *not* need to do this?

# Liberate Your Models

As is often the case in Haskell, the problem can be nicely ameliorated by *providing more types*.

Let's consider separating our concerns, and representing a `User` twice: once as a type faithful to the shape of the database table, and another as a *default way to insert it*.

```haskell
mkPersist sqlSettings [persistLowerCase|
    User        sql="users"
        name        Text
        birthday    Day
        createdAt   UTCTime     default=now()
        updatedAt   UTCtime     default=now()

    InsertUser  sql="users"
        name        Text
        birthday    Day
        createdAt   UTCTime     default=now()   MigrationOnly
        updatedAt   UTCtime     default=now()   MigrationOnly

|]

foo :: SqlPersistT IO ()
foo = do
    insert InsertUser
        { insertUserName = "Matt Parsons"
        , insertUserBirthday = fromGregorian 1988 09 29
        }
```

Now, we have a variant of our type which *does not have timestamps*, and we can use this to insert a value into the database.
The database supplies the value we need.

There's two tricks going on here:

1. `sql="users"`
2. `MigrationOnly`

The `sql=` in `persistent` typically means "Use this name in the `sql` representation of this."
For a table, this tells `persistent` that the *table name* for our type is `users`.
And - we have *two Haskell models* that reference `users`!

Then, `MigrationOnly` is a signal to `persistent` that the field should not be present in the generated Haskell code.
So `InsertUser` will not have Haskell code for `createdAt` or `updatedAt`, but `persistent` will still expect the database to have the right shape.

# Decouple Your Models

This has application beyond providing a more convenient interface for inserting default columns.
You can actually *decouple* your tables from each other and have business logic that relies on a *subset* of the database.

For example, let's look at some code that needs to know about `Organization`s, but that does not care at all about `User`s.
The *view* of the `Organization` table that this code needs looks like this:

```haskell
mkPersist sqlSettings [persistLowerCase|
    MyOrganization      sql="organization"  !no-migrate
        name        Text
|]
```

Note that we don't actually reference the `UserId` type, which means we don't need the `User` model in scope.
This allows us to *decouple* this logic from the whole `User` notion, or anything else that `Organization` depends on that is irrelevant to the code that `MyOrganization` is useful for.

This uses another `persistent` feature: `!no-migrate`. 
When we write this, we tell `persistent` not to include this model in our migrations.

As long as the database table indicated by `sql="organization"` is compatible with what we have here for the operations we do on it, we're fine - and if we're just *reading*, then we're totally fine!
Unfortunately, `persistent` does not offer a means of blocking *insert*, so this can do unsafe things.
