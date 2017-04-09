---
title: "Maybe? Use a type parameter!"
date: 2017-04-08
layout: post
categories: programming
---

Haskell's a powerful and flexible language for modeling the real world.
By pushing information into the type level, we can make our program safer and easier to refactor.
Where many safety features provide limitations, we also get flexibility from these.

So let's look at a common Real World data set: a microblogging system with Users, Posts, and Organizations!

# The Data Model

```haskell
data User = User
    { userName :: Text
    , userPosts :: [Post]
    , userOrganization :: Maybe Organization
    }

data Organization = Organization
    { organiationName :: Text
    , organizationUsers :: [User]
    }

data Post = Post
    { postTitle :: Text
    , postBody :: Text
    , postComments :: [Post]
    , postAuthor :: User
    }
```

This is a pretty simple data model, and it captures our relationships fairly well.
However, it has some issues -- A `User`'s `Organization` is going to link back to that `User`, which is going to result in a cycle!
If we try to `print` that `User`, then it'll go on forever.
Also, any function which takes a `User` and operates on the `Organization` will have to consider the `Maybe`.
Consider this function that gets a user's comembers in the organization:

# The Pain Points

```haskell
coworkers :: User -> Maybe [User]
coworkers user = case userOrganization user of
    Nothing -> 
        Nothing
    Just organization ->
        Just (organizationUsers organization)
-- or,
coworkers = fmap organizationUsers . userOrganization
```

Having `Maybe` values all over the place is much nicer than implicit `null`, but it's still a pain compared to ordinary values.

When we're loading this information from the database, it's going to be a little awkward, as our database model isn't going to correspond exactly to this.
We'd need to have slightly different data types to represent keys, rather than entities:

```haskell
data DbUser = DbUser
    { dbUserName :: Text
    , dbUserOrganiation :: Maybe OrganizationId
    }

data DbOrganization = DbOrganization
    { dbOrganizationName :: Text
    }

data DbPost = DbPost
    { dbPostTitle :: Text
    , dbPostBody :: Text
    , dbPostAuthor :: UserId
    , dbPostParent :: Maybe PostId
    }

type UserId = Text
type OrganizationId = Text 
type PostId = Text
```

So now, we represent a `DbUser` with a name and an optional organization ID, which we'll use it's name.
An organization just contains it's name -- the relationship to Users is contained by the User model.
Likewise, posts no longer contain a reference to their replies, but instead a reference to the post that they are a reply to.
Users don't have Posts directly, and the Post model refers to the author.

Man, this is getting to be a lot of boiler plate, and there's a lot of duplication.
It *seems* like this can be simplified or made more general.
Maybe we can reach for some Template Haskell, or perhaps we should get some extensible records library and turn on the kitchen sink of language extensions.

# The Template Haskell Solution

Actually,

# Let's not

tis a silly place

Instead, let's inspect some commonalities in our `User` and `DbUser`:

```haskell
data User = User
    { userName :: Text
    , userPosts :: [Post]
    , userOrganization :: Maybe Organization
    }

data DbUser = DbUser
    { dbUserName :: Text
    , dbUserOrganiation :: Maybe OrganizationId
    }
```

So the `name` remains the same, but the shape of the organization changes -- we have a `Maybe` in both cases, but a reference/ID for the database and an entity for the user.
The database also has no concept of the Posts.
Our first step in cleaning this up is in making the organization a type parameter:

```haskell
data User org = User
    { userName :: Text
    , userOrganization :: Maybe org
    }

type UserModel = User Organization
type UserDb = User OrganizationId
```

And now, our data model allows us to use the same type to describe these two use cases! 
So this is a small victory.
We can take it a bit further, though -- why hardcode the `Maybe`ness of that organization?
We've solved some of the boilerplate, but we still have the issue with `coworkers` returning a `Maybe`.

```haskell
coworkers :: User Organization -> Maybe [User OrganizationId]
coworkers = fmap organizationUsers . userOrganization
```

So, let's remove the `Maybe` from our definition, which moves the absence or presence of the organization from the value level to the type level.

```haskell
data User org = User 
    { userName :: Text
    , userOrganization :: org
    }
```

Now, let's look at all of our cool variants!

```haskell
type UserWithOrg 
    = User Organization

type UserInDb      
    = User (Maybe OrganizationId)

type UserWithOrgId 
    = User OrganizationId

type UserWithoutOrganization 
    = User ()
```

We can express some really neat stuff here. 
Our type for `coworkers` is a lot nicer:

```haskell
coworkers :: User Organization -> [User OrganizationId]
coworkers = organizationUsers . userOrganization
```

We're now disallowed from passing a `User` in unless we've already given that user an `Organization`.
We've also gained a nice way of bottoming out our relationship: the `Organization` contains a list of users with organization referneces, instead of actual organizations.
This makes it safe to print the whole thing out.

We can also immediately see whether or not we need to do joins, inner joins, left joins, etc. because the nature of the relationship is specified in the type.
The functions for loading stuff out of the database is like:

```haskell
-- | Load all the users out of the database.
--   This is an ordinary select.
loadUsers :: Database [User (Maybe OrganizationId)]
loadUsers = execute [sql|
    select users.* 
    from users
    |]

-- | Load all the users with organizations out of the database.
--   This does an inner join.
loadUsersWithOrganizations :: Database [User OrganizationId]
loadUsersWithOrganizations = execute [sql|
    select users.* 
    from users 
    inner join organizations 
        on users.organization_id = organizations.id
    |]

-- | Load all the users with their organization
loadUsersAndOrganizations :: Database [User Organization]
loadUsersAndOrganizations = combine <$> execute [sql|
    select users.*, organizations.*
    from users
    inner join organizations 
        on users.organization_id = organizations.id
    |]
  where
    combine user organization = 
        user { userOrganization = organization }
```

You'd also know from the type signature if we did a `left join` instead, since we'd have a `Maybe Organization`.

So, how would I write this model out?

```haskell
data User org posts = User
    { userName :: Text
    , userOrganization :: org
    , userPosts :: posts
    }

data Organization users = Organization
    { organizationName :: Text
    , organizationUsers :: users
    }

data Post user = Post
    { postTitle :: Text
    , postBody :: Text
    , postAuthor :: user
    }
```

I'm not going to contain the Post hierarchy within the post datatype, because that makes that data type responsible for too much.
If I want to represent that, a `Tree (Post user)` does fine.

What's another benefit we get from this?

# TYPE CLASSES

Oh dang! Now that our `User`, `Organization`, and `Post` have type parameters, we can write `Functor`, `Foldable`, `Traversable`, etc instances.
Actually, we don't have to -- we can derive them with the help of our language extension friends:

```haskell
{-# language DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

data User org posts = User
    { userName :: Text
    , userOrganization :: org
    , userPosts :: posts
    } deriving (Functor, Foldable, Traversable)
```

`User` is an instance of `Bifunctor`, `Bifoldable`, and `Bitraversable`, so we can map over both the `org` and the `posts` parameter.
This gives us a lot of good code reuse.

# Make Fields For Fun And Profit

As the final thing to do, we'll use the `Control.Lens` function `makeFields` to make it easy to access these types.

```haskell
makeFields ''User
makeFields ''Organization
makeFields ''Post
```

And now we can write code like `user ^. organization . name` to access a user's organization name, or `user ^.. posts . title`.

# Further Watching

This post is surely inspired from [Stephen Compall's ComposeConf talk](https://www.youtube.com/watch?v=BHjIl81HgfE), which is a great thing to watch.
