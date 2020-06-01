---
title: "Quick Memory Trick"
date: 2020-06-01
layout: post
categories: programming
---

So, Haskell has an amazing potential for writing correct code, but sometimes it doesn't leave an obvious way to remember things.
With record or product types, we don't usually write direct pattern matches to access fields.
You're much more likely to see field labels as accessor functions, or an extension like `RecordWildCards` or `NamedFieldPuns`, or occasionally a field match.

```haskell
data User = User
    { userName :: String
    , userAge :: Int
    , userAdmin :: Bool
    }

-- a direct pattern match, rare:
showUser :: User -> String
showUser (User name age _admin) =
    name ++ "(age: " ++ show age ++ ")"

-- function accessor:
showUser :: User -> String
showUser user =
    userName user ++ "(age: " ++ show (userAge user) ++ ")"

-- field pattern match:
showUser :: User -> String
showUser User { userName = name, userAge = age } =
    name ++ "(age: " ++ show age ++ ")"

-- RecordWildCards
showUser :: User -> String
showUser User {..} =
    userName ++ "(age: " ++ show userAge ++ ")"

-- NamedFieldPuns
showUser :: User -> String
showUser User { userName, userAge } =
    userName ++ "(age: " ++ show userAge ++ ")"
```

When you write with the direct pattern match, you have to match every single field.
In `showUser`, we had to match on the `_admin` field, even though we weren't using it.
If you ever add or remove a field, you have to modify the pattern match, *even if the modified fields are irrelevant to the function*.
This causes noisy diffs and busywork that doesn't *get things done*.

Accessor functions are super flexible - if we decide we want to refactor the `User` type to instead contain a first and last name, the field `userName` can be converted into a function that concatenates the two fields.
This is like using methods instead of field access in Object Oriented languages.

But often times the "field pattern matches" are more convenient, and they're just as resilient to modifications of irrelevant fields.
If a record type has fields that are irrelevant to the function, then this is a safe and reasonable choice.

Sometimes, they're so convenient that you want to use RecordWildCards to pattern match all the fields out, *and* consume all of them.
Is there a way to get the convenience of `RecordWildCards` *and* the safety of knowing that modifying the type will cause a compile error?

# Yes!

I call it the "Undefined Pattern Match" trick.
This is a good way to check that all fields are accounted for in a codebase, even if the record isn't being pattern matched.
I discovered the trick because we have a number of places in the work codebase where adding or removing a field must be accounted for in a way that isn't tracked in the types, or by pattern matching on a relevant value.

In the functions you're defining that requires attention when a constructor changes, you write the following:

```haskell
userFields :: [UserField]
userFields = 
    [ ( "name", SomeField userName )
    , ( "age", SomeField userAge )
    , ( "admin", SomeField userAdmin )
    ]
  where
    User _ _ _ =
        undefined
```

Now, if I go to add a field to `User` (or remove one), then this function will cause a compile-error.
I am reminded that I need to update this definition.

You can also user `error` to attach a note:

```haskell
userFields :: [UserField]
userFields = 
    [ ( "name", SomeField userName )
    , ( "age", SomeField userAge )
    , ( "admin", SomeField userAdmin )
    ]
  where
    User _ _ _ =
        error "Don't forget to update the fields"
```

Actually, since `undefined` can be used at any type, you can leave a note with it:

```haskell
userFields :: [UserField]
userFields = 
    [ ( "name", SomeField userName )
    , ( "age", SomeField userAge )
    , ( "admin", SomeField userAdmin )
    ]
  where
    User _ _ _ =
        undefined "Don't forget to update fields!"
```

If you use this pattern in your codebase a lot, you may want to make a helper term, so you can attach documentation.

```haskell
-- | We use this function on the right-hand side of a pattern match 
-- so we can remind ourselves to modify functions that rely on the
-- fields of a record.
undefinedPatternMatch :: String -> a
undefinedPatternMatch _ = undefined
```
