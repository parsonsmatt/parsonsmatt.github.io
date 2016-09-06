---
title: "What are corecords?"
date: 2016-09-06
layout: post
categories: programming
---

Have you heard the term "corecord" before?
Are you confused about what it could possibly mean?
I was!
But now I think I have it mostly figured out, so I'm going to share some of this wisdom.

# Records

Corecords and records are related.
As is usually the case in Haskell, we can chop the `co` off the word and look at the related concept to get an idea of what we're dealing with.
A record is something like this:

```haskell
data Foo = Foo
    { bar :: String
    , wat :: Int
    }
```

This data declaration defines a new type `Foo` with a single constructor `Foo` that takes two arguments, a `String` and an `Int`.
The field names `bar` and `wat` are syntax sugar for defining field accessors.
The above can be desugared as:

```haskell
data Foo = Foo String Int

bar :: Foo -> String
bar (Foo a _) = a

wat :: Foo -> Int
wat (Foo _ a) = a
```

We also get record update and creation syntax, which lets us write:

```haskell
foo1 = Foo { wat = 2, bar = "lawl" }
foo2 = foo1 { bar = "asdf" }
```

Unfortunately, Haskell doesn't really have records.
It pretends to, with these accessors and update syntax, but we're lacking the real power of first class records.
What *is* a real record?

# Row Types

A record is a dictionary with keys that are incorporated into the type.
PureScript has row types, and they look like this:

```haskell
foobar :: { foo :: String } -> String
foobar object = object.foo
```

We can specify that we take a record with a field named `foo` and a type `String`.
We can then use `foo` as a label to access the value in the record.
PureScript also has row polymorphism which allows you to say:

```haskell
quux :: forall r. { foo :: String | r } -> String
quux rec = r.foo
```

which means:

> Give me a record with a `foo :: String` attribute and any other rows, and I'll give you a string back.

We can pass `{foo: "hey", bar: 2}` to this function no problem.

I think we have a pretty good understanding of records now:

- They're like a tuple or product type, BUT...
- The order of the fields doesn't matter
- The fields have a *label* that we can use to identify them with

# Corecords

When we're considering the dual to records, the main thing is that: a record is a *product*.
It contains a bunch of things, accessible by fields, and there's no notion of ordering.

The corecord to a record, then, is a *sum* type.
It is the *reification* of the fields!
So for each field in the record, we must create a constructor in the sum.
For the `Foo` record above, we'll have the following corecord:

```haskell
data Foo = Foo
    { bar :: String
    , wat :: Int
    }

data CoFoo
    = Bar
    | Wat
```

Why do we need this sum type representation?
What's wrong with the generated `bar` and `wat` functions that Haskell makes for us?
Well, their type is heterogenous: `bar :: Foo -> String` and `wat :: Foo -> Int`, so we can't store them generically as *accessors* to Foo.
But we can totally have a `[CoFoo]`.

It gets kinda tricky to talk about using a corecord as an accessor into a record.
You have to have some type level tricks to know the type of the thing you're accessing, which I don't want to cover right now.
So instead, I'm going to shift gears and talk about a practical example of how I used corecords to solve a real business problem, and the library that I'm releasing to help with the process!

# Email Templating

At work, we've got an email templating system that allows people to use variables for different things.
In order to render an email, we need to have all the various information for each variable that the user might select.

The first version of the variable replacement system used regular expressions to scan the template and `gsub` the values in where the variables were.
This ended up being really hard to debug and test, and had a number of subtle issues with regular expression parsing.

The second version used a `Map Text Text`.
We'd look up a variable like `"first-name"` in the map, getting us a `Maybe Text`.
The template substitution was done in the `Maybe` monad, indicating that it could possibly fail at runtime.
We also didn't have a single point of truth for where the variables were.
The compiler wasn't helping us write correct code.

I attempted to make it more type-safe by first describing a *record* to contain the variables.
Since the variables are static, we can use the type system to ensure that we don't try to access anything we don't have.
This was a big win: the algorithm for rendering a template became total, not partial, and we got some compiler assistance in ensuring that we constructed the record without any missing fields (thanks to it being a compile-time error to forget a strict field in a record).

However, when parsing the email templates, what do I do with the variable names?
I could parse them into `VariableRec -> Text`, but there's no way to verify that I'm talking about the fields of the record.
That could easily just be `show`!
When I'm testing the parser, I have to ensure that I haven't forgotten any of the variables.

The fourth version used a simple record/corecord system.
First, I started with the *corecord*: the sum of all variables!

```haskell
data Variable
    = FirstName
    | LastName
    | Address
    | StoreLink
    derivign (Eq, Ord, Enum, Bounded)
```

and then defined a record (with `!Bang Patterns` for strict fields):

```haskell
data VariableRec = VariableRec
    { firstName :: !Text
    , lastName  :: !Text
    , address   :: !Text
    , storeLink :: !Text
    }
```

and finally, a function to get the variable out of the record:


```haskell
getVariable :: Variable -> VariableRec -> Text
getVariable FirstName = firstName
getVariable LastName  = lastName
getVariable Address   = address
getVariable StoreLink = storeLink
```

So, what's cool about this? Well, note that we've derived `Enum` and `Bounded` for the corecord type.
Now it's real easy to talk about `[minBound .. maxBound] :: [Variable]` and know that we're talking about *all* of the variables.
We also get compile-time verification that I haven't missed any cases for a sum type.

It turns out, it's much easier to write the 
