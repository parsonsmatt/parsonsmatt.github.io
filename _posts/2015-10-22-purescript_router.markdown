---
title: "PureScript Router"
date: 2015-10-22
layout: post
categories: programming
---

Not only has SlamData came up with [purescript-halogen](https://github.com/slamdata/purescript-halogen), they've also got a nice routing library [`purescript-routing`](https://github.com/slamdata/purescript-routing).
While I'll be demonstrating it with the `purescript-halogen` library, it's actually library agnostic and should work with anything.
Let's dive in and learn how to use it!

## Defining Routes

The first step is defining our routes. We're making a website for logging weightlifting sessions, so we're concerned with three things:

1. Getting home. Safety is important and it's a dangerous world out there.
2. Logging sessions. That's literally the point, right?
3. Viewing our own profile. Only our own. Vanity is key to success in lifting weights.

```haskell
data Routes
  = Profile
  | Session
  | Home
```

Now that we've defined the data type, we need to write a matcher.
This is a function that takes the stuff after the `#` in the URL and figures out what item in our `Routes` is the right thing.
For this super basic example, we're just going to have the three pages above, so we'll just parse literals:

```haskell
routing :: Match Routes
routing = Profile <$ lit "" <* lit "profile"
      <|> Session <$ lit "" <* lit "session"
      <|> Home <$ lit ""
```

"What's that `lit ""` business?" Well, the routing library strips out all of the slashes, so if we want to refer to a single slash, we have to use the `lit ""` bit.
This is using the Applicative style parsing. We could rewrite the individual parsers as:

```haskell
routing :: Match Routes
routing = profileParser <|> sessionParser <|> homeParser
  where
    profileParser = do
      lit ""
      lit "profile"
      pure Profile
    sessionParser = do
      lit ""
      lit "session"
      pure Session
    homeParser = do
      lit ""
      pure Home
```

And, when we get into more complicated URLs, we may indeed find ourselves wanting to do that.
For now, the above examples are just fine.


