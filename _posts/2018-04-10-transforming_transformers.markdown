---
title: "Transforming Transformers"
date: 2018-04-10
layout: post
categories: programming
---

There's a kind fellow named `lunaris` on [the FPChat slack channel](https://fpchat-invite.herokuapp.com/) that shares exceptionally good advice.
Unfortunately, due to the ephemeral nature of Slack, a lot of this advice is lost to history.
I've been pestering him to write up his advice in a blog so that it could be preserved.
He hasn't posted it yet, so I'm going to start posting his rants for him ;)

`lunaris` works with a company called Habito, and they are currently hiring for a wide variety of roles.
If this post appeals to you (and you live in London), then [check out their job openings](https://www.habito.com/careers)!

---

> @lunaris says... (with minor formatting edits)

What I meant by obviating transformer stacks was perhaps specific to my (or what I think is my) use case.
That is, you're building a set of services, `MonadAccounts m` (`createAccount :: Email -> Password -> m Account`), etc.
You can do them as dictionaries or type classes.
If you go down the latter (which I think is worth it because eventually the hassle of passing those dictionaries becomes a mite too great for my liking), you probably want to build the services modularly.
So you whip out some transformers `AccountT`, `ProfileT`, `ApplicationsT`, etc.
And you instantiate a big stack `App = AccountT (ProfileT ..` in your main.
Where it's something like, for each transformer:

```haskell
newtype AccountT m a = AccountT (m a)

instance 
  ( MonadReader r m
  , HasSomeAccountConfiguration r
  ) => MonadAccount (AccountT m) where
        ...
```

Or some such.
And at the bottom of your `App` is `ReaderT GlobalConfig IO` such that `HasSomeAccountConfiguration GlobalConfig` is an instance that tells you where to get the things needed to configure your account service.

This is all fine, except you also have to write the passthrough instances for `MonadReader` for all your services.
And of course any other things you might want to pass through (e.g. `MonadPostgreSQL`, `MonadHTTP` -- "effect"-like things).
We previous "solved" the pass through using something like `monad-classes` in Haskell, which uses a load of type hackery to avoid the squared-instances problem. But it comes with lots of costs and we ended up abandoning it.
There are other games you can play around it.
But what we've ended up pursuing instead is taking the functions you'd normally write:

```haskell
-- api/
class Monad m => MonadAccounts m where
  createAccount :: Email -> Password -> m Account

-- impl/
createAccountImpl
  :: (MonadReader r m, HasAccountConfig r)
  => Email -> Password -> m Account
```

And instead of then also having `impl` define and export `AccountT` with an instance such that `createAccount = createAccountImpl`, just export `createAccountImpl`.
Then in main, do:

```haskell
newtype App a = App (ReaderT GlobalConfig IO a)
instance MonadAccounts App where
  createAccount = createAccountImpl
```

This has a different set of trade-offs. For one, you no longer have a stack of binds to wade through or lift.
Things like `HasAccountConfig` you can automatically instantiate using generics too.
The last tradeoff is that you can't derive these mechanical instances.
Moreover, because you can't derive them, you can't enforce that people will write them correctly.
E.g. if your class has methods M1, M2 and you export M1Impl, M2Impl, nothing stops someone from using M1Impl but ignoring M2Impl, which may violate any laws your class' implementation would otherwise fulfill.

However.

If you have [`deriving via`](https://github.com/Icelandjack/deriving-via) (and sorry, the flood is nearly over).
You _can_ have `impl` define and export:

```haskell
newtype AccountT m a = AccountT (m a)

instance 
  ( MonadReader r m
  , HasAccountConfig r
  ) => MonadAccounts (AccountT m) where
  createAccount = createAccountImpl
```

And not export the method implementations (as before).
Now, in main, you just write:

```haskell
newtype App a = App (ReaderT GlobalConfig IO a)
  deriving MonadAccounts via AccountT
```

Or something similar.
And get the instances you want, without the transformer stack.
Of course, you still want things like `MaybeT` and the like for their use in composing effects, even in MTL-like code blocks.
But assuming this works, that feels to me like how I'd want to do application effects from then on.
Still mulling it over though.

---

Big thanks to `@lunaris` for letting me post this.
