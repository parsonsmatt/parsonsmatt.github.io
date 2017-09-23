---
title: "What does Free buy us?"
date: 2017-09-22
layout: post
categories: programming
---

Let's talk about free monads.

Why are they free?
Do monads ordinarily cost us something?

The category theory intuition for "free" roughly expands to:

> This structure gives you a free X when given a Y

So, when we talk about the typical free monad type:

```haskell
data Free f a
    = Pure a
    | Free (f (Free f a))
```

this really expands to:

> This structure gives you a free _monad_ for a given _functor_

This expansion is witnessed by the instance:

```haskell
instance (Functor f) => Monad (Free f) where
    return = Pure
    Pure a >>= k = k a
    Free m >>= k = Free ((>>= k) <$> m)
```

The instance says: "If your `f` is a `Functor`, then a `Free f` is a `Monad`."
The exact implementation is less important.

# Free Monoids

We say that "List is the free monoid."
What we mean is that:

> This structure gives you a free _monoid_ for a given _type_.

So we can equip any value with list and it becomes a monoid, for free.
This expansion is witnessed by the instance:

```haskell
instance Monoid [a] where
    mempty = []
    mappend xs ys = xs ++ ys
```

Are there other free monoids?
Yes!

We have a free monoid for a given semigroup.
This is the more moral instance of `Monoid` for `Maybe`:

```haskell
instance (Semigroup a) => Monoid (Maybe a) where
    mempty = Nothing
    mappend (Just a) (Just b) = Just (a <> b)
    mappend Nothing (Just b) = Just b
    mappend (Just a) Nothing = Just a
    mappend Nothing Nothing = Nothing

-- where `<>` comes from Data.Semigroup
```

# What's the point?

Great question!
What do these constructs buy us?
What is the alternative to using `Free`'s `Monad` instance for a given `Functor`?

The common motivation for `Free` is writing a data structure that we can build up specialized programs in, and then vary the interpretation.
So let's do this without free.
We've been tasked with writing our billing logic system for our SaaS billing system.
We're going to construct a data type that represents a program where we check a user's balance and either charge them, notify them, or cancel their subscriptions based on various factors.
We absolutely need to get this right, so we're doing this weird heavy weight technique to improve our confidence in it's correctness.

# Sum Commands

First, we need to represent the various things we want to do:

```haskell
data BillingProgram
    = GetUserBalance
    | GetUserLastPaymentDate
    | CancelSubscription
    | ChargeUser
    | SendLateNotice
```

These are the commands we need to do:

1. We need a way to get the user's current account balance.
2. We need a way to get the user's last successful payment date.
3. We need a way to cancel a user's subscription.
4. We need to be able to charge the user.
5. We need to be able to send the user a late notice.

This data type is a sum type that represents the possible commands we can issue to the system.
Now, we can construct "programs" using just this type!
Here is a basic interpreter for this data type:

```haskell
data BillingState
    = BillingState
    { userId :: UserId
    , userBalance :: Double
    , userSubscription :: SubscriptionId
    , lastPaymentDate :: Day
    }

interpret :: BillingProgram -> StateT BillingState IO ()
interpret GetUserBalance = do
    id <- gets userId
    balance <- liftIO $ Stripe.getUserBalance id
    modify (\s -> s { userBalance = balance })
interpret GetUserLastPaymentDate = 
    -- etc...

```

We can have our logic construct a `[BillingProgram]` value, and then use `mapM_ interpret` over that.
However, this is really inflexible.
We're required to store every bit of state in the interpreter, as well as the current user and subscription that we're working on.
Let's delegate some of that work to our command type.

In a sense, this is sort of like the code we might write in a highly stateful, imperative OOP context:

```java
class BillingProgram {
    private UserId userId;
    private double userBalance;
    private SubscriptionId userSubscription;
    private Day lastPaymentDate;

    public BillingProgram(UserId u) { /* etc... */ }

    public void runLogic() {
        getUserBalance();
        if (this.userBalance > 100) {
            chargeUser();     
        } else {
            sendBalanceNotice();     
        }
    }
}
```

The command data type has no way of communicating arguments, and it has no way of communicating a return value.
This is no fun.

# Commands, with info!

We want our commands to contain the information they need in order to be able to do work.
Rather than a simple signal to our interpreter on what action to take, we'll also include the parameters that we wish to act on.

```haskell
data BillingProgram 
    = GetUserBalance UserId
    | GetUserLastPaymentDate UserId
    | CancelSubscription UserId PlanId
    | ChargeUser UserId Double
    | SendLateNotice PlanId Email
```

Now, we've augmented our data type.
Interpreting this has become a lot easier -- we no longer need to carry the user ID in the state, or the last payment date.
These are just things we can interpret and request.

```haskell
interpret :: BillingProgram -> IO ()
interpret (GetUserBalance userId) =
    Stripe.getUserBalance userId
interpret (GetUserLastPaymentDate userId) =
    Stripe.getLastPaymentDate userId
interpret (CancelSubscription userId planId) = do
    subscriptions <- Stripe.getSubscriptionsFor userId
    for_ subscriptions $ \sub -> do
        when (sunPlan sun == planId) $ do
            Stripe.cancelSubscription (subId sub)
-- etc...

```

This implementation is pretty clean.
Just like the previous type, we can have our logic functions create a list of these commands, and we can use `mapM_ interpret` to interpret them meaningfully.

However, we have a problem: the two commands `GetUserBalance` and `GetUserLastPaymentDate` are *queries*.
These queries have a meaningful return value.
And we don't have a way to vary behavior.
The interpret function won't type check: `Stripe.getUserBalance` doesn't return `()`, it returns a `Double` that we want to use!

So, we have two choices:

1. Refactor the data type to *not* have queries.
2. Refactor the data type to be *able to use* queries.

Let's explore #1 first.

# No Queries No Masters

So, we can't have queries in our data type.
That means we need to factor all of the logic that we'd do on those queries into the individual commands.

```haskell
data BillingProgram
    = CancelSubscriptionIfUserPaymentTooOld UserId SubscriptionId
    | IfBalanceGreatEnoughThenChargeUserElseSendNotice UserId SubscriptionId Email
```

Ugh.
Let's write the interpreter:

```haskell
interpret :: BillingProgram -> IO ()
interpret (CancelSubscriptionIfUserPaymentTooOld userId subscriptionId) = do
    date <- Stripe.getLastPaymentDate userId
    now <- getCurrentTime
    when (now `diffTime` date > days 60) $ do
        Stripe.cancelSubscription userId subscriptionId
interpet (IfBalanceGreatEnoughThenChargeUserElseSendNotice userId subscriptionId email) = do
    balance <- Stripe.getUserBalance userId
    subscription <- Stripe.getSubscriptions subscriptionId
    if balance > subPrice subscription
       then Stripe.chargeUser userId (subPrice subscription)
       else Email.sendBalanceNotice email subscription
```

Ugh!
This is horrible.
OK, this approach was a mistake.
Let's try refactoring the data type to be able to use queries.

# The question of next

A query is "something that informs what we might want to do next."
But our command data type doesn't have any concept of "next" or "previous," only "now":
Charge the user money!
Send a billing email!

We'd previously used lists to have a sequence of commands, and we'd execute each of them individually.
Lists are a fine way to express iteration and sequencing, but they don't allow previous commands to affect future commands.
So, if we want to incorporate the idea of "next" into our data type, then we can't use lists.
We have to make it part of the type.

We'll include another field on each command: this will have the `BillingProgram` to execute after the current program.

```haskell
data BillingProgram 
    = GetUserBalance UserId            BillingProgram
    | GetUserLastPaymentDate UserId    BillingProgram
    | CancelSubscription UserId PlanId BillingProgram 
    | ChargeUser UserId Double         BillingProgram
    | SendLateNotice PlanId Email      BillingProgram 
```

Now, these commands all have a way of expressing "Once you're done with this command, here's the next command you'll want to execute."
However, we're still not using the information from the `UserBalance` and `LastPaymentDate` commands.
We can express that as a *function* where the construction of the next `BillingProgram` depends on the value that the interpreter returns.

```haskell
data BillingProgram 
    = GetUserBalance UserId            (Double -> BillingProgram)
    | GetUserLastPaymentDate UserId    (Day -> BillingProgram)
    | CancelSubscription UserId PlanId BillingProgram 
    | ChargeUser UserId Double         BillingProgram
    | SendLateNotice PlanId Email      BillingProgram 
```

That does it!
Now, we can express complex logic in our billing program.
Let's construct our billing program that expresses "If the user has enough balance, then charge them, otherwise send a balance notice."

```haskell
chargeOrEmail :: User -> Subscription -> BillingProgram
chargeOrEmail user sub =
    GetUserBalance (userId user) $ \userBalance ->
        if userBalance >= subPrice sub
           then ChargeUser (userId user) (subPrice sub) ???
           else SendLateNotice (subPlan sub) (userEmail user) ???
```

Errr, this doesn't quite work.
We need something in our command data type to indicate "This program is complete."
Let's add that constructor:

```haskell
data BillingProgram 
    = GetUserBalance UserId            (Double -> BillingProgram)
    | GetUserLastPaymentDate UserId    (Day -> BillingProgram)
    | CancelSubscription UserId PlanId BillingProgram 
    | ChargeUser UserId Double         BillingProgram
    | SendLateNotice PlanId Email      BillingProgram 
    | Done
```

The `Done` constructor is the only constructor that doesn't contain a `BillingProgram`, which means that every `BillingProgram` must end with a `Done` (or loop infinitely).
Alright, we can finish our program now:

```haskell
chargeOrEmail :: User -> Subscription -> BillingProgram
chargeOrEmail user sub =
    GetUserBalance (userId user) $ \userBalance ->
        if userBalance >= subPrice sub
           then ChargeUser (userId user) (subPrice sub) Done
           else SendLateNotice (subPlan sub) (userEmail user) Done
```

# Meaningful return values

But, hmm, what if we want to report on whether or not we could successfully bill the user?
The command data type has no way of "returning" a value.
That's kind of unfortunate.
We can change the `Done` constructor to take a value, but we don't want to constrain the type of the value -- we could potentially return all kinds of things!
That means we need to add a type variable to the command data type:

```haskell
data BillingProgram ret
    = GetUserBalance UserId            (Double -> BillingProgram ret)
    | GetUserLastPaymentDate UserId    (Day -> BillingProgram ret)
    | CancelSubscription UserId PlanId (BillingProgram ret) 
    | ChargeUser UserId Double         (BillingProgram ret)
    | SendLateNotice PlanId Email      (BillingProgram ret)
    | Done ret
```

Alright, now we can rewrite our program to return whether or not we successfully billed the customer:

```haskell
chargeOrEmail :: User -> Subscription -> BillingProgram Bool
chargeOrEmail user sub =
    GetUserBalance (userId user) $ \userBalance ->
        if userBalance >= subPrice sub
           then ChargeUser (userId user) (subPrice sub) (Done True)
           else SendLateNotice (subPlan sub) (userEmail user) (Done False)
```

Very cool. We're starting to have a reasonably fully featured language for billing our customers.
However, we don't have any tools for taking an existing program and extending it, or composing it with another one.

# Extending programs

So, we want to extend a preexisting program.
What does it mean to extend a program?
To me, that suggests that we'll start running a new program with the output of the old program.
In order to get the output of the old program, we need to run it until we get to a `Done` constructor.
Then, we can use the value from `Done` to continue the program.
We'll start with the `Done` case:

```haskell
andThen 
    :: BillingProgram a 
    -> (a -> BillingProgram b) 
    -> BillingProgram b
andThen (Done ret) mkProgram = mkProgram ret
```

We take the return value from the previous program, and use it to construct the next bit of the program.
Now, we just need to plumb this through the other constructors:

```haskell
andThen (GetUserBalance userId next) mkProgram =
    GetUserBalance userId (\balance -> andThen (next balance) mkProgram)
andThen (GetUserLastPaymentDate userId next) mkProgram =
    GetUserLastPaymentDate userId (\date -> andThen (next date) mkProgram)
andThen (CancelSubscription userId planId next) = 
    CancelSubscription userId planId (andThen next mkProgram)
andThen (ChargeUser userId amount next) =
    ChargeUser userId amount (andThen next mkProgram)
andThen (SendLateNotice planId email next) =
    SendLateNotice planId notice (andThen next mkProgram)
```

We want to *not* change the existing command structure.
The only thing we do here is use `andThen` to recursively walk the program until we hit `Done`, at which point we extend the program with the new program using the output of the old program.

# huh

## that looks familiar

Let's write a non-trivial program that uses these commands:

```haskell
billingProgram :: User -> [Subscription] -> BillingProgram ()
billingProgram _ [] = 
    Done ()
billingProgram user (sub:subs) =
    GetUserBalance uid $ \balance ->
        if balance > price then 
            ChargeUser uid price theRest
        else 
            SendLateNotice plan (userEmail user)
                $ GetUserLastPaymentDate uid
                $ \day -> if day < 60daysago 
                    then CancelSubscription uid plan theRest
                    else theRest
  where
    uid = userId user
    price = subPrice sub
    plan = subPlan sub
    theRest = billingProgram user subs
```

This is super clumsy and ugly to write.
We have to manually iterate over the list, and we have these weird uppercase constructors everywhere.
We need to manually handle lambda scopes and other such nonsense.

Let's factor out some of the common patterns here, and use the `andThen` function we wrote earlier to build programs rather than manually grafting this stuff together.

```haskell
getUserBalance :: UserId -> BillingProgram Double
getUserBalance userId =
    GetUserBalance userId (\amount -> Done amount)

end :: BillingProgram ()
end = Done ()

getLastPaymentDate :: UserId -> BillingProgram Day
getLastPaymentDate userId =
    GetUserLastPaymentDate userId (\day -> Done day)

cancelSubscription :: UserId -> PlanId -> BillingProgram ()
cancelSubscription userId planId =
    CancelSubscription userId planId end

-- etc, this gets pretty repetitive
```

Alright, let's use these and the `andThen` to write the above logic out:

```haskell
billingProgram :: User -> [Subscription] -> BillingProgram ()
billingProgram _ [] = 
    end
billingProgram user (sub:subs) =
    getUserBalance uid `andThen` \balance ->
    if balance > price then 
        chargeUser uid price 
            `andThen` \_ -> theRest
    else 
        sendLateNotice plan (userEmail user) `andThen` \_ ->
        getUserLastPaymentDate uid `andThen` \day ->
        if day < 60daysago 
           then cancelSubscription uid plan 
                    `andThen` \_ -> theRest
           else theRest
  where
    uid = userId user
    price = subPrice sub
    plan = subPlan sub
    theRest = billingProgram user subs
```

This looks quite a bit nicer!

# ah yes, i've seen this before

This is strongly reminding me of `Monad` at this point.
Let's write an instance of `Monad` for our type:

```haskell
instance Monad BillingProgram where
    return = Done
    (>>=) = andThen
```

Huh. That was easy.

Now we can take advantage of `do` notation and all the functions that are generic over the monad.
I'm specifically thinking of these friends:

```haskell
forM_ :: (Monad m) => (a -> m b) -> [a] -> m [b]

when :: (Monad m) => Bool -> m () -> m ()
```

Let's rewrite our program with this new fanciness:

```haskell
billingProgram :: User -> [Subscription] -> BillingProgram ()
billingProgram user subs = forM_ subs $ \sub -> do
    let uid = userId user
        price = subPrice sub
        plan = subPlan sub
    balance <- getUserBalance uid
    if balance > price
        then do
            chargeUser uid price
        else do
            day <- getUserLastPaymentDate uid
            when (day < 60daysago) $ do
                cancelSubscription uid plan
```

Now *this* is some nice, readable, and idiomatic code.
We've used the `Monad` instance to get sweet, sweet `do` notation.
We haven't tried to interpret it, yet -- is this going to suck?

# interpreting the monad

It's fairly straightforward.
Let's do a Stripe interpreter:

```haskell
interpret :: BillingProgram a -> IO a
interpret (Done a) = pure a
interpret (ChargeUser uid price next) = do
    Stripe.chargeUser uid price
    interpret next
interpret (SendLateNotice plan email next) = do
    Email.sendLateNoticeFor plan email
    interpret next
interpret (GetUserBalance uid next) = do
    balance <- Stripe.getBalance uid
    interpret (next balance)
interpret etcccc = do
    putStrLn "you could finish me"
```

This interpreter just walks down the command tree.
It interprets the command, and then calls the interpreter on the next command recursively.
Where the next command is a function, we first aquire the value, pass it to the function to generate the next command, and the interpret the result.

Can we write a test interpreter?
Yes!

```haskell
interpretTest :: BillingProgram a -> State Mock a
interpretTest (Done a) = pure a
interpretTest (ChargeUser uid price next) = do
    modify (subtractBalance uid price)
    interpret next
interpretTest (SendLateNotice plan email next) = do
    modify (addBillingEmail plan email)
    interpret next
interpretTest (GetUserBalance uid next) = do
    balance <- gets (userBalance uid)
    intepret (next balance)
interpretTest etc = error "finish meeee"
```

This one doesn't use any IO.
It operates in the `State` monad, so we can keep it entirely pure.
We can provide an initial `Mock` state and then make assertions on what the `Mock` looks like after we run a program.
This lets us write tests without needing to mock out any IO or anything else nasty.

# what have we done?!

You might be satisfied to stop here.
We've accomplished a lot, after all!

You might think: 

> Dang, that was a lot of boilerplate.
> There was a lot of repetition in the definition of `andThen`, and the definition of the interpreter seemed awfully repetitive as well.
> What if I write another EDSL (embedded domain specific language)? Will I have to write all this boilerplate again?

Let's go deeper.

Let's write another data type for an EDSL.
This one describes a terminal interaction:

```haskell
data Terminal a
    = GetLine (String -> Terminal a)
    | Done a
    | PrintLine String (Terminal a)

instance Monad Terminal where
    return = Done
    t >>= mk = 
        case t of
            GetLine next ->
                GetLine $ \s -> next s >>= mk
            PrintLine str next ->
                PrintLine str (next >>= mk)
            Done a ->
                mk a

interpret :: Terminal a -> IO a
interpret (Done a) = pure a
interpret (GetLine next) = do
    str <- getLine
    interpret (next str)
interpret (PrintLine str next) = do
    putStrLn str
    interpret next
```

There's definitely a fair amount of boilerplate here.
The structure is very similar.
Let's look at these two types and see what we can factor out:

```haskell
data Terminal a
    = GetLine (String -> Terminal a)
    | PrintLine String (Terminal a)
    | Done a

data BillingProgram ret
    = GetUserBalance UserId            (Double -> BillingProgram ret)
    | GetUserLastPaymentDate UserId    (Day -> BillingProgram ret)
    | CancelSubscription UserId PlanId (BillingProgram ret) 
    | ChargeUser UserId Double         (BillingProgram ret)
    | SendLateNotice PlanId Email      (BillingProgram ret)
    | Done ret
```

Both of these types have a `Done` constructor, so we should be able to factor that out.
Both of these types are also recursive, so we should be able to factor the recursion out.

That means our type should have two components: 

1. Factored out recursion (aka, `Fix`)
2. A `Done` constructor.

```haskell
data Free f a
    = Free (f (Free f a))
    | Done a
```

This actually looks really similar to a list, except the recursion has an intermediate step, the `Free` doesn't take a value, and the `Done` constructor takes a value.
Let's lay them side by side:

```haskell
data Free f a 
    = Free   (f (Free f a))
    | Done a

data List   a
    = Cons a (List a)
    | Nil
```

In fact, `Free` is more general than list!
We can recover singly linked lists by providing an appropriate `f` and `a`, specifically, `(,) n` and `()`:

```haskell
type List a = Free ((,) a) ()

totallyAList :: List Int
totallyAList = Free (1, Free (2, Free (3, Done ())))
```

Anyway, back to stuff people actually care about.

Now that we've factored out the common stuff between our two program types, let's get some common machinery between them:

```haskell
data TerminalF next
    = GetLine (String -> next)
    | PrintLine String next

type Terminal = Free TerminalF

getLine :: Terminal String
getLine = Free (GetLine (\str -> Done str))

printLine :: String -> Terminal ()
printLine str = Free (PrintLine str (Done ()))
```

The new command data type only has the commands we care about.
We replace the explicit recursion with a `next` type variable, which the `Free` type fills in.

```haskell
data BillingF next
    = GetUserBalance UserId (Double -> next)
    | ChargeUser UserId Double next
    | etc you get it

type Billing = Free BillingF

getUserBalance :: UserId -> Billing Double
getUserBalance userId = Free (GetUserBalance userId Done)

chargeUser :: UserId -> Double -> Billing ()
chargeUser uid amt = Free (ChargeUser uid amt (Done ()))
```

Same -- the new command data type doesn't have to worry about `Done`, or anything else.
Because `Free` has an instance of `Monad` for any `Functor`, we only have to write a `Functor` instance to make this work.

I lied.

We don't even have to write that instance.

We just have to ask for it!

```haskell
{-# LANGUAGE DeriveFunctor #-}

data TerminalF next
    = GetLine (String -> next)
    | PrintLine String next
    deriving Functor
```

Haskell lets us derive `Functor` for types where it can figure it out.

So, the free monad instance makes it easy to write programs, but does it make interpreters easy?
Yes!

We can define this function:

```haskell
foldFree 
    :: (Functor f, Monad m) 
    => (forall a. f a -> m a) 
    -> Free f a 
    -> m a
foldFree morph (Done a) = return a
foldFree morph (Free f) = do
    a <- morph f
    foldFree morph a
```

This reads as:

> Give me a way to interpret your commands into some monad.
> Then give me a program built of these commands.
> I'll interpret all of the commmands for you.

So we can write our terminal program as:

```haskell
data TerminalF next
    = GetLine (String -> next)
    | PrintLine String next
    deriving Functor

type Terminal = Free TerminalF

interpret :: Terminal a -> IO a
interpret = foldFree morph
  where
    morph :: TermF a -> IO a
    morph (GetLine next) =
        next <$> getLine
    morph (PrintLine s n) = do
        putStrLn s 
        pure n
```

Note the really interesting bit of this interpreter -- we don't have to specify any recursion, at all.
`foldFree` handles all of that for us.
We just need to specify the bits that should happen at each step of the recursion.

# Wrap it up

We've implemented a data type to represent a primitive set of commands.
We've then extended those commands with arguments, which allowed us to shift complexity from the interpreter into the commands themselves.
Then, we factored the question of "what to do next" from the list data structure into the command data type.
This increased the complexity of both the data type and the interpreter.
However, we were able to get a `Monad` instance for our programs, which gave us a lot of awesome flexibility for writing the EDSLs.

To tame that complexity, we factored the "what to do next" back out into a new data type, this time called `Free` instead of `List`.
`Free` and `List` are similar; and we can use `Free` to write `List` and other interesting data structures.
The only requirement that `Free` has to give a monad to the whole type is that the `f` type parameter be a `Functor`.

I did a similar dive into recursive types in [Recursion Excursion](http://www.parsonsmatt.org/2015/09/24/recursion.html), which you may find interesting.
