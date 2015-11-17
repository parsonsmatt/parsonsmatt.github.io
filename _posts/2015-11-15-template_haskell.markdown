---
title: "Template Haskell Is Not Scary"
date: 2015-11-15
layout: post
categories: programming
---

## A Beginner Tutorial

This tutorial is aimed at people who are beginner-intermediate Haskellers looking to learn the basics of Template Haskell.

I learned about the power and utility of metaprogramming in Ruby.
Ruby metaprogramming is done by constructing source code with string concatenation and having the interpreter run it.
There are also some methods that can be used to define methods, constants, variables, etc.

In my [Squirrell](https://github.com/parsonsmatt/squirrell/) Ruby library designed to make encapsulating SQL queries a bit easier,
I have a few bits of metaprogramming to allow for some conveniences when defining classes.
The idea is that you can define a query class like this:

```ruby
class PermissionExample
  include Squirrell

  requires :user_id
  permits :post_id

  def raw_sql
    <<SQL
SELECT *
FROM users
  INNER JOIN posts ON users.id = posts.user_id
WHERE users.id = #{user_id} #{has_post?}
SQL
  end

  def has_post?
    post_id ? "AND posts.id = #{post_id}" : ""
  end
end
```

and by specifying `requires` with the symbols you want to require, it will define an instance variable and an attribute reader for you, and raise errors if you don't pass the required parameter.
Accomplishing that was pretty easy.
Calling `requires` does some bookkeeping with required parameters and then calls this method with the arguments passed:

```ruby
def define_readers(args)
  args.each do |arg|
    attr_reader arg
  end
end
```

Which you can kinda read like a macro: take the arguments, and call `attr_reader` with each.
The magic happens later, where I overrode the `initialize` method:

```ruby
def initialize(args = {})
  return self if args.empty?

  Squirrell.requires[self.class].each do |k|

    unless args.keys.include? k
      fail MissingParameterError, "Missing required parameter: #{k}"
    end

    instance_variable_set "@#{k}", args.delete(k)
  end

  fail UnusedParameter, "Unspecified parameters: #{args}" if args.any?
end
```

We loop over the arguments provided to `new`, and if any required ones are missing, error.
Otherwise, we set the instance variable associated with the argument, and remove it from the hash.

Another approach involves taking a string, and evaluating it in the context of whatever class you're in:

```ruby
def lolwat(your_method, your_string)
  class_eval "def #{your_method}; puts #{your_string}; end"
end
```

This line of code defines a method with your choice of name and string to print in the context of whatever class is running.

## wait this isn't haskell what am i doing here

Metaprogramming in Ruby is mostly based on a textual approach to code.
You use Ruby to generate a string of Ruby code, and then you have Ruby evaluate the code.

If you're coming from this sort of background (as I was), then Template Haskell will strike you as different and weird.
You'll think "Oh, I know, I'll just use quasi quoters and it'll all work just right."
Nope.
You have to think very differently about metaprogramming in Template Haskell.
You're not going to be putting strings together that happen to make valid code.
This is Haskell, we're going to have some compile time checking!

## Constructing an AST

In Ruby, we built a string, which the Ruby interpreter then parsed, turned into an abstract syntax tree, and interpreted.
In Haskell, we'll skip the string step.
We'll build the abstract syntax tree directly using standard data constructors.
GHC will verify that we're doing everything OK in the construction of the syntax tree, and then it'll print the syntax tree into our source code before compiling the whole thing.
So we get two levels of compile time checking -- that we built a correct template, and that we used the template correctly.

One of the nastiest things about textual metaprogramming is that there's no guarantee that your syntax is right -- and it can be really hard to debug when doing more complicated stuff.
Programming directly into an AST makes it a lot easier to verify the correctness of what we write.
The quasiquoters are a convenience built around AST programming, but I'm of the opinion that you should learn the AST stuff first and then dive into the quoters when you have a good idea of how they work.

Alright, so let's get into our first example.
We've written a function `bigBadMathProblem :: Int -> Double` that takes a lot of time at runtime, and we want to write a lookup table for the most common values.
Since we want to ensure that runtime speed is super fast, and we don't mind waiting on the compiler, we'll do this with Template Haskell.
We'll pass in a list of common numbers, run the function on each to precompute them, and then finally punt to the function if we didn't cache the number.

Since we want to do something like the `makeLenses` function to generate a bunch of declarations for us, we'll first look at the type of that in the `lens` library.
Jumping to [the lens docs](https://hackage.haskell.org/package/lens-4.13/docs/Control-Lens-TH.html), we can see that the type of `makesLenses` is `Name -> DecsQ`.
Jumping to [the Template Haskell docs](https://hackage.haskell.org/package/template-haskell-2.10.0.0/docs/Language-Haskell-TH.html), `DecsQ` is a type synonym for `Q [Dec]`.
`Q` appears to be a monad for Template Haskell, and a [`Dec`](https://hackage.haskell.org/package/template-haskell-2.10.0.0/docs/Language-Haskell-TH.html#t:Dec) is the data type for a declaration.
The constructor for making a function declaration is `FunD`.
We can get started with this!

We'll start by defining our function.
It'll take a list of commonly used values, apply the function to each, and store the result.
Finally, we'll need a clause that passes the value to the math function in the event we don't have it cached.

```haskell
precompute :: [Int] -> DecsQ
precompute xs = do
  -- .......
  return [FunD name clauses]
```

Since `Q` is a monad, and `DecsQ` is a type synonym for it, we know we can start off with `do`.
And we know we're going to be returning a function definition, which, according to the `Dec` documentation, has a field for the name of the function and the list of clauses.
Now it's up to us to generate the name and clauses.
Names are easy, so we'll do that first.

We can get a name from a string using `mkName`.
This converts a string into an unqualified name.
We're going to choose `lookupTable` as the name of our lookup table, so we can just use that directly.

```haskell
precompute xs = do
  let name = mkName "lookupTable"
  -- ...
```

Now, we need to apply each variable in `xs` to the function named `bigBadMathProblem`.
This will go in the `[Clause]` field, so let's look at what makes up a `Clause`.
According to [the documentation](https://hackage.haskell.org/package/template-haskell-2.10.0.0/docs/Language-Haskell-TH.html#t:Clause), a clause is a data constructor with three fields: a list of `Pat` patterns, a `Body`, and a list of `Dec` declarations.
The body corresponds to the actual function definition, the `Pat` patterns correspond to the patterns we're matching input arguments on, and the `Dec` declarations are what we might find in a `where` clause.

Let's identify our patterns first.
We're trying to match on the `Int`s directly. Our desired output is going to look something like:

```haskell
lookupTable 0 = 123.546
lookupTable 12 = 151626.4234
lookupTable 42 = 0.0
-- ...
lookupTable x = bigBadMathProblem x
```

So we need a way to get those `Int`s in our `xs` variable into a `Pat` pattern.
We need some function `Int -> Pat`... Let's check out [the documentation](https://hackage.haskell.org/package/template-haskell-2.10.0.0/docs/Language-Haskell-TH.html#t:Pat) for `Pat` and see how it works.
The very first pattern is `LitP`, which takes an argument of type `Lit`.
A `Lit` is a sum type that has a constructor for the primitive Haskell types.
There's one for `IntegerL`, which we can use.

So, we can get from `Int -> Pat` with the following function:

```haskell
intToPat :: Int -> Pat
intToPat = LitP . IntegerL . toInteger
```

Which we can map over the initial list to get our `[Pat]`!

```haskell
precompute xs = do
  let name = mkName "lookupTable"
      patterns = map intToPat xs
  -- ...
  return [FunD name clauses]
```

Our `lookupTable` function is only going to take a single argument, so we'll want to `map` our integer `Pat`s into `Clause`, going from our `[Pat] -> [Clause]`.
That will get use the `clauses` variable that we need.
From above, a clause is defined like:

```haskell
data Clause = Clause [Pat] Body [Dec]
```

So, our `[Pat]` is simple -- we only have one literal value we're matching on.
`Body` is defined to be either a `GuardedB` which uses pattern guards, or a `NormalB` which doesn't.
We could define our function in terms of a single clause with a `GuardedB` body, but that sounds like more work, so we'll just use a `NormalB` body.
The `NormalB` constructor takes an argument of type `Exp`.
So let's dig in to [the `Exp` documentation!](https://hackage.haskell.org/package/template-haskell-2.10.0.0/docs/Language-Haskell-TH.html#t:Exp)

There's a lot here.
Looking above, we really just want to have a single thing -- a literal!
The precomputed value.
There's a `LitE` constructor which takes a `Lit` type.
The `Lit` type has a constructor for `DoublePrimL` which takes a `Rational`, so we'll have to do a bit of conversion.

```haskell
precomputeInteger :: Int -> Exp
precomputeInteger = LitE . DoublePrimL . toRational . bigBadMathProblem
```

We can get the `Body`s for the `Clause`s by mapping this function over the list of arguments.
The declarations will just be blank, so we're ready to create our `clauses`!

```haskell
precompute xs = do
  let name = mkName "lookupTable"
      patterns = map intToPat xs
      fnBodies = map precomputeInteger xs
      precomputedClauses = 
        zipWith (\body pattern -> Clause [pattern] body []) fnBodies patterns
  -- ......
  return [FunD name clauses]
```

There's one thing left to do here.
We need to create another clause with a variable `x` that we delegate to the function.
Since we're introducing a local variable, we don't need to worry about being hygienic with our naming, so we can use `mkName` again.
We will have to get a bit more complicated with our `Body` expression, since we've got an application to a function going on.

```haskell
precompute xs = do
  let name = mkName "lookupTable"
      patterns = map intToPat xs
      fnBodies = map precomputeInteger xs
      precomputedClauses =
        zipWith (\body pattern -> Clause [pattern] body []) fnBodies patterns
      x' = mkName "x"
      lastClause = [Clause [x'] appBody []]
  -- ...
      clauses = precomputedClauses ++ lastClause
  return [FunD name clauses]
```

Going back to the `Exp` type, we're now looking for something that captures the idea of application.
The `Exp` type has a data constructor `AppE` which takes two expressions and applies the second to the first.
That's precisely what we need!
It also has a data constructor `VarE` which takes a `Name` argument.
That's all we need. Let's do it.

```haskell
precompute xs = do
  let name = mkName "lookupTable"
      patterns = map intToPat xs
      fnBodies = map precomputeInteger xs
      precomputedClauses = 
        zipWith (\p b -> Clause [p] b []) patterns fnBodies
      x' = mkName "x"
      lastClause = [Clause [x'] appBody []]
      appBody = AppE (VarE (mkName "bigBadMathProblem") (VarE x'))
      clauses = precomputedClauses ++ lastClause
  return [FunD name clauses]
```

We did it! We wrangled up some Template Haskell and wrote ourselves a lookup table.
Now, we'll want to splice it into the top level of our program with the `$()` splice syntax:

```haskell
$(precompute [1..1000])
```

As it happens, GHC is smart enough to know that a top level expression with the type `Q [Dec]` can be spliced without the explicit splicing syntax.

Creating Haskell expressions using the data constructors is really easy, if a little verbose.
Let's look at a little more complicated example.

## Boilerplate Be Gone!

We're excited to be using the excellent `users` library with the `persistent` backend for the web application we're working on (source code located [here, if you're curious](https://github.com/parsonsmatt/QuickLift/)).
It handles all kinds of stuff for us, taking care of a bunch of boilerplate and user related code.
It expects, as it's first argument, a value that can be unwrapped and used to run a Persistent query.
It also operates in the `IO` monad.
Right now, our application is setup to use a custom monad `AppM` which is defined like:

```haskell
type AppM = ReaderT Config (EitherT ServantErr IO)
```

So, to actually use the functions in the `users` library, we have to do this bit of fun business:

```haskell
someFunc :: AppM [User]
someFunc = do
  connPool <- asks getPool
  let conn = Persistent (`runSqlPool` connPool)
  users <- liftIO (listUsers conn Nothing)
  return (map snd users)
```

That's going to get annoying quickly, so we start writing functions specific to our monad that we can call instead of doing all that lifting and wrapping.

```haskell
backend :: AppM Persistent
backend = do
  pool <- asks getPool
  return (Persistent (`runSqlPool` pool))

myListUsers :: Maybe (Int64, Int64) -> AppM [(LoginId, QLUser)]
myListUsers m = do
  b <- backend
  liftIO (listUsers b m)

myGetUserById :: LoginId -> AppM (Maybe QLUser)
myGetUserById l = do
  b <- backend
  liftIO (getUserById b l)

myUpdateUser :: LoginId -> (QLUser -> QLUser) -> AppM (Either UpdateUserError ())
myUpdateUser id fn = do
  b <- backend
  liftIO (updateUser b id fn)
```

ahh, totally mechanical code.
just straight up boiler plate.
This is exactly the sort of thing I'd have metaprogrammed in Ruby.
So let's metaprogram it in Haskell!

First, we'll want to simplify the expression.
Let's use `listUsers` as the example.
We'll make it as simple as possible -- no infix operators, no `do` notation, etc.

```haskell
listUsersSimple m = (>>=) backend (\b -> liftIO (listUsers b m))
```

Nice. To make it a little easier on seeing the AST, we can take it one step further.
Let's explicitly show all function application by adding parentheses to make everything as explicit as possible.

```haskell
listUsersExplicit m = ((>>=) backend) (\b -> liftIO ((listUsers b) m))
```

The general formula that we're going for is:

```haskell
derivedFunction arg1 arg2 ... argn =
  ((>>=) backend) (\b -> liftIO ((...(((function b) arg1) arg2)...) argn))
```

We'll start by creating our `deriveReader` function, which will take as it's first argument the `backend` function name.

```haskell
deriveReader :: Name -> DecsQ
deriveReader rd = 
  mapM (decForFunc rd)
    [ 'destroyUserBackend
    , 'housekeepBackend
    , 'getUserIdByName
    , 'getUserById
    , 'listUsers
    , 'countUsers
    , 'createUser
    , 'updateUser
    , 'updateUserDetails
    , 'authUser
    , 'deleteUser
    ]
```

This is our first bit of special syntax.
The `'destroyUserBackend` is a shorthand way of saying `mkName "destroyUserBackend"`
Now, what we need is a function `decForFunc`, which has the signature `Name -> Name -> Q Dec`.

In order to do this, we'll need to get some information about the function we're trying to derive.
Specifically, we need to know how many arguments the source function takes.
There's a whole section in the Template Haskell [documentation about 'Querying the Compiler'](https://hackage.haskell.org/package/template-haskell-2.10.0.0/docs/Language-Haskell-TH.html#g:3) which we can put to good use.

The `reify` function returns a value of type `Info`.
For type class operations, it has the data constructor `ClassOpI` with arguments `Name`, `Type`, `ParentName`, and `Fixity`.
None of these have the arity of the function directly...

I think it's time to do a bit of exploratory coding in the REPL.
We can fire up `GHCi` and start doing some Template Haskell with the following commands:

```haskell
λ: :set -XTemplateHaskell 
λ: import Language.Haskell.TH
```


We can also do the following command, and it'll print out all of the generated code that it makes:

```haskell
λ: :set -ddump-splices
```
Now, let's run `reify` on something simple and see the output!

```haskell
λ: reify 'id

<interactive>:4:1:
    No instance for (Show (Q Info)) arising from a use of ‘print’
    In a stmt of an interactive GHCi command: print it
```

Hmm.. No show instance. Fortunately, there's a workaround that can print out stuff in the `Q` monad:

```haskell
λ: $(stringE . show =<< reify 'id)
"VarI 
  GHC.Base.id 
  (ForallT 
    [KindedTV a_1627463132 StarT] 
    [] 
    (AppT 
      (AppT ArrowT (VarT a_1627463132)) 
      (VarT a_1627463132)
    )
  ) 
  Nothing
  (Fixity 9 InfixL)"
```

I've formatted it a bit to make it a bit more legible.
We've got the `Name`, the `Type`, a `Nothing` value that is always `Nothing`, and the fixity of the function.
The `Type` seems pretty useful... Let's look at the `reify` output for one of the class methods we're trying to work with:

```haskell
λ: $(stringE . show =<< reify 'Web.Users.Types.getUserById)
"ClassOpI 
  Web.Users.Types.getUserById 
  (ForallT 
    [KindedTV b_1627432398 StarT] 
    [AppT (ConT Web.Users.Types.UserStorageBackend) (VarT b_1627432398)]
    (ForallT 
      [KindedTV a_1627482920 StarT]
      [AppT (ConT Data.Aeson.Types.Class.FromJSON) (VarT a_1627482920),AppT (ConT Data.Aeson.Types.Class.ToJSON) (VarT a_1627482920)]
      (AppT 
        (AppT 
          ArrowT 
          (VarT b_1627432398)
        )
        (AppT 
          (AppT
            ArrowT 
              (AppT 
                (ConT Web.Users.Types.UserId)
                (VarT b_1627432398)
              )
          )
          (AppT 
            (ConT GHC.Types.IO) 
            (AppT 
              (ConT GHC.Base.Maybe)
              (AppT 
                (ConT Web.Users.Types.User) 
                (VarT a_1627482920)
              )
            )
          )
        )
      )
    )
  ) 
  Web.Users.Types.UserStorageBackend 
  (Fixity 9 InfixL)"
```

WOOOOH. That is a ton of text!!
We're mainly interested in the `Type` declaration, and we can get a lot of information about what data constructors are used from [the rather nice documentation](https://hackage.haskell.org/package/template-haskell-2.10.0.0/docs/Language-Haskell-TH.html#t:Type).
Just like `AppE` is how we applied an expression to an expression, `AppT` is how we apply a type to a type.
`ArrowT` is the function arrow in the type signature.

Just as an exercise, we'll go through the following type signature and transform it into something a bit like the above:

```haskell
fmap :: (a -> b) -> f a -> f b
~ ((->) a b) -> (f a) -> (f b)
~ (->) ((->) a b) ((f a) -> (f b))
~ (->) ((->) a b) ((->) (f a) (f b))
```

Ok, now all of our `(->)`s are written in prefix form.
We'll replace the arrows with `ArrowT`, do explicit parentheses, and put in the `ApplyT` constructors working from the innermost expressions out.

```haskell
~ (ArrowT ((ArrowT a) b)) ((ArrowT (f a)) (f b))
~ (ArrowT ((ApplyT ArrowT a) b)) ((ArrowT (ApplyT f a)) (ApplyT f b))
~ (ArrowT (ApplyT (ApplyT ArrowT a) b)) (ApplyT (ApplyT ArrowT (ApplyT f a)) (ApplyT f b))
~ ApplyT (ArrowT (ApplyT (ApplyT ArrowT a) b)) (ApplyT (ApplyT ArrowT (ApplyT f a)) (ApplyT f b))
```

That got pretty out of hand and messy looking.
But, we have a good idea now of how we can get from one representation to the other.

So, going from our type signature, it looks like we can figure out how we can get the arguments we need from the type!
We'll pattern match on the type signature, and if we see something that looks like the continuation of a type signature, we'll add one to a count and go deeper.
Otherwise, we'll skip out.

The function definition looks like this:

```haskell
functionLevels :: Type -> Int
functionLevels = go 0
  where
    go :: Int -> Type -> Int
    go n (AppT (AppT ArrowT _) rest) =
      go (n+1) rest
    go n (ForallT _ _ rest) =
      go n rest
    go n _ =
      n
```

Neat! We can pattern match on these just like ordinary Haskell values.
Well, they *are* ordinary Haskell values, so that makes perfect sense.

Lastly, we'll need a function that gets the type from an `Info`.
Not all `Info` have types, so we'll encode that with `Maybe`.

```haskell
getType :: Info -> Maybe Type
getType (ClassOpI _ t _ _) = Just t
getType (DataConI _ t _ _) = Just t
getType (VarI _ t _ _)     = Just t
getType (TyVarI _ t)       = Just t
getType _                  = Nothing
```

Alright, we're ready to get started on that `decForFunc` function!!
We'll go ahead and fill in what we know we need to do:

```haskell
decForFunc :: Name -> Name -> Q Dec
decForFunc reader fn = do
  info <- reify fn
  arity <- maybe (reportError "Unable to get arity of name" >> return 0)
                 (return . functionLevels)
                 (getType info)
  -- ...
  return (FunD fnName [Clause varPat (NormalB final) []])
```

arity acquired. Now, we'll want to get a list of new variable names corresponding with the function arguments.
When we want to be hygienic with our variable names, we use the function `newName` which creates a totally unique variable name with the string prepended to it.
We want (1 - arity) new names, since we'll be using the bound value from the reader function for the other one.
We'll also want a name for the value we'll bind out of the lambda.

```haskell
varNames <- replicateM (arity - 1) (newName "arg")
b <- newName "b"

```

Next up is the new function name.
To keep a consistent API, we'll use the same name as the one in the actual package.
This will require us to import the other package qualified to avoid a name clash.

```haskell
let fnName = mkName . nameBase $ fn
```

`nameBase` has the type `Name -> String`, and gets the non-qualified name string for a given `Name` value.
Then we `mkName` with the string, giving us a new, non-qualified name with the same value as the original function.
This might be a bad idea? You probably want to provide a unique identifier.
Module namespacing does a fine job of that, imo.

Next up, we'll want to apply the `(>>=)` function to the `reader`.
We'll then want to create a function which applies the `bound` expression to a lambda.
Lambdas have an [LamE](https://hackage.haskell.org/package/template-haskell-2.10.0.0/docs/Language-Haskell-TH.html#v:LamE) constructor in the `Exp` type.
They take a `[Pat]` to match on, and an `Exp` that represents the lambda body.

```haskell
bound  = AppE (VarE '(>>=)) (VarE reader)
binder = AppE bound . LamE [VarP b] 
```

So `AppE bound . LamE [VarP b]` is the exact same thing as `(>>=) reader (\b -> ...)`! Cool.

Next up, we'll need to create `VarE` values for all of the variables.
Then, we'll need to apply all of the values to the `VarE fn` expression.
Function application binds to the left, so we'll have:

```haskell
fn       ~                   VarE fn
fn a     ~             AppE (VarE fn) (VarE a)
fn a b   ~       AppE (AppE (VarE fn) (VarE a)) (VarE b)
fn a b c ~ AppE (AppE (AppE (VarE fn) (VarE a)) (VarE b)) (VarE c)
```

This looks just like a left fold!
Once we have that, we'll apply the fully applied `fn` expression to `VarE 'liftIO`, and finally bind it to the lambda.

```haskell
varExprs   = map VarE (b : varNames)
fullExpr   = foldl AppE (VarE fn) varExprs
liftedExpr = AppE (VarE 'liftIO) fullExpr
final      = binder liftedExpr
```

This produces our `(>>=) reader (\b -> fn b arg1 arg2 ... argn)` expression.

The last thing we need to do is get our patterns.
This is just the list of variables we generated earlier.

```haskell
varPat = map VarP varNames
```

And now, the whole thing:

```haskell
deriveReader :: Name -> DecsQ
deriveReader rd =
  mapM (decForFunc rd) 
    [ 'destroyUserBackend
    , 'housekeepBackend
    , 'getUserIdByName
    , 'getUserById
    , 'listUsers
    , 'countUsers
    , 'createUser
    , 'updateUser
    , 'updateUserDetails
    , 'authUser
    , 'deleteUser
    ]

decForFunc :: Name -> Name -> Q Dec
decForFunc reader fn = do
  info <- reify fn
  arity <- maybe (reportError "Unable to get arity of name" >> return 0)
        (return . functionLevels) 
        (getType info)
  varNames <- replicateM (arity - 1) (newName "arg")
  b <- newName "b"
  let fnName     = mkName . nameBase $ fn
      bound      = AppE (VarE '(>>=)) (VarE reader)
      binder     = AppE bound . LamE [VarP b]
      varExprs   = map VarE (b : varNames)
      fullExpr   = foldl AppE (VarE fn) varExprs
      liftedExpr = AppE (VarE 'liftIO) fullExpr
      final      = binder liftedExpr
      varPat     = map VarP varNames
  return $ FunD fnName [Clause varPat (NormalB final) []]
```

And we've now metaprogrammed a bunch of boilerplate away!

We've looked at the docs for Template Haskell, figured out how to construct values in Haskell's AST, and worked out how to do some work at compile time, as well as automate some boilerplate.
I'm excited to learn more about the magic of defining quasiquoters and more advanced Template Haskell constructs, but even a super basic "build expressions and declarations using data constructors" approach is very useful.
Hopefully, you'll find this as useful as I did.


