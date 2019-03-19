---
title: "Implementing UNION in Esqueleto I"
date: 2019-01-31
layout: post
categories: programming
---

We use the SQL `UNION` operator at IOHK in one of our `beam` queries, and `esqueleto` does not support it.
To make porting the IOHK SQL code more straightforward, I decided to implement `UNION`.
This blog post series will delve into implementing this feature, in a somewhat stream-of-thought manner.

This is the first post in the series.
[Click here for part II]({% post_url 2019-02-01-esqueleto_union_ii %}).

# Background

`esqueleto` is a SQL library that builds on the `persistent` library for database definitions and simple queries.
It attempts to provide an embedded DSL that allows you to use SQL and Haskell together.
In my opinion, it has less complicated types than `beam` and an easier to learn UX than `opaleye`.
The `persistent` quasiquoter model definitions save a bunch of boilerplate, too.

`esqueleto` is implemented in a somewhat convoluted manner -- we have a type class `Esqueleto query expr backend` that everything is defined in terms of.
However, the functional depenencies on the class essentially only permit a single instance.
The `query` type is always fixed to `SqlQuery`, a `WriterT [Clauses] (StateT IdentInfo)` monad.
The `expr` type is always `SqlExpr`, which is a GADT that provides a structure for SQL expressions.

It's kind of a tagless final encoding paired with a GADT initial encoding.
Neat.

# Goal

Alright, so let's start with the SQL.
We want to be able to write a Haskell thing that translates to this SQL:

```sql
SELECT name
FROM person
UNION
SELECT title
FROM blog_post
```

Let's just write the syntax out that `esqueleto` usually uses, and see where that takes us:

```haskell
unionTest = ( 
  select $ 
  from $ \person ->
  return (person ^. PersonName)
  ) `union` (
  select $
  from $ \blog ->
  return (blog ^. BlogPostTitle)
  )
```

This is a pleasing looking API!
Can it work?
What type would `union` need to have?

Well, probably not.
Let's look at the type of `select`:

```haskell
select 
  :: (SqlSelect a r, MonadIO m)
  => SqlQuery a 
  -> SqlReadT m [r]
```

Once something has become a `SqlReadT` value, we can't really introspect on the query structure anymore.
So we can't have this syntax :(

Let's try something else:

```haskell
unionTest = 
  select $
  union 
    ( from $ \person -> do
      pure (person ^. PersonName)
    )
    ( from $ \blog -> do
      pure (blog ^. BlogPostTitle)
    )
```

This means that `union` will end up returning a `SqlQuery a`.
It takes two arguments, each of which is a query returning the same thing.
We have our first attempt at a type to implement!

```haskell
union
  :: SqlQuery a
  -> SqlQuery a
  -> SqlQuery a
union query0 query1 = undefined
```

# First Attempt

Alright, so, uh, how do we make values of type `SqlQuery`?
Let's first look at what the type actually is:

```haskell
-- | SQL backend for @esqueleto@ using 'SqlPersistT'.
newtype SqlQuery a = Q 
  { unQ :: W.WriterT SideData (S.State IdentState) a 
  }

-- | Side data written by 'SqlQuery'.
data SideData = SideData 
  { sdDistinctClause :: !DistinctClause
  , sdFromClause     :: ![FromClause]
  , sdSetClause      :: ![SetClause]
  , sdWhereClause    :: !WhereClause
  , sdGroupByClause  :: !GroupByClause
  , sdHavingClause   :: !HavingClause
  , sdOrderByClause  :: ![OrderByClause]
  , sdLimitClause    :: !LimitClause
  , sdLockingClause  :: !LockingClause
  }

-- | List of identifiers already in use and supply of temporary
-- identifiers.
newtype IdentState = IdentState 
  { inUse :: HS.HashSet T.Text 
  }

initialIdentState :: IdentState
initialIdentState = IdentState mempty
```

So, we use the `WriterT SideData` to accumulate information about the query we're building.
And then we use `IdentState` to keep track of identifiers in use.

Let's look at some things that return `SqlQuery` values.
I searched through the `Database.Esqueleto.Internal.Sql` module for `-> SqlQuery` and got some interesting results.
The only function that *returns* a `SqlQuery` value in the whole module is this:

```haskell
-- line 497
  withNonNull :: PersistField typ
              => SqlExpr (Value (Maybe typ))
              -> (SqlExpr (Value typ) -> SqlQuery a)
              -> SqlQuery a
  withNonNull field f = do
    where_ $ not_ $ isNothing field
    f $ veryUnsafeCoerceSqlExprValue field
```

Okay, so `where_` is a `SqlQuery` function.
Let's look for it's definition:

```haskell
class (Monad query) =>
  Esqueleto query expr backend 
    | query -> expr backend
    , expr -> query backend 
    where
-- snip...

-- in Database.Esqueleto.Internal.Language, line 93
  -- | @WHERE@ clause: restrict the query's result.
  where_ :: expr (Value Bool) -> query ()
```

The class definition has functional dependencies that basically make it so you can determine any type variable from any other.
Since `persistent` uses the `SqlBackend` type for the `backend`, you end up needing to totally pick `Esqueleto SqlQuery SqlExpr SqlBackend`, and you can't vary any of those types.

Okay, let's find the instance definition:

```haskell
-- line 452 in Database.Esqueleto.Internal.Sql
  where_ expr = Q $ W.tell mempty { sdWhereClause = Where expr }

  on expr = Q $ W.tell mempty { sdFromClause = [OnClause expr] }

  groupBy expr = Q $ W.tell mempty { sdGroupByClause = GroupBy $ toSomeValues expr }

  having expr = Q $ W.tell mempty { sdHavingClause = Where expr }

  locking kind = Q $ W.tell mempty { sdLockingClause = Monoid.Last (Just kind) }
```

There's actually a bunch, and they mostly just `tell` about a part of the query we're building.
Cool.
This may be useful soon, but it's not immediately obvious to me *how*.

I spent some time perusing the rest of the library, and I found another combinator that *takes* a `SqlQuery` value and produces something else:

```haskell
sub 
  :: PersistField a 
  => Mode 
  -> SqlQuery (SqlExpr (Value a)) 
  -> SqlExpr (Value a)
sub mode query = ERaw Parens $ \info -> toRawSql mode info query
```

This looks useful!
Let's look at the `ERaw` constructor, which is from the `SqlExpr` datatype:

```haskell
  -- Raw expression: states whether parenthesis are needed
  -- around this expression, and takes information about the SQL
  -- connection (mainly for escaping names) and returns both an
  -- string ('TLB.Builder') and a list of values to be
  -- interpolated by the SQL backend.
  ERaw     
    :: NeedParens 
    -> (IdentInfo -> (TLB.Builder, [PersistValue])) 
    -> SqlExpr (Value a)
```

Okay, so we can *start* with this approach and just generate the raw SQL we need.
This is *probably* the wrong approach, but it might work, and working is better than imaginary.

```haskell
union
  :: PersistField a
  => SqlQuery (SqlExpr (Value a))
  -> SqlQuery (SqlExpr (Value a))
  -> SqlQuery (SqlExpr (Value a))
union query0 query1 =
  pure $ ERaw Parens $ \info ->
    let
      (q0, v0) = toRawSql SELECT info query0
      (q1, v1) = toRawSql SELECT info query1
     in
      (q0 <> " UNION " <> q1, v0 <> v1)
```

This is *basically* what `sub` does.
We just concatenate them with the `UNION` operator in between.
Let's write a test and see how it works!

## Testing the First Approach

I hop into the `esqueleto` test suite and start writing my test:

```haskell
testCaseUnion :: Run -> Spec
testCaseUnion run = do
  describe "union" $ do
    it "works" $ do
      run $ do
        let
          names =
            [ "john", "joe", "jordan", "james" ]
          blogs =
            [ "asdf", "qwer", "berty", "nopex" ]
        (pid:_) <- forM names $ \name ->
          insert (Person name Nothing Nothing 3)

        forM_ blogs $ \blog ->
          insert (BlogPost blog pid)

        res <- select $
          ( from $ \person -> do
            pure (person ^. PersonName)
          )
          `union`
          ( from $ \blog -> do
            pure (blog ^. BlogPostTitle)
          )

        liftIO $
          L.sort (map unValue res)
            `shouldBe`
              L.sort (names <> blogs)
```

We insert four blogs and people into the database, and then get the `UNION` of their names and titles.
Does it work?

No :(

```
  test/Common/Test.hs:1421:11:
  1) Tests that are common to all backends, union, works
       expected: ["asdf","berty","james","joe","john","jordan","nopex","qwer"]
        but got: ["asdf"]
```

Okay, that's a bit weird.
Why does it only pick the first?
Let's test our understanding and try a raw query.
I'll add a line that runs the raw SQL and then I'm going to `error` out to see the output in the test suite:

```haskell
-- ...
res' <- rawSql 
  (  "SELECT Person.name FROM Person "
  <> "UNION "
  <> "SELECT BlogPost.title FROM BlogPost" 
  )
  []
error (show $ map unSingle (res' :: [Single String]))
-- ...
```

Now, we get an error that shows what that query returned:

```
  test/Common/Test.hs:1417:9:
  1) Tests that are common to all backends, union, works
       uncaught exception: ErrorCall
       ["asdf","berty","james","joe","john","jordan","nopex","qwer"]
       CallStack (from HasCallStack):
         error, called at test/Common/Test.hs:1417:9 in main:Common.Test
```

Okay, that's *exactly* what I expected to see!
So there's something weird about how the query is being generated.
I want to get a textual representation of the query, and `toRawSql` is the function to do that.
I'm going to make a wrapper around it:

```haskell
renderQuery
  :: (Monad m, EI.SqlSelect a r)
  => SqlQuery a
  -> SqlPersistT m TL.Text
renderQuery q = do
  conn <- ask
  pure (queryToText conn q)

queryToText :: EI.SqlSelect a r => SqlBackend -> SqlQuery a -> TL.Text
queryToText conn q =
  let (tlb, _) = EI.toRawSql EI.SELECT (conn, EI.initialIdentState) q
  in TLB.toLazyText tlb
```

And we'll render the query:

```haskell
-- ...snip
        let
          q =
            ( from $ \person -> do
              pure (person ^. PersonName)
            )
            `union`
            ( from $ \blog -> do
              pure (blog ^. BlogPostTitle)
            )
        res <- select q
        error . show =<< renderQuery q
-- snip...
```

Now, what do we get?

```
  test/Common/Test.hs:1415:9:
  1) Tests that are common to all backends, union, works
       uncaught exception: ErrorCall
       "SELECT (SELECT \"Person\".\"name\"\nFROM \"Person\"\n UNION SELECT \"BlogPost\".\"title\"\nFROM \"BlogPost\"\n)\n"
       CallStack (from HasCallStack):
         error, called at test/Common/Test.hs:1415:9 in main:Common.Test
```

Okay, so it's doing something that we *don't* want.
We want this:

```sql
SELECT name
FROM person
UNION
SELECT title
FROM blog_post
```

And it's doing this:

```sql
SELECT (
  SELECT name
  FROM person
  UNION
  SELECT title
  FROM blog_post
)
```

Which explains our problem! We actually need it do `SELECT * FROM (the union query)`.
Or removing the outer SELECT entirely.
So, this suggests that this isn't the right approach.

Next post, I'll attempt to find another way to implement it, and write down the stream-of-thought process on how I got there.
