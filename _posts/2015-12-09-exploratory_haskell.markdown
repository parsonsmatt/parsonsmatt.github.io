---
title: "Exploratory Haskell"
date: 2015-12-09
layout: post
categories: programming
redirect_from: "/programming/2015/12/09/exploratory_haskell.html"
---

## It doesn't have to be so thought out.

A lot of people think that Haskell is *great* for expressing a problem that you understand really well, but it's not so great for sketching out a problem and prototyping.
In Ruby, you can start writing code that kinda works, and refine it to be more and more correct.
In Haskell, you really have to get the code to type check before you do anything else, and if you don't have a good idea of your data model, then you don't have types to work with, and you can't really make it all work.
Right?

I'm fortunate enough to be able to pick whatever language I want to solve programming problems in my Artificial Intelligence course at UGA.
One assignment was to implement the [DPLL algorithm](https://en.wikipedia.org/wiki/DPLL_algorithm) from the textbook (Russell and Norvig).
I decided to copy the pseudocode, translate it directly into Haskell, and see if I could get to a working solution from there.
The Wikipedia article linked does a good job explaining the algorithm, so I'll just go over the code.
Very briefly, the algorithm is a way of taking a list of clauses of Boolean logic and determining whether or not there is a way to assign the variables in the clauses to make the whole expression true.

The original code is available [in this repository](https://www.github.com/parsonsmatt/dpll).

## The Pseudocode

Here is the pseudocode for the algorithm, reproduced from Norvig and Russell's textbook:

```
function DPLL(clauses, symbols, model ) returns true or false
    if every clause in clauses is true in model then
        return true
    if some clause in clauses is false in model then 
        return false
    P, value ← FIND-PURE-SYMBOL (symbols, clauses, model)
    if P is non-null then 
        return DPLL(clauses, symbols – P, model ∪ {P =value})
    P, value ← FIND-UNIT-CLAUSE (clauses, model)
    if P is non-null then
        return DPLL(clauses, symbols – P, model ∪ {P =value})
    P ← FIRST (symbols)
    rest ← REST (symbols)
    return DPLL(clauses, rest, model ∪ {P =true}) 
        or DPLL(clauses, rest, model ∪ {P =false}))
```

Now let's make it Haskell!
We'll translate names and terms directly and assume functions exist that we can use.
We already know that `P` can be nullable, so we'll go ahead and use `Maybe` for that type.
Instead of `if` checking, we'll pattern match on the result.

```haskell
dpll :: Clauses -> Symbols -> Model -> Bool
dpll clauses symbols model =
  if all (`isTrueIn` model) clauses then
      True
  else if any (`isFalseIn` model) clauses then
      False
  else case findPureSymbol symbols clauses model of
      (Just sym, val) ->
          dpll clauses (symbols `minus` sym)
                       (model `including` (sym := val))
      (Nothing, val) ->
          case findUnitClause clauses model of
              (Just sym, val) ->
                  dpll clauses (symbols `minus` sym)
                               (model `including` (sym := val))
              (Nothing, val) ->
                  case symbols of
                       (x:xs) ->
                           dpll clauses xs (model `including` (x := False))
                           || dpll clauses xs (model `including` (x := True))
                       [] ->
                           False -- should not happen?
```

This doesn't compile or type check at all.
Despite that, `hlint` is capable of suggesting code improvements, and we can follow those to a much nicer looking implementation.
Here's a mostly syntastic transformation to the above code:

```haskell
dpll :: Clauses -> Symbols -> Model -> Bool
dpll clauses symbols model
  | all (`isTrueIn` model) clauses  = True
  | any (`isFalseIn` model) clauses = False
  | otherwise =
      case findPureSymbol symbols clauses model of
           Just (sym := val) ->
             dpll clauses (symbols `minus` sym)
                           (model `including` (sym := val))
           Nothing ->
             case findUnitClause clauses model of
                  Just (sym := val) ->
                    dpll clauses (symbols `minus` sym)
                                  (model `including` (sym := val))
                  Nothing ->
                    case symbols of
                         (x:xs) ->
                           dpll clauses xs (model `including` (x := False)) 
                           || dpll clauses xs (model `including` (x := True))
                         [] ->
                           False -- really why
```

The "pattern matching on Maybe" approach is kind of ugly.
Let's extract those branches into named subexpressions and use `maybe` from `Data.Maybe` to choose which branch to go on, and pattern match directly on the list at the function definition rather than later on.

```haskell
dpll :: Clauses -> Symbols -> Model -> Bool
dpll clauses symbols@(x:xs) model
  | all (`isTrueIn` model) clauses  = True
  | any (`isFalseIn` model) clauses = False
  | otherwise =
      maybe pureSymbolNothing pureSymbolJust (findPureSymbol symbols clauses model)
  where
    pureSymbolJust (sym := val) =
      dpll clauses (symbols `minus` sym) (model `including` (sym := val))
    pureSymbolNothing =
      maybe unitClauseNothing unitClauseJust (findUnitClause clauses model)
    unitClauseJust (sym := val) =
      dpll clauses (symbols `minus` sym) (model `including` (sym := val))
    unitClauseNothing =
      dpll clauses xs (model `including` (x := False))
      || dpll clauses xs (model `including` (x := True))
```

We've eliminated the manual pattern matching.
While more astute programmers might have noticed what's going on with the previous form, I didn't quite catch on to the overall pattern.
Note that we're doing the same thing in every terminal branch: we recursively call `dpll`, and only the variable assignment changes!
More than that, we're expressing the idea of "Try this thing. If it succeeds, take the result. If it fails, try the next thing."

The `Maybe` monad captures a similar idea, but isn't quite what we want -- that returns `Nothing` if any are `Nothing`, and really, we want to take the first `Just` value and do something with it.

`Maybe` has an `Alternative` instance, which does exactly what we want:

```haskell
> Nothing <|> Just 10 <|> Nothing <|> Just 2
Just 10
```

One of the cool things we can do with an `Alternative` is the [`asum`](https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-Foldable.html#v:asum) function.
We give it a `Foldable t` full of `Alternative f => f a` values, and it gives us back an `f a` based on the definition of `Alternative` for our `f`.
We can very succinctly express the idea "Try these actions and take the first successful one" with this!

Alright, let's refactor our code to use that idea.
We're using the `Maybe` functor all over the place, so to bring `dpll` in line with the `find` functions, we'll return a `Maybe Model`.
This makes the function even more useful -- now you get back the model instead of just whether or not the statement is satisfiable!

```haskell
dpll :: Clauses -> Symbols -> Model -> Maybe Model
dpll clauses symbols model
  | all (`isTrueIn` model) clauses  = Just model
  | any (`isFalseIn` model) clauses = Nothing
  | otherwise =
      let controlList :: [Maybe Assignment]
          controlList =
            [ findPureSymbol symbols clauses model
            , findUnitClause clauses model
            , (:= False) <$> listToMaybe symbols
            , (:= True)  <$> listToMaybe symbols
            ]
```

`controlList` is our list of possible assignments.
We want to take the first one that actually works and do something with it.

```haskell
          next :: Assignment -> Maybe Model
          next (sym := val) =
            dpll clauses (symbols `minus` sym) (model `including` (sym := val))
```

The next step is taking the assignment, and recursively calling `dpll` with the symbol removed from the symbol collection and the model including the assignment.

And now, for the expression that puts everything together:

```haskell
       in asum (map (>>= next) controlList)
```


Haskell's laziness works well for us here.
In a strict language, this code would generate *all possible* models before picking the first one.
We can really leverage Haskell's laziness to make this efficient and fast.

## "uh but it still doesn't compile"

So we've taken a pseudocode algorithm, translated it to Haskell, and applied a bunch of Haskell idioms to it.
There are no type definitions, and the code doesn't compile, and virtually no functions are defined.
Now I guess we'll want to fill in those lower level details with stuff that fits what we want.

Let's analyze what we're doing with the terms to get some idea of what data types will fit for them.
A symbol could be anything -- but we'll just use a String for now.

```haskell
type Symbol = String
```

I've used `listToMaybe` on the symbols.
This betrays my intent -- `Symbols` is going to be a list.
That gives us enough information to define `minus` as well.

```haskell
type Symbols = [Symbol]

minus :: Symbols -> Symbol -> Symbols
minus xs = (xs \\) . pure
-- or, if you don't like point-free,
minus xs x = xs \\ [x]
```

The `Clauses` type is a collection of expressions.
An expression is a list of terms in [Conjunctive Normal Form (CNF)](https://en.wikipedia.org/wiki/Conjunctive_normal_form).

```haskell
type Clauses = [CNF]
```

With CNF, juxtaposion is conjunction.
We can therefore represent a CNF expression as a list of literals:

```haskell
type CNF = [Literal]
```

Finally, a literal is a pair of Sign and Symbol.

```haskell
type Literal = (Sign, Symbol)
```

A sign is a function that either negates or doesn't negate a boolean expression.

```haskell
data Sign = Pos | Neg
  deriving (Eq, Ord, Show)

apply :: Sign -> Bool -> Bool
apply Pos = id
apply Neg = not

-- some helper functions for testing
n :: Symbol -> Literal
n c = (Neg, c)

p :: Symbol -> Literal
p c = (Pos, c)
```

We can now actually construct an example `Clauses` to test our function (when it, you know, compiles):

```haskell
ex :: Clauses
ex =
  [ [ n "p", p "a", p "c" ]
  , [ n "a" ]
  , [ p "p", n "c" ]
  ]
```

The main use we have for the clauses type is to map over it with `isTrueIn` and `isFalseIn`, checking every clause in the list against the model.
The model is an assignment of symbols to truth values, and random access time is important.
We'll use a map.

```haskell
type Model = Map Symbol Bool

isTrueIn :: CNF -> Model -> Bool
isTrueIn cnf model = or (mapMaybe f cnf)
  where
    f (sign, symbol) = apply sign <$> Map.lookup symbol model
```

Here, we're looking up every symbol in the CNF expression, and applying the literal's sign to the possible value.
If there's no value in the model, then it doesn't return anything.
`or` checks to see if there are any `True` values in the resulting list.
If there are, then the CNF is true in the model.

```haskell
isFalseIn :: CNF -> Model -> Bool
isFalseIn cnf model = all not literals
  where
    literals =
      map f cnf
    f (sign, symbol) =
      apply sign (Map.findWithDefault (apply sign True) symbol model)
```

`isFalseIn` is trickier -- we map over the CNF expression with a default value of the `sign` applied to `True`.
Then, we apply the `sign` again to the resulting value.
`all not` is a way of saying "every element is false."

Now the compiler is complaining about not recognizing the `:=` symbol.
As it happens, any infix function that prefixed with a `:` is a data constructor.
We'll define the data type `Assignment` and give it some accessor functions.

```haskell
data Assignment 
  = (:=) 
  { getSymbol :: Symbol
  , getValue :: Bool 
  }

instance Show Assignment where
  show (s := v) = "(" ++ s ++ " := " ++ show v ++ ")"
```

An advantage of using a data constructor is that we can pattern match on the values of that constructor.
This gives us a rather nice definition of the `including` function:

```haskell
including :: Model -> Assignment -> Model
including m (sym := val) = Map.insert sym val m
```

The final remaining items that aren't defined are `findPureSymbol` and `findUnitClause`.
From the textbook,

> Pure symbol heuristic: A pure symbol is a symbol that always appears with the same "sign" in all clauses.
> For example, in the three clauses (A ∨ ¬ B), (¬ B ∨ ¬ C), and (C ∨ A), the symbol A is pure because only the positive literal appears, B is pure because only the negative literal appears, and C is impure.

If a symbol has all negative signs, then the returned assignment is False.
If a symbol has all positive signs, then the returned assignment is True.
We'll punt refining the clauses with the model to a future function...

```haskell
findPureSymbol :: Symbols -> Clauses -> Model -> Maybe Assignment
findPureSymbol symbols clauses' model =
  asum (map makeAssignment symbols)
  where
    clauses = refinePure clauses' model
    makeAssignment :: Symbol -> Maybe Assignment
    makeAssignment sym =
      (sym :=) <$> negOrPos (signsForSymbol sym)
```

We're using `asum` again to pick the first assignment that works out.
This maps the assignment of the sym variable over the `negOrPos` function, which determines whether the symbol should have a True or False assignment.

```haskell
    signsForSymbol :: Symbol -> [Sign]
    signsForSymbol sym =
      clauses >>= signsForSymInClause sym
    signsForSymInClause :: Symbol -> CNF -> [Sign]
    signsForSymInClause sym =
      map fst . filter ((== sym) . snd)
```

So we're filtering the `CNF` (a list of `Literal`s or `(Sign, Symbol)`) to only contain the elements whose second element is equal to the symbol.
And then we're extracting the first element, leaving just the `Sign`s.

```haskell
    negOrPos :: [Sign] -> Maybe Bool
    negOrPos = getSingleton . nub

getSingleton :: [a] -> Maybe a
getSingleton [x] = Just x
getSingleton _   = Nothing
```

Finally, we `nub` the list, and if it's a singleton, then we have a success.

Now, we'll define the `findUnitClause` function.
From the textbook,

> Unit clause heuristic: A unit clause was defined earlier as a clause with just one literal.
In the context of DPLL, it also means clauses in which all literals but one are already assigned false by the model.
For example, if the model contains B = true, then (¬B ∨ ¬C) simplifies to ¬C, which is a unit clause.
Obviously, for this clause to be true, C must be set to false.
The unit clause heuristic assigns all such symbols before branching on the remainder.

As above, we'll punt refining the clauses with the model until later.

```haskell
findUnitClause :: Clauses -> Model -> Maybe Assignment
findUnitClause clauses' model = assignSymbol <$> firstUnitClause
  where
    clauses :: Clauses
    clauses = refineUnit clauses' model

    firstUnitClause :: Maybe Literal
    firstUnitClause =
      asum (map (getSingleton .  mapMaybe ifInModel) clauses)

    ifInModel :: Literal -> Maybe Literal
    ifInModel (sign, symbol) = 
      case Map.lookup symbol model of
           Just val -> if apply sign val
                          then Nothing
                          else Just (sign, symbol)
           _ -> Just (sign, symbol)

    assignSymbol :: Literal -> Assignment
    assignSymbol (sign, symbol) = symbol := apply sign True
```

This is much simpler than pure symbols!
We map assignment over the first unit clause that is found.
The first unit clause is found with the `asum` technique.
We take the list of clauses, and for each clause, first determine what to do if it's in the model already.
If the symbol is in the model, then we check to see if the literal in the clause has a `True` or `False` value by applying the sign to the value.
If the value is True, then we don't include it.
Otherwise, we include the literal in the list.
Finally, we attempt to get the singleton list.
`asum` gets the first clause which satisfies these conditions.

Now, in the previous functions, we punted refining the clauses.
It's time to do that.
For a pure symbol, the given optimization is (from the book):

> Note that, in determining the purity of a symbol, the algorithm can
ignore clauses that are already known to be true in the model constructed
so far. For example, if the model contains B = false, then the clause
(¬ B ∨ ¬ C) is already true, and in the remaining clauses C appears only
as a positive literal; therefore C becomes pure.

We'll start by folding the model and clauses into a new set of clauses.
The helper function will go through each symbol in the model, find the relevant clauses, and modify them appropriately.

```haskell
refinePure :: Clauses -> Model -> Clauses
refinePure = Map.foldrWithKey f
  where
    f :: Symbol -> Bool -> Clauses -> Clauses
    f sym val = map discardTrue
      where
        discardTrue =
          filter (not . clauseIsTrue)
        clauseIsTrue (sign, symbol) =
          symbol == sym && apply sign val
```

The optimization from the text for the unit clause is:

> In the context of DPLL, it also means clauses in which all literals but one are already assigned false by the model.
For example, if the model contains B = true, then (¬ B ∨ ¬ C) simplifies to ¬ C, which is a unit clause.
Obviously, for this clause to be true, C must be set to false.
The unit clause heuristic assigns all such symbols before branching on the remainder.

```haskell
refineUnit :: Clauses -> Model -> Clauses
refineUnit clauses model = map refine clauses
  where
    refine :: CNF -> CNF
    refine cnf =
      case allButOneFalse cnf of
           Just (s := True)  -> [p s]
           Just (s := False) -> [n s]
           Nothing -> cnf

    allButOneFalse :: CNF -> Maybe Assignment
    allButOneFalse =
      getSingleton . filter (not . getValue) . map assign

    assign :: Literal -> Assignment
    assign (sign, sym) =
      sym := Map.findWithDefault (apply sign True) sym model
```

If all but one of the literals in the CNF are false, then we return that with the proper assigment.
Otherwise, we return the whole CNF expression.

Starting from a straight up transcription, we've now finally implemented enough to solve problems!

```haskell
solved :: Maybe Model
solved = dpll ex ["p", "a", "c"] Map.empty
```

The output will be kind of ugly, so let's make a pretty printing function:

```haskell
showModel :: Model -> String
showModel = 
  unlines . map (show . snd) . Map.toList . Map.mapWithKey (:=)
```

Evaluating `solved` in GHCi gives us:
    
    Prelude HW6> putStr . showModel . fromJust $ solved
    (a := False)
    (c := False)
    (p := False)

Assigning the variables with those values does return a true model.
Nice!

## Australia

Given a set of colors, and a map, can you color every state in a way that all touching states have different colors?

This is the coloring problem (with a good [intro here](https://www.seas.upenn.edu/~cis391/Lectures/CSP-6up.pdf)).
As it happens, Australia makes a nice simple model for this, and the three coloring of Australia is easy enough to do by hand that it makes a good model for testing our software out.
Let's define the problem and find the solution!

First, we'll want to define our symbols:

```haskell
colors :: [Symbol]
colors = [green, blue, red]

green = "-green"
blue = "-blue"
red = "-red"

states :: [Symbol]
states =
  [ western
  , southern
  , northern
  , queensland
  , newSouthWales
  , victoria
  ]

western  = "Western"
southern = "Southern"
northern = "Northern"
queensland = "Queensland"
newSouthWales = "New South Wales"
victoria = "Victoria"
```

Now we'll express that a given state can have one color, but only one:

```haskell
hasColor :: Symbol -> Clauses
hasColor st = 
   [ [ p $ st `is` green
     , p $ st `is` blue
     , p $ st `is` red
     ]
   , [ n $ st `is` blue
     , n $ st `is` red
     ]
   , [ n $ st `is` green
     , n $ st `is` red
     ]
   , [ n $ st `is` green
     , n $ st `is` blue
     ]
   ]
```

Since our symbols are lists, we can concatenate them together.
We don't want to get the two confused, so we make specialized functions that only work on symbols and clauses, respectively.

```haskell
is :: Symbol -> Symbol -> Symbol
is = (++)

(/\) :: Clauses -> Clauses -> Clauses
(/\) = (++)
```

And, since we'll often want to take a list of things, apply a function to each, and make a clause out of the whole thing, we'll alias the `bind` function to something that looks kind of like "take the conjunction of this whole set."

```haskell
(/\:) :: Monad m => m a -> (a -> m b) -> m b
(/\:) = (>>=)
```

At first, we'll simply say that every state has a color.

```haskell
initialConditions :: Clauses
initialConditions = states /\: hasColor
```

Next, we'll say that for a pair of adjacent states, they can't both be the same color.

```haskell
adjNotEqual :: (Symbol, Symbol) -> Clauses
adjNotEqual (a, b) = colors /\: bothAreNot
  where
    bothAreNot color =
      [ [ n $ a `is` color
        , n $ b `is` color
        ]
      ]
```

Next, a list of adjacent states...

```haskell
adjStates :: [(Symbol, Symbol)]
adjStates =
  [ (western, northern)
  , (western, southern)
  , (northern, southern)
  , (northern, queensland)
  , (southern, newSouthWales)
  , (southern, victoria)
  , (southern, queensland)
  , (newSouthWales, queensland)
  , (newSouthWales, victoria)
  ]

adjacentStatesNotEqual :: Clauses
adjacentStatesNotEqual = adjStates /\: adjNotEqual

australiaClauses :: Clauses
australiaClauses =
  initialConditions /\ adjacentStatesNotEqual

australiaSymbols :: Symbols 
australiaSymbols =
  is <$> states <*> colors
```

Now, we've finally accomplished the encoding, and we can get the solution.

```haskell
australiaSolution :: Maybe Model
australiaSolution = dpll australiaClauses australiaSymbols mempty
```

It can be printed with the following function:

```haskell
showOnlyTrue :: Model -> String
showOnlyTrue =
  unlines . map (show . snd) 
    . filter (getValue . snd) 
    . Map.toList . Map.mapWithKey (:=)

printAustralia :: IO ()
printAustralia = do
  let model = fromJust australiaSolution
  putStrLn (showOnlyTrue model)
```

Which, when evaluated in GHCi, gives us:

    Prelude HW6> printAustralia
    (New South Wales-red := True)
    (Northern-red := True)
    (Queensland-blue := True)
    (Southern-green := True)
    (Victoria-blue := True)
    (Western-blue := True)

This can be verified manually to be a correct coloring of Australia.

Well, we started with a bunch of pseudocode, transcribed it directly into Haskell syntax, refactored it blindly until it was nice and idiomatic, and finally started implementing the details we'd assumed.
This is rather different from how I usually approach solving problems in Haskell, and it was pretty fun.
