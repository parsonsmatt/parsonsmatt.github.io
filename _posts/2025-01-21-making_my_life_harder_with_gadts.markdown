---
title: "Making My Life Harder with GADTs"
date: 2025-01-21
layout: post
categories: programming
---

Lucas Escot wrote a good blog post titled ["Making My Life Easier with GADTs"](https://acatalepsie.fr/posts/making-my-life-easier-with-gadts.html), which contains a demonstration of GADTs that made his life easier.
He posted the article to [reddit](https://www.reddit.com/r/haskell/comments/1i6f48k/making_my_life_easier_with_gadts/).

I'm going to trust that - for his requirements and anticipated program evolution - the solution is a good one for him, and that it actually made his life easier. 
However, there's one point in his post that I take issue with:

> Dependent types and assimilated type-level features get a bad rep. They are often misrepresented as a futile toy for “galaxy-brain people”, providing no benefit to the regular programmer. I think this opinion stems from a severe misconception about the presumed complexity of dependent type systems.

I am *often* arguing against complexity in Haskell codebases.
While Lucas's prediction about "misconceptions" may be true for others, it is not true for me.
I have worked extensively with Haskell's most advanced features in large scale codebases.
I've studied ["Types and Programming Languages,"](https://www.cis.upenn.edu/~bcpierce/tapl/) [the Idris book](https://www.manning.com/books/type-driven-development-with-idris), ["Type Theory and Formal Proof"](http://anggtwu.net/tmp/nederpelt_geuvers__type_theory_and_formal_proof_an_introduction.pdf), and many other resources on advanced type systems.
I don't say this to indicate that I'm some kind of genius or authority, just that I'm not a rube who's looking up on the Blub Paradox.
My argument for simplicity comes from the hard experience of having to rip these advanced features out, and the pleasant discovery that simpler alternatives are usually nicer in every respect.

So how about GADTs?
Do they make my life easier?
Here, I'll reproduce [the comment I left on reddit](https://www.reddit.com/r/haskell/comments/1i6f48k/making_my_life_easier_with_gadts/m8ddgn7/):

---

> They are often misrepresented as a futile toy for “galaxy-brain people”, providing no benefit to the regular programmer. I think this opinion stems from a severe misconception about the presumed complexity of dependent type systems. 

This opinion - in my case at least - stems from having seen people code themselves into a corner with fancy type features where a simpler feature would have worked just as well. 

In this case, the "simplest solution" is to have two entirely separate datatypes, as the blog post initially starts with. These datatypes, after all, *represent different things* - a typed environment and an untyped environment. Why mix the concerns? What pain or requirement is solved by having *one* more complicated datatype when *two* datatypes works pretty damn well?

> I could indeed keep typed environments completely separate. Different datatypes, different information. But this would lead to a lot of code duplication. Given that the compilation logic will mostly be mostly identical for these two targets, I don’t want to be responsible for the burden of keeping both implementations in sync.

Code duplication can be a real concern. In this case, we have code that is not *precisely* duplicated, but simply *similar* - we want compilation logic to work for both untyped *and* typed logics, and only take typing information into account. When we want code to work over multiple possible types, we have two options: parametric polymorphism and ad-hoc polymorphism.

With parametric polymorphism, the solution looks like this:

```haskell
data GlobalEnv a = GlobalEnv [(Name, GlobalDecl a)]

data GlobalDecl a
  = DataDecl (DataBody a)
  | FunDecl  (FunBody a)
  | TypeDecl a

data DataBody a = DataBody
  { indConstructors :: [ConstructorBody a]
  }

data ConstructorBody a = ConstructorBody
  { ctorName :: Name
  , ctorArgs :: Int
  , ctorType :: a
  }

data FunBody a = FunBody
  { funBody :: LamBox.Term
  , funType :: a
  }
```

This is actually *very* similar to the GADT approach, because we're threading a type variable through the system. For untyped, we can write `GlobalDecl ()`, and for typed, we can write `GlobalDecl LamBox.Type`.

Functions which can work on either untyped or typed would have `GlobalDecl a -> _` as their input, and functions which require a representation can specify it directly. This would look very similar to the GADT approach: in practice, replace `GlobalDecl Typed` with `GlobalDecl Type` and `GlobalDecl Untyped` with `GlobalDecl ()` and you're good.

(or, heck, `data Untyped = Untyped` and the change is even smaller).

This representation is *much* easier to work with. You can `deriving stock (Show, Eq, Ord)`. You can `$(deriveJSON ''GlobalEnv)`. You can delete several language extensions. It's also more flexible: you can use `Maybe Type` to represent *partially typed* programs (or programs with type inference). You can use `Either TypeError Type` to represent full ASTs with type errors. You can `deriving stock (Functor, Foldable, Traversable)` to get access to `fmap` (change the type with a function) and `toList` (collect all the types in the AST) and `traverse` (change each type effectfully, combining results).

When we choose GADTs here, we pay significant implementation complexity costs, and we give up flexibility. What is the benefit? Well, the entire benefit *is* that we've given up flexibility. With the parametric polymorphism approach, we can put *anything* in for that type variable `a`. The GADT prevents us from writing `TypeDecl ()` and it forbids you from having anything other than `Some (type :: Type)` or `None` in the fields.

This *restriction* is what I mean by 'coding into a corner'. Let's say you get a new requirement to support partially typed programs. If you want to stick with the GADT approach, then you need to change `data Typing = Typed | Untyped | PartiallyTyped` and modify all the `WhenTyped` machinery - `Optional :: Maybe a -> WhenTyped PartiallTyped a`. Likewise, if you want to implement inference or type-checking, you need another constructor on `Typing` and another on`WhenTyped` - `... | TypeChecking` and `Checking :: Either TypeError a -> WhenTyped TypeChecking a`.

But wait - now our `TypeAliasDecl` has become overly strict!

```haskell
data GlobalDecl :: Typing -> Type where
  FunDecl       :: FunBody t     -> GlobalDecl t
  DataDecl      :: DataBody t    -> GlobalDecl t
  TypeAliasDecl :: TypeAliasBody -> GlobalDecl Typed
```

We actually want `TypeAliasDecl` to work with *any* of `PartiallyTyped`, `Typed`, or `TypeChecking`. Can we make this work? Yes, with a type class constraint:

```haskell
class IsTypedIsh (t :: Typing)

instance IsTypedIsh Typed
instance IsTypedIsh PartiallyTyped
instance (Unsatisfiable msg) => IsTypedIsh Untyped

data GlobalDecl :: Typing -> Type where
  FunDecl       :: FunBody t     -> GlobalDecl t
  DataDecl      :: DataBody t    -> GlobalDecl t
  TypeAliasDecl :: (IsTypedIsh t) => TypeAliasBody -> GlobalDecl t
```

But, uh oh, we also want to write *functions* that can operate in many of these states. We can extend `IsTypedish` with a function witness `witnessTypedish :: WhenTyped t Type -> Type`, but that also doesn't quite work - the `t` actually determines the output type.

```haskell
class IsTypedIsh (t :: Typing) where
  type TypedIshPayload t 
  isTypedIshWitness :: WhenTyped t Type -> TypedIshPayload t

instance IsTypedIsh Typed where
  type TypedIshPayload Typed = Type
  isTypedIshWitness (Some a) = a

instance IsTypedIsh PartiallyTyped where
  type TypedIshPayload PartiallyTyped = Maybe Type
  isTypedIshWitness (Optional a) = a

instance IsTypedIsh TypeChecking where
  type TypedIshPayload TypeChecking = Either TypeError Type
  isTypedIshWitness (Checking a) = a

instance (Unsatisfiable msg) => IsTypedIsh Untyped
```

Now, this does let us write code like:

```haskell
inputHasTypeSorta :: (IsTypedIsh t) => GlobalDec t -> _
```

but actually *working* with this becomes a bit obnoxious. You see, without knowing `t`, you can't know the *result* of `isTypedIshWitness`, so you end up needing to say things like `(IsTypedish t, TypedIshPayload t ~ f Type, Foldable f) => ...` to cover the `Maybe` and `Either` case - and this only lets you fold the result. But now you're working with the infelicities of type classes (inherently open) and sum types (inherently closed) and the way that GHC tries to unify these two things with type class dispatch.

*Whew*.

Meanwhile, in parametric polymorphism land, we get almost all of the above *for free*. If we want to write code that covers multiple possible cases, then we can use *much* simpler type class programming. Consider how *easy* it is to write this function and type:

```haskell
beginTypeChecking 
    :: GlobalDecl () 
    -> GlobalDecl (Maybe (Either TypeError Type))
beginTypeChecking = fmap (\() -> Nothing)
```

And now consider what you need to do to make the GADT program work out like this.
