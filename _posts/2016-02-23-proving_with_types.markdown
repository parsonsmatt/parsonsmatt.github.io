---
title: "Proving With Types"
date: 2016-02-23
layout: post
categories: programming
---

I think that the Curry Howard correspondence is one of the coolest things *ever*.
Philip Wadler's ["Propositions as Types"](http://homepages.inf.ed.ac.uk/wadler/papers/propositions-as-types/propositions-as-types.pdf) paper sends chills down my spine.
Getting to _literally_ reason about the programs we write and use ~Logic~ on them is fascinating and amazing to me.
So what does this all mean?
Is it practical?
Can we do anything with it?

# We can!

(please clap)

Here's the basic idea: when we write a type signature in our programs, we're being tricked into writing a proposition in logic.
When we write an implementation for it, we're providing evidence for that proposition, and potentially proving it.
The more powerful our type system, the more logically we can think about it.

Let's take Java for example.
Here's a method signature:

```java
public Integer length(List<Character> string);
```

This signature is like a promise: "If you give me a `List<Character>`, then I'll give you an `Integer`."
We can write that in propositional logic as:

$$List_{Character} \implies Integer$$

Now, in order to *prove* this, we must demonstrate that this works *for all* lists of characters.
If someone can provide a `List<Character>` that causes this function to break, then our proposition is false!
Fortunately, lists are simple, and even Java's `null` isn't terribly awful here.
Here's our proof:

```java
public Integer length(List<Character> string) {
    if (string == null) {
        return 0;
    }

    Integer result = 0;

    for (Character c : string) {
        result += 1;
    }

    return result;
}
```

Actually, we can provide a proof much more simply: `return 0;` satisfies the promise in the type signature!

It's a little easier to see the connection with types and logic with Haskell's syntax. Compare these two declarations:

```haskell
length :: [Char] -> Integer
```

$$List_{Character} \implies Integer$$

There's even an arrow!

# Proving Something False

We can use this sort of logic to prove things false, too.
Let's say you want to prove ONCE AND FOR ALL that the Haskell function `head :: [a] -> a` function is just bad.
It's not OK and you want to remove it from your codebase, and maybe even get a ruckus going on the Haskell mailing list.
The idea that throwing exceptions in pure code is bad hasn't been cutting it at the company meetings, so we'll have to resort to logic.

First, we need a way to translate some ideas to get at their logical counterpoints.
How are we going to represent the idea of a linked list in logic?
Our tools in logic are `and` ($\land$), `or` ($\lor$), and `implies` ($\implies$).
We've already seen how `implies` corresponds to functions, so now we just need to figure out `or` and `and` and we can do some THEOREM PROVING.

## And

$A \land B$ is true if $A$ is true and if $B$ is true.
If either of them are false, then the proposition $A \land B$ is false.
Logical `and` has a few rules that we can look at:

### And Introduction

If $A$ is true and $B$ is true, then $A \land B$ is true.
That means whenever we know $A$ and $B$, we can introduce $A \land B$.
This theorem looks like:

$$A \implies (B \implies A \land B)$$

What's a Java method signature that corresponds with this?

```java
public <A, B> And<A, B> andIntroduction(A a, B b);
```

Or Haskell:

```haskell
andIntroduction :: a -> b -> And a b
```

### And Elimination

If $A \land B$ is true, then we can eliminate the $\land$ and write down both $A$ and $B$ on their own.
As a logical proposition, this looks like:

$$A \land B \implies A$$
$$A \land B \implies B$$

Which we can implement in our two favorite programming languages:

```java
public <A, B> A andElimOne(And<A, B> and);
```

```haskell
andElimOne :: And a b -> a
andElimTwo :: And a b -> b
```

Given the rules and type signatures for and elimination, I think we can arrive at a suitable class to implement it.
It's the humble pair, or tuple!

```java
public class <A, B> Tuple<A, B> {
    public final A fst;
    public final B snd;
    public Tuple(A a, B b) {
        this.fst = a;
        this.snd = b;
    }
}
```

```haskell
type And a b = (a, b)
```

## Or

The proposition $A \lor B$ is true if either of $A$ or $B$ are true.
If one is false, that's fine! As long as they both aren't false.
Now let's review the rules for `or` ($\lor$).
This one will be a little trickier.

### Or Introduction

If we know $A$ is true, then we can say that $A \lor B$ is true.
Even if we *know* that $B$ is false, we can still say $A \lor B$ since $A$ is true.
Expressed as a theorem, we have:

$$A \implies A \lor B$$

Which gives us a Java method signature:

```java
public <A, B> Or<A, B> orIntroduction(A a);
```

and a Haskell type signature:

```haskell
orIntroduction :: a -> Or a b
```

Naturally, we can also write `b -> Or a b`.

### Or Elimination

Or elimination is a way of rewriting an `Or` value.
It's a bit trickier than `And` elimination, since in `And` elimination we know we've got an A and a B.
If we know that $A \lor B$ is true, then we have two possible cases: either $A$ is true, or $B$ is true.
But we don't know which!
So we'll have to able to handle either case.

So, provided we know how to handle $A \implies Q$ *and* $B \implies Q$, then we can do some elimination.
Because a full type signature might get messy, here's a list of "requirements" and a guarantee:

- If we can handle an $A$: $A \implies Q$
- If we can handle a $B$: $B \implies Q$
- If one is true: $A \lor B$
- Then I can give you a $Q$

Written out, this is:

$$(A \lor B) \implies (A \implies Q) \implies (B \implies Q) \implies Q$$

Translated to Java:

```java
public <A, B, Q> Q orElimination(Or<A, B> or, Function<A, Q> left, Function<B, Q> right);
```

In Haskell, we've got:

```haskell
orElimination :: (Or a b) -> (a -> q) -> (b -> q) -> q
```

### wat

So we know we can make an $A \lor B$ from either an $A$ or a $B$.
And we know that we can eliminate it, but only if we can handle either an $A$ or a $B$.
This is Haskell's `Either` type:

```haskell
data Either a b = Left a | Right b
type Or = Either
```

Implementing Either in Java is left as an exercise for the reader.

# Logically Sound

Alright, we've reviewed our logic and got some tools on our belts.
Let's get back to lists, and how much we *hate* the `head` function.
The Haskell list type is defined like:

```haskell
data List a
    = Nil
    | Cons a (List a)
```

Now, we need to translate this to *just* using `Or` and `And`.
We can start by deconstructing the `|` in the top level sum and replacing it with an `Either`.
Since the `Left` value is a nullary constructor, we can say that the end of a list is like `Left ()`.

```haskell
type List' = Either () (...)
```

That leaves the other case.
We've got a constructor `Cons a (List a)`, which has two elements.
We can express that as a tuple: `(a, List a)`.
Haskell doesn't let you have cycles in type synonym declarations, so we can't *actually* say:

```haskell
type List a = Either () (a, List a)
```

But we can imagine that this is the case.
Now that we have it in terms of `Either` and `(,)`, we can write it as a logical proposition:

$$List_{A} = () \lor A \land List_{A}$$

Now, we can plug that directly in to the type signature for `head`, which we'll put here in logical form:

$$head : List_{A} \implies A$$

Replacing $List A$ with the definition of list:

$$(() \lor A \land List_{A}) \implies A$$

Now we can use the `and` elimination rule as listed above to get rid of the $List_{A}$ term:

$$() \lor A \implies A$$

We can rewrite propositions with an implication using the following pattern: $P \implies Q \equiv \neg P \land Q$.
So we can remove the $\implies$ in the above, replacing it with:

$$\neg (() \lor A) \lor A$$

There's a neat trick called [De Morgan's Law](https://en.wikipedia.org/wiki/De_Morgan%27s_laws) which lets us move the negation inside of the parentheses.
Note that, in classical logic, all four of the laws hold.
When talking about type theory, only three do!
However, we're using one of the ones that are still right, so we're good to go.

$$\neg () \land \neg A \lor A$$

Finally, we can do the last `and` elimination, and boil this down all the way.

$$\neg A \lor A$$

Classical logic includes an axiom called [The Law of Excluded Middle](https://en.wikipedia.org/wiki/Law_of_excluded_middle).
This law allows us to assume that $A$ is true if $\neg A \lor A$.
Intuitionistic logic -- which tries to make logic from the bare minimum -- is unable to prove this, and does not accept it as an axiom.

Classically, we'd read that:

> Either A is true, or Not A is true.

and if you accept that $A$ can be either true or false, that seems like it makes sense.

The intuitionistic version of logic instead reads that like:

> I have a proof that:
> Either I have evidence that A is true, or I don't have evidence that A is true.

and this also makes a lot of sense: if you don't know whether or not you have evidence for A's truth, then you can't assume that A is true!

# Victory!

We've achieved a minor victory today.
When functional programmers talk about 'reasoning about their code', this is one of the things we're talking about.
Like, literal, actual, _reasoning_.
Not just 'thinking', but logical, clear, structured reasoning about how the code is working and what it means.

If you're interested in learning more, I'd highly recommend reading [Type Theory and Formal Proof: An Introduction](http://www.amazon.com/Type-Theory-Formal-Proof-Introduction/dp/110703650X).
