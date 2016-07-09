---
title: "Programming Lambda I: Parsing"
date: 
layout: post
categories: programming
---

I don't know about you, but I really like programming languages!
I especially like functional languages.
The lambda calculus is a really simple and elegant way to express computation.
Let's make a programming language based on the lambda calculus, implemented in Haskell!

I'm no expert in any of this.
I'm going to use this blog series to document what I'm doing, and how I learn.
We're going to be writing tests, QuickCheck properties, and hopefully using modern Haskell best practices.
If I've screwed something up, please send an email or comment on the GitHub repository.

# Parsing

The first thing I want to do is get our parser running.
That way, we can build on this incrementally, and will be able to enter source text rather than dealing directly with ASTs.

One of the more common ways to write a parser in Haskell is to use a parser combinator library.
Parser combinators are neat!
And `megaparsec`, a fork of the venerable `parsec` with bug fixes, performance improvements, and other updating is an excellent example.

First, we define the type of our abstract syntax tree:

```haskell
data Lambda = Var Text | App Lambda Lambda | Abs Text Lambda
```

Now let's describe our syntax using the libraries combinators.

```haskell
import Text.Megaparsec
import Text.Megaparsec.Text
import Data.Text (Text)
import qualified Data.Text as T

identifier :: Parser Text
identifier = T.pack <$> some alphaNumChar

variable :: Parser Lambda
variable = Var <$> identifier

application :: Parser Lambda
application = between (space *> char '(')) (space *> char ')') $ do
        space
        l <- lambda
        space
        r <- lambda
        space
        return (App l r)
    
abstraction :: Parser Lambda
abstraction = between (space *> char '(')) (space *do
    char 
```

## Parsing


The above specification is the completely unambiguous grammar, which has members like:

```
# id
(\\ x . x)
# const a b
(((\\x . \\y . x) a) b)
```

This is kind of gross! All of those parentheses make it really annoying to read and write.
We'd really prefer something like Haskell's lambda syntax, where application is whitespace.
Let's write a grammar that follows that.

## BNF
```
Lambda := Variable | Application | Abstraction | ( Lambda )

Application 
   := Variable
    | Variable Application
    | Variable (Lambda)

Abstraction := \\ Variable . Lambda

Variable := [a-zA-Z]+
```

This captures what we want.
We can now use parentheses to make terms more explicit (the meaning of the `( Lambda )` rule), and the tricky recursive bits in the application case give us the behavior we want there.
Now, this grammar is pretty inefficient.
Look at how many common prefixes there are!

```
Lambda = Variable | AppTo App | Abstraction

Variable = [a-zA-Z]+

Abstraction ::= \\ Variable . Lambda

AppTo := Variable | (Abstraction) | AppTo App

App ::= Variable | (Abstraction) | (AppTo App)
```
