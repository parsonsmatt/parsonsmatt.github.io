---
title: "Object Oriented Programming in Haskell"
date: 2017-02-17
layout: post
categories: programming
---

# Introduction

Well, here you are.
You drank the kool aid and learned Haskell.
You overcame all the novelties and differences from imperative programming, figured out how to use monoids and monads, and wrote yourself a couple dozen parsers.
Now it's time to implement some Real World Program, and you're a little lost.

> Haskell is *so* different from object oriented programming that you must unlearn all of that knowledge to learn the Functional Paradigm

Perhaps you've heard this a few times.
Well, it's true -- Haskell has different idioms than OOP.
It's also unhelpful.
If you're thinking, "Oh, I know, I'll use a factory to select the right instance," then being told that "factories don't real in Haskell" doesn't help you.

I had a conversation with a coworker where he asked:

> Does Haskell have inheritance?

To which I responded:

> No.

This was deeply unsatisfying to me, because I didn't get to actually help him accomplish his goal.
This blog post series is my attempt to address this and similar questions.

# Haskell's XY Problem

The XY problem (described in much greater detail in [this Stack Overflow post][xy-problem]) is what happens when you ask about how to implement your chosen solution, rather than ask about how to solve your given problem.

People that get started building software in Haskell tend to ask XY questions.
Here are a few common ones that I've seen:

- "Does Haskell have inheritance?"
- "How can I have a heterogeneous list?"
- "How can I use mocks or spys to make testing easier?"

The answer to all of these is usually "don't" or "no," but instead of just saying "no," I'd like to cover common features in OOP and how Haskell developers accomplish the same goals.

# What is OOP?

"Object oriented programming" is about as controversial of a term as "functional programming."
My OOP instruction was mostly through reading and watching [Sandi Metz][sandi-metz], [Gary Bernhardt][gary], and the surrounding Ruby community, which is heavily inspired by Smalltalk.
Sandi Metz's excellent book [Practical Object Oriented Design in Ruby][poodr] is a fantastic guide to writing good OOP code.

The core of object oriented programming is the object.
An object has many metaphors: cells in biology, actors, messengers, etc.
Objects are:

1. Data -- they hold on to some internal state, which may be hidden
2. Behavior -- they respond to messages (or methods) by updating their internal state, issuing side effects, and/or returning a value.

There's a lot more to most OOP -- interfaces, classes, inheritance, traits, modules, mixins, overriding, namespaces, abstract classes, etc.
But most of those features can be expressed in terms of simpler ideas.
To maximize the applicability, I'll try to stay as simple in OOP terms as possible, so you can use this information regardless of which OOP language you're most familiar with.

For the up-to-date table of contents, check out my [tutorials](/tutorials/) page.

[xy-problem]: http://meta.stackexchange.com/questions/66377/what-is-the-xy-problem
[sandi-metz]: https://www.sandimetz.com/
[poodr]: https://www.sandimetz.com/products#product-poodr
[gary]: https://www.destroyallsoftware.com/screencasts
