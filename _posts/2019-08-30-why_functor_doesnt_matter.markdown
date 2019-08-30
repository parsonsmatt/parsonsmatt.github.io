---
title: "Why 'Functor' Doesn't Matter"
date: 2019-08-30
layout: post
categories: programming
---

Alternative, less click-baity title: Names Do Not Transmit Meaning

People often complain about the *names* for concepts that are commonly used in Functional Programming, especially Haskell.
Functor, monoid, monad, foldable, traversable, arrow, optics, etc.
They're weird words!
`Functor` comes from category theory, `Monoid` comes from abstract algebra.
`Arrow` comes from -- well it's just kind of  made up!
`Optics`, lenses, prisms, etc are all somewhat strange metaphors for what's going on.
What's the deal?
Why can't they just pick simple names that mean what they are?
Why can't they use practical and ordinary terms, the way that Object Oriented Programming does?

Some people strengthen their complaint with moral urgency: `Functor` is a confusing word,

- *and* it makes it more difficult for people to learn Haskell, (it doesn't)
- *and* this makes Haskell an elitist, non-inclusive language, (it is, but this isn't why)
- *and* if we just used "my favorite term" instead, it wouldn't be a problem! (sorry but your favorite term betrays a misunderstanding of the concept)

So, let's talk about names.
What are they?
What do they do?
How do they matter, and why?

# What's in a name?

My name is Matthew Parsons.
If you google "Matthew Parsons", you'll see a bunch of footballers, a doctor, a professor, a radio personality.
My blog is the last entry on the second page of Google results for my name, which I'm pretty proud of.

My name isn't globally unique - there are a lot of Matt Parsons running around.
Many of them think that they have my email, and sign me up for all kinds of silly stuff (and some serious stuff, too).
If you further qualify the name - by appending 'Haskell' to the search query - then you get a ton of stuff that points to me.
I appear to be the most prominent Haskell programmer named Matt Parsons (for now).

If I'm in a group of folks, and you say the word "Matt," I'm going to assume you're trying to get my attention.
Unless there's another Matt in the group, at which point you'll probably say "Matt Parsons" or similar to disambiguate.
I'm about to go on a bikepacking trip with two other Matts.
I suspect my first name will be dropped entirely on this trip.

What does my name tell you about me?
Almost nothing.
I'm an English speaking male, probably of British descent.
But it doesn't tell you that I like kittens, that I like Haskell, that I like to ride bikes, or that I dislike the color red.
It's merely an imperfect, globally duplicated pointer.

# What's a name good for?

It's a pointer with an ambiguous address space.
It's a key in a nondeterministic map.
It's a database ID column with an index, but not a unique index.

They're bad!
They don't scale, at all.
I think of a concept, I say a word, and hopefully this points to the same concept in your brain.
We use names as shortcuts for communicating common concepts.
If we need to learn more about a concept, we can look up the name and try to find relationships to other names.

Names can't transmit meaning.
They just point to concepts.
Concepts must be explained and understood, usually in terms of a large quantity of simpler or more familiar names.
If we want a name to fully describe the concept it points to, then it must be a very simple concept indeed.

# How can we judge a name?

Names can't transmit meaning, and so a name shouldn't be judged on how well it transmits meaning.
That doesn't mean that names can't be judged at all - there are good and bad aspects to names.

- How reliably does it point to the right concept?
- How memorable is it? 
- How easy is it to pronounce (for the language it originated in)?
- How aesthetically appealing is it?

The last three are pretty subjective - I used to find it difficult to remember the difference between `Monoid` and `Monad` because they both have the form `mon*d`, and because people kept saying things like "Monads are monoids for functors."
I find a word like 'illuminate' pretty and 'buzzfeed' gross, which is totally just because I am weird and have opinions about this.

Reliability is also subjective.
It all depends on context and familiarity.
It's essentially impossible to have fully unique names for ideas - even if you pick something totally novel, someone else can come along and use your unique name for a totally different concept.

`Functor` gets picked on a lot, so let's look at that.
The word [functor](https://en.wiktionary.org/wiki/functor) has three meanings, one in linguistics, one in object oriented programming, and one in category theory.
This is pretty good - only three collisions, and it is usually pretty clear what you mean based on context clues.

The best (but still extremely bad) alternative name to `Functor` is `Mappable`.
It's the best alternative because it is the *least* misleading - I've seen people suggest `Iterable`, `Streaming`, `Container`, `Lift`, and they're all dramatically more misleading.

The [Wiktionary page](https://en.wiktionary.org/wiki/mappable) relates it to maps, suggesting that it means you "can make a geographical map of a thing," or that you can construct a "mapping" between two sets of things.
Grammatically, it implies that you can use a verb "map" over the thing.

So let's look at the [Wiktionary entry for 'map'](https://en.wiktionary.org/wiki/map).
At a first glance, it's a way bigger page than `Functor`.
There's a common understanding: geographic maps, like you use to navigate a new city.
The next most common understanding is more abstract:

> A graphical representation of the relationships between objects, components or themes.

Third definition is from math, and makes it a synonym for 'function'.
But that's not quite right - after all, `Mappable x` implies "I can map x," and you can apply a function to any value in Haskell.
So that doesn't really give any additional clarity.

The other meanings are completely out-of-bounds, and it's unlikely that someone would be confused.
This name, `Mappable`, points to a bunch of potential meanings already, and none of them are really right.
So we can create a new meaning that `Mappable` points to, and hope that people infer the right one.

But we still have to *explain* what a `Mappable` is.
"What's a `Mappable`? Well, it's something you can `map` over!" is a terrible explanation!
It's literally just the grammatic expansion of the word.
All it does is move the question one bit further:

> What does it mean to be able to `map` over something?

Wait, "map over"? This is unfamiliar terminology.
I know about "maps" like Google Maps.
I know that I can 'map' a space out and provide information about how to get from here to there.

You might think that because you already learned `map` from JavaScript or Python that it's a good enough name.
But that's an argument from familiarity.
Rubyists and Smalltalkers are more familiar with the name `select` for this operation.
If they want to call it `Selectable`, then who are we to stop them?

Fact is, "mapping" isn't an easy concept, no matter what you call it.
We could call it "florbing" and it would require the exact same amount of instruction and understanding for the concept to work out.

# So what makes a name bad?

Names can't transmit *meaning*.
They can transmit a pointer, though, which might point *to* some meaning.
If that meaning isn't the *right* meaning, then the recipient will *misunderstand*.
Misunderstandings like this can be difficult to track down, because our brains don't give us a type error with a line and column number to look at.
Instead, we just feel confused, and we have to dig through our concept graph to figure out what's missing or wrong.

Object Oriented Programming is *littered* with terrible names, precisely because they *mislead* and cause a false familiarity.
Object, Class, Visitor, Factory, Command, Strategy, Interface, Adapter, Bridge, Composite.
All of these are common English words with a relatively familiar understanding to them.
*And all of them are misleading*.

"Object" is possibly the most reasonable name choice - the English word 'object' just refers to any random physical thing, or grammatically speaking, the target of a subject - something we act upon.
After that, it's misleading names causing confusion.

What's a class?
It's a blueprint for objects!
Why not call it an `ObjectBlueprint`?
Uhhh... And how does that relate to static class variables, and other attributes of classes?

What's the visitor pattern?
What does it mean for my code to "visit" another piece of code?
You need to understand the abstract meaning of these words before "visitor pattern" means anything to you.

What's the difference between an interface, a bridge, and an adapter?
These three terms are all idioms for the same sort of concept in English, but they have rather different precise meanings in programming.

`Monad` - now that's a name that didn't mean *anything* to me when I first read it.
I read it, and I immediately knew that I didn't understand the underlying concept.
At the time, I was so tired of reading familiar words, assuming they meant something that I understood, and stepping on abstract landmines that betrayed my lack of understanding.
`Monad` was a breath of fresh air.
A new concept, and a new name to go with it!

Concepts are hard.
Names don't make them any easier or harder to understand.
Names are only useful in their value as pointers, and to establish relationships between concepts.

`Functor` is hard to learn.
It is not hard to learn because it is named `Functor`.
If you renamed it to anything else, you'd have just as hard of a time, *and* you'd be cutting off your student from all of the resources and information currently using the word "functor" to refer to that concept.
