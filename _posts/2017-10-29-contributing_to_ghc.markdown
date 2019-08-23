---
title: "Contributing to GHC"
date: 2017-10-29
layout: post
categories: programming
---

UPDATED 2019-08-22: See the end of the post for how this turned out.

This post serves as notes and explorations of my first patch to GHC.
I'm going to start from the very beginning -- so it might be kind of boring!

# Get on that documentation

First thing's first: [check out the documentation](https://ghc.haskell.org/trac/ghc/).
It was a little overwhelming at first, until I noticed the [Newcomer's info](https://ghc.haskell.org/trac/ghc/wiki/Newcomers) link.
This one is much easier to get started with -- has all the directions you need to get GHC building!
I ran the commands, GHC downloaded and built, and soon all was ready to go.

# Pick a Ticket

The next thing to do is to pick a ticket to implement.
The Newcomer's Info page has a list of newcomer friendly tickets.
[This ticket](https://ghc.haskell.org/trac/ghc/ticket/12389) seemed relatively easy -- a small change to the parsing rules to allow trailing and leading commas to data constructor export lists.
I made a comment indicating that I would try to make a patch for it.

# Explore the source code!

This was fun -- GHC has a *huge* codebase.
Fortunately, the parser was relatively easy to locate.
There's a directory `compiler` that has the compiler, and `parser` is located right under there.

As an aside:
I use FZF to fuzzy locate files in projects via [fzf.vim](https://github.com/junegunn/fzf.vim).
This lets me open vim up, type some barely coherent garbage in, and mostly find what I am looking for.
I do this as my primary means of exploring a code base -- I'll whack `<leader>e` which lets me fuzzy search file names.

The Haskell language parser uses [Happy, the parser generator for Haskell](https://www.haskell.org/happy/).
I used Happy briefly when working on the Appel compiler book, but have otherwise never used a parser generator.

# Implement a test!

How do testing!?
I used my `FZF` trick from above and did a fuzzy search for `testparse`.
This showed me that there was a `testsuite` directory that included a `tests/parser` directory.
There's a `testuite/README.md` file that has more information on how tests run, and also a [link to the official test suite documentation](http://ghc.haskell.org/trac/ghc/wiki/Building/RunningTests).
That page included a [link on how to add a test](https://ghc.haskell.org/trac/ghc/wiki/Building/RunningTests/Adding).
I ran the test suite at first, and -- holy crap -- it takes a LONG time to run the entire GHC test suite!
So I killed it, reread the README, found that you can run just a section of a test, and then did that.

I followed the directions to add a test and came up with [this commit](https://github.com/parsonsmatt/ghc/commit/b3544c708d73ea42af5468814fceffb99dd844d6).
The test failed, so I'm good to go.

# Write a fix!

Now, it's time to get that test passing.
I'm not super familiar with parser combinators, so I tried stuff until the test passed.
The test suite readme had instructions on running a single test with `make TEST=T12389` which I gladly took advantage of.
To make compiling things faster, you can `cd` into the relevant directory and run `make`.
For the parser, that's `compiler`.

That work ended up with [this commit](https://github.com/parsonsmatt/ghc/commit/97561e566b1524a971ab4511ce26b6c8623438b4).
I made the test example a little bigger to test for more stuff.

# Make a PR!

This is where things get Weird, if you're like me and you started getting into open source after GitHub had essentially established world dominance.
GHC is the first project I've ever worked on that was hosted on a non-GitHub (or GH-like site, like BitBucket or GitLab).

But, before I get too excited, I look at the instructions for [how to submit a patch to GHC](https://ghc.haskell.org/trac/ghc/wiki/WorkingConventions/FixingBugs).
After making the commits, it asks to "validate the commits" using [Travis or a validation script](https://ghc.haskell.org/trac/ghc/wiki/TestingPatches).

# Er, Validate Commits!

Looks like I can just run `./validate` in the GHC repo.
Except that gets me a `sphinx-build` not found error.
Fortunately, that's easy to fix by installing it via apt-get.

`./validate` then proceeds to run for a VERY long time.

... Still running...

Ok, at this point I got impatient.

# Make a PR, again!

[The directions for submitting a patch using Phabricator](https://ghc.haskell.org/trac/ghc/wiki/Phabricator) are pretty great.
I ran the relevant commands, and now [my patch is on the website](https://phabricator.haskell.org/D4134).
Neat!

At this point, it's time to wait for code review, implement any requested changes, and then pat myself on the back for contributing to GHC.
You can do it, too!

# UPDATE: 2019-08-22

It's been almost two years since this whole process got started.
The contribution did not land. 
Here's what happened.

[`hvr` left a comment](https://phabricator.haskell.org/D4134#115719) indicating that this had been thought through, and it would need a much larger investment of time and energy.
I was told to make a GHC proposal so that it could go through the official process.
So, [I made an official GHC proposal](https://github.com/ghc-proposals/ghc-proposals/pull/87).
The proposal discussion ballooned to 166 comments at the time of writing this update.
Here's my summary:

- Matt: Here's a proposal!
- Community: This is good, but it must be expanded to cover all cases of comma-separated enumerations.
- Matt: OK, here's the updated proposal. There's an interaction with `TupleSections`, though. How should we handle it?
- Community: bikeshedding about syntax for about 1.5 years
- Committee: Matt can you address concerns/questions?
- Matt: I've addressed everything as well as I can, as far as I know it's just up to a vote.
- Committee: Matt can you address concerns/questions?
- Matt: Can you be more specific about the questions/concerns? 
- Committee: 
- Committee: 
- Committee: Matt can you address concerns/questions?
- Matt: OK, so there are two options for this: 

  1. Make the extensions `ExtraCommas` and `TupleSections` completely incompatible.
  2. Excluse tuples from `ExtraCommas`.

  Let's pick one and get on with this. I want this off my plate so if you haven't decided in a week then I'm closing it out.
- Committee: 
- Committee:
- Matt: OK, closing it out.
- Committee:
- Committee: Okay, we've decided to accept it!
- SPJ: Wait, hold on, I thought we were voting on the first proposal and I hadn't actually read what we were voting on! What if...

And so I will not be contributing to GHC until they've significantly improved their processes for newcomers.
