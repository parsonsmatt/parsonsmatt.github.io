---
title: "Capability and Suitability"
date: 2018-11-03
layout: post
categories: programming
---

Gary Bernhardt has a fantastic talk on [Capability vs Suitability](https://www.youtube.com/watch?v=NftT6HWFgq0), where he separates advances in software engineering into two buckets:

- Capability: The ability to do new things!
- Suitability: The ability to do things well.

Capability is progressive and daring, while suitability is conservative and boring.
Capability wants to create entirely new things, while suitability wants to refine existing things.

This post is going to explore a metaphor with bicycles, specifically bike tires, while we think about capability and suitability.
When you get a bike, you have so many options.
Tire size is one of them.
You can opt for a super narrow road tire -- a mere 19mm in width!
Or, on the other end of the scale, you can opt for a truly fat tire at around 5" in width.
What's the difference?

Narrower tires are less capable -- there is less terrain you can cover on a narrow tire.
However, they're more suitable for the terrain they can cover -- a 19mm tire will be significantly lighter and faster thana 5" tire.
A good 19mm tire weighs around 200g, while a 5" tire might weigh 1,800g each.
Lugging around an extra 7lbs of rubber takes a lot of energy!
Additionally, all that rubber is going to have a lot of rolling resistance -- it'll be harder to push across the ground on smooth surfaces where the 19mm tire excels.

So, most cyclists don't use fat tire bikes.
But they also don't use 19mm skinny tires.
Most road cyclists have moved up to 25 or 28mm tires.
While the 19mm tires work fantastically on a perfectly smooth surface, they start suffering when the road gets bumpy.
All the bumps and rough surfaces call for a slightly more capable tire.
The wider tires can run lower air pressure, which lets them float over bumps rather than being bumped up and down.

So, we have two competing forces in bike tires:

- The speed and comfort on the terrain you ride most frequently
- The speed and comfort on the worst terrain you encounter regularly

You want enough capability to handle the latter, while a tire that's suitable for the former.

In computer programming, we tend to reach for the most capable thing we can get our hands on.
Dynamically typed, impure, and Turing complete programming languages like Ruby, JavaScript, and Python are immensely popular.
Statically typed languages are often seen as stifling, and pure languages even more so.
There simply *aren't* many languages that are Turing incomplete, that's how little we like them!

Yet, these extra gains in capability are often unnecessary.
There's very little code that's difficult to statically type with a reasonable type system.
Impurity seems convenient, until you realize that you need to look at every single method call to see *why* the code that renders an HTML page is making an N+1 query and ruining performance.
Indeed, even Turing completeness is overrated -- a Turing incomplete language permits dramatically more optimizations and static analysis for bug prevention, and very few programs actually require Turing completeness.

In this sense, programmers are like cyclists that pick up the 5" tire fat bikes and then wonder why they're moving so slow.
They may ride in the snow or deep sand once or twice a year, and they stick with the 5" tire for that reason alone.
Programmers that are willing to give up the capability they don't need in order to purchase suitability they could use tend to go faster, as you might expect.
Folks that learn Haskell and become sufficiently familiar with purely functional and statically typed programming tend to take those practices with them, even in impure or dynamically typed languages.

It is easier to understand what you did when you limit what you can do.
