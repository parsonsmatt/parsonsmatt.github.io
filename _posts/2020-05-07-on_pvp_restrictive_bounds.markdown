---
title: "On PVP + Restrictive Bounds"
date: 2020-05-07
layout: post
categories: programming
---

The following discussion occured on the [FPChat Slack (invite link)](https://fpchat-invite.herokuapp.com/).
It was pasted into a gist by [Emily Pillmore](https://twitter.com/pitopos).
However, the gist was mysteriously deleted.
I offered to host the interesting discussion on my blog, which will hopefully preserve it.

The gist was recovered from Google Cache and is reproduced here.
Some formatting and emojis are probably wrong because I don't have the original markdown.

---

## On PVP + Restrictive Bounds

The prompt came from a tweet (later found out to be ranting into the void and not to be considered real advice) by a Stackage trustee:

> Hey Haskell developers! STOP clap USING clap RESTRICTIVE clap UPPER clap BOUNDS! clap At least stop using them by default!

### Part I

C: What do people consider the best practices on upper bounds in packages?

L: No build failures / No restrictive upper bounds. Pick one. (And stackage only works as a third alternative as long as you can afford to stay within it.)

П: @C This is why PVP is so important. Having upper bounds to within the next major version are good enough to guarantee that your bounds will not get in your way until you absolutely need them to, and will save you from committing to potential API breakages. As long as everyone follows PVP, you will not see either semantic or api breakages within a major version. However, there are bad actors using Hackage who will not follow this protocol, and do their own thing. This is bad for everyone, and a few missteps in major packages have made Stackage people paranoid about enforcing builds as a source of truth. Rather than becoming more vigilant or chalking it up to human error, Stackage folks recommend no one use upper bounds, and we just all run all plans against all new package versions to pick one build plan per cycle , instead of a potential range cabal offers. However, builds are not a source of truth. They are just proof of compilation! The package that maintains the same api, has no tests, and redefines every definition to be undefined still would pass build in Stackage!

1. This is a waste of resources, and a combinatorially explosive algorithm you can only accomplish with lots of boxes.
2. It asks everyone to commit to potential API breakages.
3. It still doesn't solve the problem of semantic breakages, and if a particular package has a downstream package set that lacks tests for a certain breaking change in semantics semantics, all of its downstream dependencies are now wrong for the user, despite building
4. If any of these problems appear in Cabal, i can revise my bounds and commit a revision. Stackage does not like revisions.
5. Some like to complain about a mythical Solver creating build plans that don't work. This is not the case if everyone follows PVP, and I've still never seen evidence to support this. It's probably an artifact of the Cabal 1.0 era that has long been out of date.
6. I think in the end, J is complaining about the work he's found himself doing as a result of a bad maintainer not following semantic versioning, which is a problem in any system.

For further reading, there's

* https://github.com/haskell/pvp/blob/master/pvp-faq.md
* https://www.yesodweb.com/blog/2015/09/true-root-pvp-debate 
* https://www.reddit.com/r/haskell/comments/3ls6d4/stack_and_the_pvp/cv92v6k/

L: Following PVP alone is not sufficient, you also need people to avoid breaking APIs to not bump major versions. (Just to be clear, I've been guilty of this too, "don't do it" is easier said than done)

J: I’m complaining about the work I find myself doing as a result of the impedance mismatch between how the PVP dictates the way the world ought to be and the way that the people inhabiting this world actually follow it.

П: PVP is not entirely sufficient, sure, but to quote Λ [from the tweet]:

@J, Why? I’d much rather weaken bounds myself using allow-newer than get broken build plans. Build plan solver errors are much easier for me to debug and diagnose than compilation failures!
If you don’t like having to weaken bounds periodically, maybe break compatibility less often.
П: Or, just realize that Stackage and Hackage serve two different purposes, and that the Stackage set of needs should not be the default thing we prescribe to everyone. It breaks hackage, and forces us all to rely on an incredibly costly methodology and is (historically) affected by the same semantic issues when picking out build plans. Stackage got bitten by aeson as well, if you remember

C: One major downside to allow-newer on the stack side is it doesn't mean what it says, it means allow anything. Cabal has fixed that.

### Part II.

(Later)

Sockington: I think people who want to ignore upper bounds should use allow-newer: True in their cabal.project (or stack equivalent) instead of expecting the entire world to commit to api breakages.

Sockington: Frankly I'm surprised that this isn't already the default in stack, what with everything being managed by LTS.

L: that's doesn't solve the problem for Stackage maintainers though. which is that they get spammed with hundreds of packages with spurious bounds and the only thing they can do is wait for the packages to get fixed.

J: Yes, if I wasn’t complaining vacuously on Twitter I would have written a more nuanced blog post explaining the crux of the problem. IME, Haskell’s community has poor hygiene when it comes to managing version boundaries over the course of a package’s lifecycle. I would say that in the vast majority of cases, version bumps signifying breaking changes that cause lots of downstream build plan failures are completely spurious. Put another way: requesting that people abide by these rules carefully is an exercise in strict discipline across a diverse community of practitioners. Part of why I like using languages with static types is that I can offload more of what would otherwise be disciplinary enforcement onto the computer for automated checking.

J: @C: For a serious answer, I would say follow https://www.semver.org if you’re a library author, and follow a similar methodology to what SemVer proscribes if you’re a library consumer (e.g. ^1.0.0 == >= 1.0.0 && < 2.0.0)

ChrisPenciler: Or follow PVP since it's literally haskell's package versioning policy

J: Eh, AFAICT there’s nothing enforcing that and I don’t care for the PVP. If Hackage were to make a decision that the PVP was an actual standard for enforcement rather than a de facto one I’d be fine with it though.

П: the only thing enforcing PVP is the mass of historical packages that conform to it, and the inertia of a significant percentage of the Haskell community. To be fair, PVP came out before SemVer, and it's around for Hysterical Raisins. I'd like to see SemVer, but it's probably not an option at this point given the decade+ of packages following PVP. SemVer came out later, and is a refinement of PVP. But you can achieve roughly the same thing with PVP, which is fine. I just don't end up using the epochal first major version number! @J also, re: your ping about what's holding it up, the same thing that's holding a lot of Hackage infrastructure up: lack of people helping to work on it! If you or others want to dedicate some time to haskell infra, it'd be very appreciated. The Hackage servers need a lot of love, and i think there's only 2 very busy-with-life people working on it currently. One of which is going through a housing crisis and is AWOL atm. If there's one thing Snoyman has been great at, it's herding cats. The haskell.org folks, not so much besides Jasper and myself doing what we can to wrangle everyone to get things done. Anyone can pm me for more info about contributing their time to solving things for Hackage. We can get you set up and onboarded

Sockington: maybe the problem is complaining on Twitter thinking

M: I have a bunch of packages that i maintain and I pretty much alternate between strict upper bounds and YOLO upper bounds. strict upper bounds is much more work to maintain for a relatively small benefit. YOLO upper bounds require much less frequent work/maintenance, but the occasional times I need to do it, it's more annoying work. Both of the pain points could be automated with tooling but no one has written it yet.

Λ: The PVP is enormously better than semver. I don’t understand why anyone prefers semver to the PVP. They’re essentially the same, except that with semver, you only have one major version number, and with the PVP you have two. This means that under semver, any time you make any breaking change, you have to bump the major version number. When upgrading from version 1.x to 2.x, you have no idea how much effort it will be. Maybe the entire API has been overhauled and the package is completely rewritten, or maybe someone changed the behavior of a single, rarely-used function. Under semver, these are treated identically. Under the PVP, you have two major versions, so you can distinguish between “major API revisions” and “changes that could break someone’s stuff but probably won’t, or at least will be easy to fix.” This is such a huge boon, since it communicates far more information. Versioning is to some degree fundamentally a social problem, not a technical one, and I’ve personally found semver extremely frustrating here because it encourages people to either end up with absurdly large major version numbers or to release technically-breaking changes as minor version bumps because it seems so silly to bump the major version. My experience is that in ecosystems that use semver, versioning is less reliable, and I get more breakages when bumping minor version numbers than I do in Haskell. However, of course, this all flies out the window if people mysteriously choose to dogmatically flout conventions and upload packages to Hackage that use semver for no reason I can discern except spite.

One of the most frustrating things to me in this entire debate over versioning and revisions is the fact that, fundamentally, overly-loose constraints pollute the build plan solver forever if no revision is made to fix them. Even if subsequent versions of the package tighten the bounds, the solver will still see the older versions with incorrect bounds, and it will choose them and try a bad build plan instead of raising a solver error.

So you have a group of people making the following arguments:

* I don’t like restrictive upper bounds.
* I don’t like Hackage revisions.
* cabal-install sucks because it generates bad build plans. :shocked_pikachu:

J: To be fair Hackage revisions are terrible, and I cannot stand the argument that they’re fine because they don’t really introduce mutability or some other such nonsense.

Λ: Why are they terrible? If you don’t have them, a mistake in the constraints pollutes the solver forever. How do you avoid that otherwise? Just never screw up?

J: So you release a new version of your library, like every other programming ecosystem does!

Λ: I explained above why that doesn’t solve the problem.

M: I think a lot of it comes from those first two bullets being a legitimately large burden on library maintainers. So you really want some tooling around fixing that. The tooling we got was Stackage, which solved the problem by not having it in the first place. I think a more robust solution would involve lax upper bounds and tooling that can report build failures to a central service to serve as "official" upper bounds. Evidence-based bounds, rather than no-bounds or overly-conservative bounds

J: I also find the notion of a third-party being able to arbitrarily mutate metadata associated with packages to be extremely distasteful. And again, this is something that no other programming language ecosystem had to my knowledge, so why must we have it?

Λ: Okay, lets add two more bullets to the list:

* Weakening restrictive bounds is too much work.
* I don’t want someone else to weaken my bounds for me.

M: With no upper bounds, there's a small chance I'll need to make a revision to establish upper bounds on every version of a package that needs it. With restrictive upper bounds, there's a large chance that I'll need to make a revision to relax upper bounds on each version of a package. Either of these could be automated, but are both currently annoying to work with. I actually wonder which would be easier to write tooling for lol

J: Honestly though, I still don’t have a clear understanding of why Hackage is such a uniquely positioned package ecosystem that we need to implement things like revisions in the first place. To my knowledge no other package ecosystem has something like this in place, so what makes Haskell special in this regard?

Λ: I want to be very clear: if you dislike revisions, the only option is restrictive upper bounds. Otherwise you cannot revise a package to fix overly-lax constraints, and now your build plans for that package are polluted forever. You are fighting for two things that are mutually contradictory to the way a build plan solver works. Why don’t other ecosystems do this? I don’t know, but it means they’re broken in a way that we aren’t. Maybe other ecosystems just assume everyone will never screw up!

J: They don’t.

B: If your model requires mutation to fix potentially global poisoning of builds your model is bad and you shouldn't find a model that treats you better and buys you nice things

Λ: Do you know of any such model? Look, I am usually pretty empathetic when it comes to technical disagreements within the Haskell community; anyone who knows anything about me should know that pretty well. But this argument has always come off as magical thinking to me, with no proposed alternative that works. When I started arguing in favor of revisions, I had exclusively used Stack as a build tool, so I do not say this out of personal bias. The model you’re arguing for just doesn’t make sense.

B: Hackage and Cabal's problems are sui generis. So pick your poison.

J: As with all things, my argument here is that we should look at what Rust/crates.io is doing in this area and copy it wholesale.

L: how do they do it?

Λ: “Every other language does this broken thing, so we should do it too” is such a weird take coming from the Haskell community.

J: @L: Defaulting to upper bounds, community consensus on using semver, no mutation in the package index.

Λ: Cargo allows mutation via removing a package: https://doc.rust-lang.org/cargo/commands/cargo-yank.html. You need mutation somewhere. If you want to argue in favor of cargo yank rather than revisions, that’s fine. I personally think revisions are nicer to deal with, but that’s subjective at least.

M: (i'm gonna come out in favor of hackage revisions fwiw but i would prefer "known to not work" and "not known to work" operators)

Λ: Also, FWIW, rubygems also has gem yank, so it has mutation, too: https://guides.rubygems.org/command-reference/#gem-yank

J: I disagree that package removal is the same as modifying the metadata (which, again, goes further than just allowing trustees to tweak bounds). I don’t mind package removal

Λ: Why? The argument made above was “no other ecosystem needs this,” which is demonstrably false, it’s just that other ecosystems do it in a different way. In other ecosystems, you have to yank the versions with bad constraints and upload new ones (with different version numbers) with fixed constraints. Okay, so now the argument comes down to whether the revision model or the yank model is better. But with the yank model, suppose a new version of a package is released that invalidates a dozen versions’ bounds. Now you have to yank every one of those versions. If you want to support people running on those major/minor versions, you have to upload several new releases to fix the bounds for each of those major/minor lines.

J: If the revision model was limited to only changing the version bounds associated with the library portion of code residing in Hackage, I would probably not be opposed to it.

Λ: That is the revision model.

J: And I would likely prefer it over yanking. No it isn’t.

Λ: A Hackage revision cannot make code changes. Hackage revisions are to package metadata exclusively.

J: https://github.com/snoyberg/trojan Package metadata modification can extend further than purely modifying bounds associated with the library

Λ: You’re basically saying “Hackage revisions can be used maliciously because they can modify other parts of the cabal file,” but okay, so can modifying version bounds. Upload a new, malicious version of a package, then update the version bounds to use that package. If you want to pin to a particular revision to ensure you’re using a trusted plan, okay—that’s fine and reasonable! But this argument seems absurd to me: package management is fundamentally about installing untrusted code from someone else. Are you auditing all of it all the time? If not, then this “revision attack” seems like a very strange threat model to be worrying about.

J: Modifying version bounds for the core library stanza is extremely limited in scope. It introduces the possibility of transitively introducing malicious code via a version change in a third party library, but the trade off there seems to at least be reasonable with respect to the stated goal of not blocking perfectly valid build plans to be constructed with that library. I don’t find the trade off to be acceptable beyond that, personally. I don’t find restrictive upper bounds useful for test or benchmark code, and I find revisions there of limited utility accordingly.

Λ: > **J**: I don’t find the trade off to be acceptable beyond that, personally. Why?

J: And I especially dislike the ability to modify flag defaults, flag types, or add custom setup stanzas as this can change the behavior of a library at a particular version retroactively with very little visibility into the cause.

Λ: Has this literally ever happened? Or is this 100% theoretical? (I’m mostly referring to the custom-setup example; modifying flags seems like it has some valid use cases because automatic flags interact with the solver.)

J: I’m not aware of any time it’s happened, I’m simply pointing out that this attitude colors my perception of the feature in a way that makes me disinclined to favor it.

Λ: cabal-install allows you to:

* Pin the version and flags of a package with freeze files.
* Pin yourself to a state of the package index using index-state.

So you can pin yourself to a particular set of flags and revisions pretty easily. Through this lens, revisions are basically just another part of the version number, so the alternative—yanking—is worse, if anything. The yanking model allows you to do everything the revisions model allows you to do, plus you can make arbitrary code changes! However, the real reason I like revisions is that I think they are meaningfully distinct from the version number. To me, the version number versions the code, and revisions version the metadata. The second of those two things really is orthogonal to the first: the yank model requires you to bump a version number, causing a bunch of trouble and headaches for everyone who has pinned to that version number, even when no code changes were made at all. Revisions allow you to bump a different version number to not invalidate people’s build plans, and there is a social expectation that they will not do anything evil.

Package management is a social problem. There are ways to add technical guard rails and to make things easier on maintainers, but you cannot escape that at the end of the day, you are trusting tons and tons of other people at every level of the process. Revisions seem like a great option in that light, and most of the arguments I’ve heard against them boil down to “they make me uncomfortable” without providing much of a reasonable alternative.

Now, as for restrictive upper bounds on test suites: modern versions of cabal-install allow you to install a package’s library without installing its test suites and benchmarks. So even if the package has restrictive upper bounds on its test suites, you can still construct a build plan that includes the library. That seems like getting the best of both worlds. You can have the proper bounds on your test suites, and people only have to care about them if they actually want to build your test suite.

Sockington: @Λ: I would be so happy if you put together your research and thoughts on this into a blog post.
