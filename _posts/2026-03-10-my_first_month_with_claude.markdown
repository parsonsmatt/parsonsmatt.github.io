---
title: "My first month with Claude"
date: 2026-03-10
layout: post
categories: programming
---

I've been watching AI development for a long time.
I found LessWrong around 2012-2013, and managed to get myself worked up about the oncoming singularity.
I managed to chill out about it, but interest and excitement for AI remained.
The initial Deep Dream image generation, Alpha Go, etc, were all so exciting.
And then GPT-2 came out.

Over the last five years, people have been making wild claims about the utility of *present* AI.
Not "the AI that you'll have *soon*," but the current generation stuff.
And the results, frankly, had been garbage.
A sea of garbage coating the internet.
I'd try using the tools, and when checking them against my own expertise or knowledge, they always fell short.

I heard the noise on Twitter after Opus 4.5 was released in November of 2025.
Seemed like a step change- people were much more impressed with it than prior versions.
In December, I decided to give it a try.
Opus 4.5, with significant guidance, properly diagnosed and fixed some Template Haskell code generation issues.
It knew how to `-ddump-splices`, it knew how to read those splices and diagnose the issue.
Given a small, highly mechanical problem, plenty of examples, and a ton of tests, it took about 6 hours to do what I felt would have taken me 3 or 4 hours.

This is pretty incredible, because my productivity has always been limited by two things:

1. Effort. Literally whacking my keyboard and staring at computer and waiting on a compile/test loop to tell me what to do next.
2. Attention. Where I'm focusing my effort. My editor? Slack? Meetings? A bike ride? Cello? Which OSS project?

Now, with Opus 4.5, I can set a robot going and do *something else* with my effort.
While Claude Code was spinning on the Template Haskell code, I was doing another project in a different repository.
Sure, Claude took 6 hours instead of my 3, but I was able to fill those 6 hours with *effort* and attention placed elsewhere - not a full 6, as Claude required supervision and input, but call it 5.
This is a positive investment, and my personal "break even" moment.

# Using Claude Code Effectively

In mid February, I got access to an API token and unlimited usage.
I've been trying to figure out how to leverage this tool to improve my productivity, and the results have been pretty strongly positive.

The brief tl;dr:

> It's the same shit that makes humans good at software development

## Haskell is Awesome for LLMs

This was true with Opus 4.5 and is much more true with Opus 4.6.
Prior versions of LLM coding agents produced utter garbage with Haskell, most likely due to the relatively low quantity of examples.
It *seems* like the AI labs have figured out how to do higher quality training with less data, and the relatively high average quality of Haskell code helps the LLMs generate relatively high quality Haskell.

Haskell's type safety, purity, and library design opportunities make it a fantastic choice for LLM generated code.
The human developer can easily specify a solution and let Claude fill in a *surprising* amount of the boring details.

Haskell's terse nature benefits LLMs - you can simply fit more tokens into the context window when the tokens are more semantically dense.

Funny enough, all of Haskell's benefits "for LLMs" are also benefits of Haskell for humans.
I do earnestly believe that if all devs knew Haskell, we would consider switching to other languages only very rarely.
And Claude *knows Haskell*.

## Software Engineering Matters

Claude Code works *really well* with tightly scoped issues, lots of tests and examples, and good safety guardrails.
I asked it to make `cabal` faster, and taught it how to run `cabal` with debug logs, timings, and then to build a profiled version of it.
Then it looped for a bit, collected timing information on our codebase, and figured out the critical path and hot spot - the solver.
Then it [made several fixes to optimize the solver](https://github.com/haskell/cabal/pull/11566).
These changes resulted in a 30% improvement in solver times, which shaved 2 seconds off every `cabal repl` invocation- a pretty nice benefit, since that happens virtually anytime you want to do anything in our codebase.

But this only worked because the `cabal` library had timing logs, and I gave it a quick feedback loop and target.
I've had Claude Code totally fall over when trying to do bigger or more undirected work.

Fortunately, Claude can do this pretty well.
I've had Claude do some exploratory research (generally pretty highly supervised), then generate some plans for improvement (then edited and clarified), and it can then do a good job of writing up a ticket- certainly better than almost all human written tickets I've seen.

## Build Workflows Iteratively

LLMs can do anything.
But they are expensive, slow, and non-deterministic (and often incorrect).
So get the LLM to help with *replacing themselves* - build a tool or skill to do the thing faster and deterministically.

My Claude sessions generally progress from "highly supervised, exploratory work" to "mostly unsupervised, automated work."
Early sessions in a project often involve having Claude build tools - CLI scripts, libraries, interfaces - that it can use in later work to make the job easier.
A surprisingly effective prompt here is "What tools would help you do this job better next time?"
At the end of a session, I'll also have Claude review and update its skill documentation with everything I told it to do differently.

So each work session with Claude produces:

1. An artifact (the work itself)
2. Often, updates to the skill to improve efficiency on further work
3. Sometimes, a tool to deterministically do some chunk of the work.

This process ends up reducing the highly non-deterministic LLM tool with a much more deterministic tool.

## Mock Reviews and Refactoring

You can ask Claude to review code, and that works OK.
But Claude works *much better* if you ask it to assume someone else's perspective.
I've asked it to mimic myself and it did Alright.
I asked it to mimic Edward Kmett, Alexis King, and Michael Snoyman, and it did Alright - it noticed different things with each perspective and suggested improvements in line with those perspectives.

I've generally found that the initial output is of poor to middling quality.
But you can get decently far with "now make it more legible/faster/more correct" or "apply 'Parse, Don't Validate' here" etc.
After several rounds of refactoring, it makes stuff that I'm reasonably happy with putting my name on.

# What Doesn't Work Well

Claude isn't a replacement for human engineering ("yet" i guess).
It lacks qualities like taste, judgement, and vision, that are generally required in subjective work like software and product design.
So when I let Claude run totally loose on something, it *produces*, but it produces poor quality code and poorly thought out features.

I haven't had great luck with getting Claude to iterate on this itself.
When given the very large picture, it sort of flounders.
It can do some analysis and subdivision, but the divisions are often somewhat unnatural and don't feel right to me.

## Defined by our Vice

If the above complaint is about Claude's lack of virtue, let me also complain about Claude's lack of vice.
Claude is infinitely patient and willing to work very hard.
However, "infinitely patient" means that Claude has no problem at all waiting an hour for a build to finish.
You have to teach it to use faster tools and feedback loops.

Likewise, "hardworking" is a virtue when you're paying a human by the month and trusting in their laziness to be efficient, but when you're paying per unit of thought, "more work" means "more cost" and often not "more output."
You have to tell Claude to *stop doing stuff* or to do stuff more efficiently.

Fortunately, Claude is relatively teachable - but Claude very often will start a skill and then do a lot of "research and understanding" before running the one-shot script to generate the compile-errors to track down and fix.

Humans are impatient and lazy, so we build fast and efficient systems.
Without pain to guide us, we make little progress in *reducing* that pain.

# Am I still a skeptic?

I've been using AI to write 95% of my code for the last month.
And yet, I still feel like I'm more on the skeptic side of things.
AI is clearly a *useful* tool - my own productivity has doubled or more while maintaining my personal quality bar.
But it's not a do-it-all miracle - yet?

AI-first companies are experiencing massive reliability issues.
Vibe coding projects start, enjoy some success, and then go down in flames.

Humans are clearly still necessary at key points in the software lifecycle.
The bottlenecks have shifted, though, and the easiest parts of my job have been mostly automated.
What's coming next?

I'm excited to wait and find out.
