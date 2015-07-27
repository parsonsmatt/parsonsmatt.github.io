---
title: "Unix Is The IDE"
date: 2015-07-26
layout: post
categories: programming
---

## A Story on TDD in C++

Whenever people talk about editors and IDEs and other things,
something that invariably gets mentioned by a vim user is that:

> Unix is the IDE and vim is just the text editor.

There usually isn't a justification or explanation of that statement.
The IDE or emacs fans tend to think of that as a limitation of vim, rather than a strength, and feel ever more confident in their superiority.

I recently did "Unix is the IDE" and thought that a written experience on what that means exactly would be helpful.

## Ruby Experience

When I'm at work, I use Ruby, and I use a pretty standard workflow.
Guard watches the files in the directory and runs the corresponding test whenever a file is written.
With proper test design, this makes for an extremely quick and pleasant feedback loop.
Guard is always running in one of my tmux panes, and I get constant feedback whenever I'm working on a file.

This makes "test first" software development much easier and nicer.
Manually switching contexts, running `rspec`, waiting for the entire test suite to run, etc. breaks the development flow and makes it difficult to program effectively.
Allowing the feedback to exist solely as a color in my peripheral vision is huge for speeding up development.

## Class though

I'm currently taking a data structures and algorithms course at UGA.
The entire course is in C++, a language that I had no experience with before the course.
The first thing I looked for was a testing library.
[Catch](https://github.com/philsquared/Catch) stood out as being easy to use, unobtrusive, and simple: precisely the qualities I was looking for.
A few entries in my Makefile later, and I have `make test` for running the test suite and `make mtest` for running the test suite through Valgrind.
Very nice!

I looked for something similar to Guard (but for C++) and could not find anything.
I admit that I didn't look too hard.
It was just easier for me to make something on my own, and I didn't feel the need to spend a ton of time looking.

## Use the FIFO, Luke

What's the goal?
I want fast and unobtrusive test feedback.
And I don't want to spend a ton of time getting it setup, and it needs to work with what I've already got.
After all, this is just for a class, and for small projects.

Vim supports running shell commands.
At first, I just mapped a key to `:! make test`.
That took away my code, ran the tests, and then returned it back to me.
Mediocre.

I wanted it to be asynchronous and in my peripheral vision.
Vim doesn't support asynchronous stuff all that well natively, and I didn't feel like delving into the details.
Instead, I created a named pipe with `mkfifo .test_runner`.
I made a tmux pane and ran the extroardinaly complicated code:

```bash
$ `cat .test_runner`
```

Here's what's happening there:

1. The backticks tell the shell to evaluate what's inside and execute the resulting string.
2. `cat` opens the `.test_runner` FIFO and waits for input.
3. Until something writes to `.test_runner`, the backticks can't evaluate what's happening inside, and it waits.

Now, you can do `echo "make test" >> .test_runner`.
`cat` finally receives something, and it dutifully returns the string.
The backticks now have something to evaluate: `make test`, so they run the command.

Nice!

## Not quite...

Unfortunately `cat` stops after the first thing is received, so we need to keep the file open somehow.
I'm sure there are more elegant ways to do this, but the solution that just comes to mind immediately is:
wrap the whole thing in an infinite loop.

```bash
$ while true; do `cat .test_runner`; done
```

And now I can `echo` commands to the test runner FIFO all I want, and it dutifully executes them.

## Vimify

Now I've got a way for one process to send a command to another.
I need to incorporate a shortcut key that handles this in vim, so I can get that quick feedback I so desperately crave.
The code is again pretty simple:

```
map <leader>t :call system('echo "make test" >> .test_runner')<CR>
```

And now whenever I hit `<spacebar>t`, my tests run in my peripheral vision, and everything is cool.

All told, the time it took for me to get this stuff setup was probably ~20 minutes or so.
It was faster for me to invent this kinda hacky solution than it was for me to "Do It Right".

In about half an hour, I had enough of a realtime testing feedback loop to work confidently.
It wasn't ideal.
But it did get the job done.

Over the next few days, I slowly tweaked the setup:
the bash script that opened the loop also made the FIFO and deleted it on exit,
and I had the vim shortcuts also save the current file.
But for the most part, a tiny bit of effort did just about everything I needed to be productive.

## Unix As IDE

I didn't have to learn anything new here.
Not even a plugin, or new shortcuts.
All I did was reuse some existing components in a simple way to enable my current programming environment to support an entirely new language.

My first programming experience was with Eclipse and Java, last spring.
In the summer, we were encouraged to use vim or emacs, and I begrudgingly started learning vim. 
Before, I'd used the student edition of RubyMine for Ruby, and I think Notepad++ for JavaScript.
If I hadn't learned vim and Unix as welll, I imagine that I'd be trying to learn Code::Blocks or something similar.

I don't want to have to spend a ton of time learning different tools, especially for something bas basic as editing text.
I want to leverage highly general tools to accomplish a wide variety of tasks.
Using Unix as the IDE and vim as the text editor fits that perfectly.

## now i am not saying...

I'm not saying that IDEs suck.
IDEs are awesome pieces of technology that are great at what they do.
If you're working on a large project in a single language that really benefits from having an IDE,
then by all means, you should be using the IDE.

If you're a big fan of emacs as the OS and using it's inferior process management to handle REPLs, shells, etc., then that's also great.
I've just found that it doesn't really like to be subservient in the same way that vim does.
It wants to dominate your workflow and be your single point of contact.
There's nothing wrong with that.

But for me, I really like the simplicity and composability of more general tools that don't mind taking up as little space as you want them to.
Vim does a fantastic job of getting out of the way.
Even with a ton of plugins and customization, loading vim is lightning fast.
It integrates extremely well with the Unix shell.
