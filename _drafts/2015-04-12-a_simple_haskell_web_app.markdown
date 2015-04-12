---
title: "A simple Haskell web app"
date: 2015-04-12
layout: post
categories: programming
---

Alright!! I've been learning Haskell for a while now, and it's about time that I build something useful. I've been wanting a simple, clean, and awesome weightlifting web application for a while now, and while [weightxreps](http://www.weightxreps.net) is pretty awesome, it's still a lot slower and clunkier than it needs to be, and the mobile site just isn't there.

I'm going to build my own, and I'll document the process in this blog post!

## Planning:

The MVP is pretty damn M. I want it to handle basic authentication and registration, and then I want it to store a JSON blob corresponding to weightlifting sessions. Eventually I'll parse it into more usable formats, but at the moment, I just want something nicer than Blogger!

I'm going to use the [Spock](https://www.github.com/agrafix/Spock) web framework. It looks a little nicer to use than scotty and a bit less overbearing than Yesod, and should be a nice transition into Haskell development.
