---
title: "From Rails to Yesod I"
date: 2015-03-19
layout: post
category: programming
---

## Part I: Introduction

Ruby on Rails is a fantastic tool for building an app quickly and painlessly. The incredible array of gems and the focus on a pleasant development experience make it an excellent choice for beginning development: it's trivial to run `rails new .`, throw some plugins into your `Gemfile`, and use the generator scripts to build a basic scaffold of your site. Ruby itself is a pleasure to use, allowing developers to rapidly write expressive code with an extensive standard library.

Unfortunately, Ruby and Rails both suffer from being relatively slow and resource intensive. This isn't a problem for most applications, as even a $5 DigitalOcean instance is plenty to run a Rails app under light loads. The size of your Rails app gets multiplied with every forked process, and every gem and feature you add just grows that complexity.

It's certainly possible to continue running Rails on a high volume application. However, many Rails applications are converted to more performant alternatives: Twitter went to Scala, LinkedIn went to Node, etc. When you're paying for every bit of computer power you're using, you can make signficant business savings moving to a platform that requires less power to use.

Additionally, while Ruby and Rails applications are very easy to start, they can easily become difficult to maintain as they grow. Migrating to another platform might provide a better development environment for a larger application. Node.js, by all accounts, is worse on the maintainability fron (until ES6 becomes mainstream). Where do we go? I'd like to think that Yesod makes for an excellent choice.

### Why Yesod?

Haskell has a fantastic community built around producing reliable, robust, and beautiful code. The language itself encourages a development style that is concise, easy to maintain, highly modular, and highly composable. Additionally, Yesod is one of the fastest web frameworks around, beating Node.js by a factor of ~4 [according to this older benchmark](http://www.yesodweb.com/blog/2011/03/preliminary-warp-cross-language-benchmarks) (the Warp server is the Haskell webserver that Yesod runs on top of).

Yesod has many features that a Rails developer will enjoy. Routes are configured in a DSL similar to the `config/routes.rb` file. Views are composed similarly to Rails views, but with a greater level of modularity (widgets vs template partials). Yesod enforces RESTful design, which a Rails developer should find natural. Yesod's default configuration is a MVC setup similar to Rails.

The real reason, though, is that I really like using Haskell, and I want to use it productively in a real world way.

## Planned parts:

In this blog post series, I'll be converting my [MelodyScout](http://www.melodyscout.com) application to Haskell, and tracking the changes in [this Github repository](http://www.github.com/MelodyScout/MelodyScoutHS). My plan of attack for the transition has three main stages:

1. Creating a JSON API
2. Converting the front end to a SPA that consumes the JSON API
3. Creating a Yesod JSON API that mirrors the MelodyScout API
