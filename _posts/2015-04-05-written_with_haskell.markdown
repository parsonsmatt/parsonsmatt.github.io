---
title: "Written with Haskell!"
date: 2015-04-05
layout: post
categories: programming
---

Oh man! I wrote my first actually useful thing in Haskell! It's a stupid simple program that creates a new post for this Jekyll blog. I had written something similar in bash, but forgot to push it to my dotfiles repo. Rather than walking all the way upstairs, I decided that this would be easier and more fun!

[Here's a link](https://github.com/parsonsmatt/parsonsmatt.github.io/blob/master/newpost) to the code. All told, it was significantly easier for me to write than the equivalent bash script, and I definitely enjoyed writing Haskell more than bash. I can also see room for extensions: some additional command line parameters and a more versatile replace function (`replace :: [(Char, Char)] -> String -> String`).

## What took you so long?

I'm strongly reminded of [these graphs](https://github.com/Dobiasd/articles/blob/master/programming_language_learning_curves.md) on programming language productivity vs learning curves. I can't remember when exactly I started learning Ruby -- sometime in mid-2014 for sure, and after a few months, I'd written a script that read a CSV file and automated data entry over the web. I started learning Rails in early December and had my first app up and running early January, and am now employed as a Rails developer. Meanwhile, I've been learning Haskell since November '14, aside from the CIS194 exercises, haven't made anything with it. Why is that?

Perhaps the current learning resources focus too much on the more advanced concepts in Haskell, like the dreaded monad. [Learn You A Haskell](http://learnyouahaskell.com/chapters) doesn't get into basic file I/O until well after type classes and higher order functions. [Real World Haskell](http://book.realworldhaskell.org/read/) has a similar layout. But when you look at resources for beginning programmers learning Ruby, Java, JavaScript, etc. they start off with basic file I/O.

Is there a "Haskell for the Everyman"? I'd love for there to be a resource for learning Haskell that let you get more productive at first. I don't think the exact notation here is terribly difficult to teach or understand. 

```haskell
main = do
    args <- getArgs
    date <- today
    let rawTitle = concat args
        fileName = newPostFileName date rawTitle
    withFile fileName WriteMode $ \handle ->
        mapM_ (hPutStrLn handle) 
              [ "---"
              , "title: \"" ++ rawTitle ++ "\""
              , "date: " ++ show date
              , "layout: post"
              , "categories: programming"
              , "---"
              ] 
```

The only thing that's even a little tricky is `mapM_`, and you could easily write that as `forM_ list (hPutStrLn handle)` for something that's much more accessible to a beginning programmer.
