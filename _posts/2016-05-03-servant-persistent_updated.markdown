---
title: "servant-persistent updated"
date: 2016-05-03
layout: post
categories: programming
---

Previously, I [wrote a blog post on using `servant` and `persistent` together]({% post_url 2015-06-07-servant-persistent %}).
`servant` has updated to the new 0.5 version, and I felt like it was a good idea to bring my tutorial up to date.
I'd also noticed that some folks were using the repository as a starter scaffold for their own apps, which is great!
To accommodate that, I've beefed up the application to use some more Best Practices.
Let's dive in!

# Application Structure

The application has three sub-components:

- `src` : contains all the library code
- `app` : contains the code for the executable
- `test` : contains the test code

It's a good idea to extract as much code as you can in the library.
This makes it easier to test the code, as you can import it into the tests without having to recompile it every time.
Additionally, you can make the library functions available for all kinds of potential executables down the line.

# `src/Api/Person.hs`
