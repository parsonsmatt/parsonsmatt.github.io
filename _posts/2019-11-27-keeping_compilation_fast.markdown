---
title: "Keeping Compilation Fast"
date: 2019-11-27
layout: post
categories: programming
---

You're a Haskell programmer, which means you complain about compilation times.

We typically spend *a lot* of time waiting for GHC to compile code.
To some extent, this is unavoidable - GHC does a tremendous amount of work for us, and we only ever ask it to do more.
At some point, we shouldn't be terribly surprised that "doing more work" ends up meaning "taking more time."
However, there are some things we can do to allow GHC to avoid doing more work than necessary.
For the most part, these are going to be code organization decisions.

In my experience, the following things are true, and should guide organization:

- Superlinear:  GHC takes more time to compile larger modules than smaller modules.
- Constant costs: GHC takes a certain amount of start-up time to compile a module
- Parallelism: GHC can compile modules in parallel (and build tools can typically compile packages in parallel)
- Caching: GHC can cache modules

So let's talk about some aspects of project organization and how they can affect compile times.

# The `Project.Types` Megamodule

You just start on a new project, and you get directed to the God module - `Project.Types`.
It's about 4,000 lines long.
"All the types are defined in here, it's great!"
However, this is going to cause big problems for your compilation time:

- A super large module is going to take way longer to compile
- Any change to any type requires touching this module, and recompiling everything in it
- Any change to this module requires recompiling any module that depends on it, which is usually everything

We pretty much can't take advantage of caching, because GHC doesn't cache any finer than the module-level.
We can't take advantage of parallelism, as GHC's parallelism machinery only seems to work at module granularity.
Furthermore, we're tripping this constantly, which is causing GHC to recompile a lot of modules that probably don't need to be recompiled.

## Resolution

Factor concepts out of your `Project.Types` module.
This will require manually untangling the dependency graph, which can be a little un-fun.
It's probably also a good excuse to learn `.hs-boot` files for breaking mutual recursion.

There's a small constant cost to compile a module, so you probably shouldn't define a module for every single type.
Group related types into modules. 
The sweet spot is probably between 50-200 lines, but that's a number I just summoned out of the intuitional aether.

This process can be done incrementally.
Pick a concept or type from the bottom of your depedency graph, and put it in it's own module.
You'll need to import that into `Project.Types` - but do not reexport it!
Everywhere that complains, add another import to your new module.

As you factor more and more modules out, eventually you'll start dropping the dependency on `Project.Types`.
Now, as you edit `Project.Types`, you won't have to recompile these modules, and your overall compile-times will improve dramatically.
All the types that are pulled out of `Project.Types` will be cached, so recompiling `Project.Types` itself will become much faster.

Before too long, you'll be minimizing the amount of compilation you have to do, and everything will be happy.

# Package Splitting

Okay so you think "I know! I'll make a bunch of packages to separate my logical concerns!"
This is probably smart but it comes with some important trade-offs for development velocity and compile-times.

## GHCi

GHCi is pretty picky about loading specific targets, and what you load is going to determine what it will pick up on a reload.
You need to ensure that each target has the same default extensions, dependencies, compiler flags, etc. because all source files will be loaded as though they were in a single project.
This is a good reason to either use Cabal or `hpack` common stanzas for all of this information, or to use file-specific stuff and avoid using implicit configuration.

What's a "load target"?
A target is a part of a package, like a library, a specific test-suite, a specific executable, or a sub-library.
In a multi-package Cabal or Stack project, load targets can come from different packages.

Another gotcha is that any relative filepaths must resolve based on where you're going to invoke `{stack,cabal} ghci`.
Suppose you decide you want to split your web app into two packages: `database` and `web`, where `database` has a file it loads for the model definitions, and `web` has a bunch of files it loads for HTML templating.
The Template Haskell file-loading libraries pretty much assume that your paths are relative to the directory containing the `.cabal` file.
When you invoke `stack ghci` (or `cabal repl`), it puts your CWD in the directory you launch it, and the relative directories there are probably not going to work.

Once you've created that package boundary, it becomes difficult to operate across it.
The natural inclination - indeed, the reason why you might break it up - is to allow them to evolve independently.
The more they evolve apart, the less easily you can load everything into GHCi.

You can certainly load things into GHCi - in the above example, `web` depends on `database`, and so you can do `stack ghci web`, and it'll compile `database` just fine as a library and load `web` into GHCi.
However, you won't be able to modify a module in `database`, and hit `:reload` to perform a minimal recompilation.
Instead, you'll need to kill the GHCi session and reload it from scratch.
This takes a lot more time than an incremental recompilation.

## Module Parallelism

GHC is pretty good at compiling modules in parallel.
It's also pretty good at compiling packages in parallel.

Unfortunately, it can't see across the package boundary.
Suppose your package `hello` depends on module `Tiny.Little.Module` in the package `the-world`, which also contains about a thousand utility modules and Template Haskell splices and derived Generic instances for data types and type family computations and (etc......).
You'd *really* want to just start compiling `hello` as soon as `Tiny.Little.Module` is completely compiled, but you can't - GHC must compile everything else in the package before it can start on yours.

Breaking up your project into multiple packages can cause overall compile-times to go up significantly in this manner.
If you do this, it should ideally be to split out a focused library that will need to change relatively rarely while you iterate on the rest of your codebase.
I'd beware of breaking things up until absolutely necessary - a package boundary is a heavy tool to merely separate responsibilities.

## Package parallelism

The good news is that it is quite easy to cache entire packages, and the common build tools are quite good at compiling packages in parallel.
It's not that big of a deal to depend on `lens` anymore, largely because of how good sharing and caching has gotten.
So certaily don't be *afraid* to split out libraries and push them to GitHub or Hackage, but if you're not willing to GitHub it, then it should probably stay in the main package.

# Big Ol Instances Module

Well, you did it.
You have a bunch of packages and you don't want to merge them together.
Then you defined a bunch of types in `foo`, and then defined a type class in `bar`.
`bar` depends on `foo`, so you can't put the instances with the type definitions, and you're a Good Haskeller so you want to avoid orphan instances, which means you need to put all the instances in the same module.

Except - you know how you had a 4,000 line types module, which was then split-up into dozens of smaller modules?
Now you have to import *all* of those, and you've got a big 3,000 class/instance module.
All the same problems apply - you've got a bottleneck in compilation, and any touch to any type causes this big module to get recompiled, which in turn causes everything that depends on the class to be recompiled.

A solution is to ensure that all your type classes are defined above the types in the module graph.
This is easiest to do if you have only a single package.
But you may not be able to do that easily, so here's a solution:

## Hidden Orphans

The real problem is that you want to refer to the class and operations without incurring the wrath of the dependency graph.
You can do this with orphan instances.
Define each instance in it's own module and import them into the module that defines the class.
Don't expose the orphan modules - you really want to ensure that you don't run into the practical downsides of orphans while allowing recompilation and caching.

You'll start with a module like this:

```haskell
module MyClass where

import Types.Foo
import Types.Bar
import Types.Baz

class C a

instance C Foo
instance C Bar
instance C Baz
```

where a change to any `Types` module requires a recompilation of the entirety of the `MyClass` module.

You'll create an internal module for the class (and any helpers etc), then a module for each type/instance:

```haskell
module MyClass.Class where

class C a


module MyClass.Foo where

import MyClass.Class
import Types.Foo

instance C Foo


module MyClass.Bar where

import MyClass.Class
import Types.Bar

instance C Bar


module MyClass.Baz where

import MyClass.Class
import Types.Baz

instance C Baz


module MyClass (module X) where

import MyClass.Class as X
import MyClass.Foo as X
import MyClass.Bar as X
import MyClass.Baz as X
```

So what happens when we touch `Types.Foo`?
With the old layout, it'd trigger a recompile of `MyClass`, which would have to start entirely over and recompile *everything*.
With the new layout, it triggers a recompile of `MyClass.Foo`, which is presumably much smaller.
Then, we do need to recompile `MyClass`, but because all the rest of the modules are untouched, they can be reused and cached, and compiling the entire module is much faster.

This is a bit nasty, but it can break up a module bottleneck quite nicely, and if you're careful to only use the MyClass interface, you'll be safe from the dangers of orphan instances.

# Some random parting thoughts

- Don't do more work than you need to. Derived type class instances are work that GHC must redo every time the module is compiled.
- Keep the module graph broad and shallow.
- TemplateHaskell isn't that bad for compile times. 
    - You pay a 200-500ms hit to fire up the interpreter at all, but from there, most TH code is quite fast - running the TH code to parse and generate models from 1,500 lines of `persistent` quasiquoter takes about 50ms.
    - The slow part is compiling the resulting code - those 1,500 lines of model definitions expanded out to something like 200kloc
    - The solution is to split up the module, following the tips in this post!
- The following command speeds up compilation significantly, especially after exposing all those parallelism opportunities:
    ```
    stack build --fast --file-watch --ghc-options "-j4 +RTS -A128m -n2m -qg -RTS"
    ```
    These flags give GHC 4 threads to work with (more didn't help on my 8 core computer), and `-A128m` gives it more memory before it does GC.
    `-qg` turns off the parallel garbage collector, which is almost always a performance improvement.
    Thanks to [/u/dukerutledge](https://www.reddit.com/r/haskell/comments/e2l1yj/keeping_compilation_fast/f8wt34p/) for pointing out `-n2m`, which I don't understand but helped!
- Try to keep things `ghci` friendly as much as possible. `:reload` is the fastest way to test stuff out usually, and REPL-friendly code is test-friendly too!
