---
title: "The Tower of Type Safety"
date: 2017-10-28
layout: post
categories: programming
---

Type systems are a tradeoff -- on the one hand, you get static guarantees about your program.
On the other hand, you have to worry about types.
Like any reasonable trade off between two ends, people tend to pick absolutist positions and defend them to the death.
While it's fun to engage in culture wars (emacs vs vim, Windows vs Linux, OSX vs a dumpster), sometimes it's good to take a step back and reflect on what these trade-offs are.

Dynamically typed languages give you ultimate flexibility -- any variable can hold anything at all.
This results in a system that is extremely extensible.
A Ruby method can accept any object at all, and call whatever methods it wants on it.
You never need to worry about some library "not accepting" a value just because it has a type that the library didn't know about, even if it's totally fine to do so.

This flexibility is good.
It carries a cost: little and/or very difficult static analysis.
But the flexibility is good!
Programmers that know how to properly use dynamically typed languages can be very productive, and provided good unit test coverage, system design, etc. even safe and reliable.

For a programmer that likes dynamic types, static types seem overly restrictive.
A type system is -- fundamentally -- about restricting the amount of programs we can write.
And, indeed, a weak type system is possibly worse than no type system at all -- it takes away too many good programs, and doesn't forbid many bad programs.
The type systems of Java and C are, in my opinion, not worth it.

This implies that there is some "scale" or "levels" to type systems.
As we climb the levels, we should expect more restriction, more safety, more expressiveness, and more complexity/difficulty.
When we climb up the tower of type safety, we'll also be able to *get rid of* some of the complexity/difficulty of dynamically typed programming, along with the flexibility and other stuff we miss.

# Level 1: Dynamic Types

This is where dynamically typed languages like Ruby, JavaScript, etc. sit.
There is no static analysis, so all type errors occur at runtime.
We talk about the "duck type" of objects, which is essentially the set of method signatures they have.

## Java

We can do this in Java by using the `Object` type and casts.
Java supports downcasts, though the language will throw an exception if the object cannot safely be cast.
However, programming in this style gets rather unweildy, as Java's type system isn't powerful enough to express what we need.
We must write explicit interfaces for each method we want to duck type, and we have to ensure that -- at some level -- we're actually explicitly implementing those interfaces on some concrete class somewhere.
While we *can* write using duck types in Java, the type system is incapable of expressing it conveniently, and we must write a ton of boilerplate.
An enterprising IDE could certainly generate/manage all of this boilerplate for you, and perhaps even impose a more advanced type system/linter to verify safety.

When we define methods like this, we use generic types with interfaces to ensure we get all the methods we want.

```java
public interface CanFoo { 
    public void foo();
}

public interface CanBar {
    public void bar();    
}

public class App {
    public <T extends CanFoo & CanBar> 
    void doStuff(T thing) {
        thing.foo();
        thing.bar();
    }
}
```

## Haskell

Haskell supports this kind of programming with two types, each offering varying levels of safety.
We can use the function `unsafeCoerce` to coerce any value to a type named `Any`.
This is mostly like casting an object in C -- we're telling Haskell to interpret the value differently in memory.

```haskell
import Unsafe.Coerce (unsafeCoerce)
import GHC.Base (Any)

foo :: Int
foo = 1

heterogenousList :: [Any]
heterogenousList =
  [ unsafeCoerce foo
  , unsafeCoerce 'b'
  , unsafeCoerce "hello"
  ]
```

When we use `unsafeCoerce` to cast from `Any` to a result type, we will always get a result.
It'll just be.. weird.
Consider this REPL example:

```haskell
λ> unsafeCoerce "hello" :: Int
5764607524141601919
λ> unsafeCoerce "hello" :: Double
2.681562245356041e154
λ> unsafeCoerce "hello" :: Char
'\7493989781051887687'
λ> unsafeCoerce "hello" :: [Int]
[104,101,108,108,111]
```

We cast the string `"hello"` to various types and get an interpretation of the result.
This technique is only useful if we're storing the type somehow, like the [superrecord](https://www.athiemann.net/2017/07/02/superrecord.html) library.
That library makes a safe extensible record type using phantom types over an `SmallArray# Any` underlying representation.

A more useful way to have dynamic types is the [`Data.Dynamic`](https://hackage.haskell.org/package/base-4.10.0.0/docs/Data-Dynamic.html) module.
Believe it or not, Haskell has full support for dynamic types!
You can convert any value into a `Dynamic`ally typed value using `toDyn`, and you can cast a value using `fromDynamic`.
If you're expecting the wrong type, it returns `Nothing`, and if you guessed right, you get `Just val`.

The [`Data.Typeable`](https://hackage.haskell.org/package/base-4.10.0.0/docs/Data-Typeable.html) module provides a lot of utilities for working with runtime type information, including a `cast` function.

