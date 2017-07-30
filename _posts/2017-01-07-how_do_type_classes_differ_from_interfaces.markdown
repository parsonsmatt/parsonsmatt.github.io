---
title: "How do type classes differ from interfaces?"
date: 2017-01-07
layout: post
categories: programming
---

Haskell type classes are a tricky concept for many Haskell beginners to learn.
Most languages cannot express them at all, and they don't have a concept that comes close.
For many object oriented languages, the `Interface` is the closest language construct available.
Ruby `module`s fill a similar niche.
However, while these concepts both address name overloading and a kind of polymorphism, they miss some of the power that type classes provide.

This post is intended for people curious about type classes.
It doesn't assume any knowledge of Haskell or functional programming.
Familiarity with a statically typed language like Java or C# will help.

# Type Class Introduction/Recap

If you know what a type class is, feel free to skip to the next header.

To recap, a Haskell type class is defined like this:

```haskell
--    [1]       [2] [3]
class (Eq a) => Ord  a  where
--  [4]
    compare :: a -> a -> Ordering

data Ordering = EQ | GT | LT
```

1. This is the "super class" constraint. To make a type an instance of `Ord`, it must be an instance of `Eq`.
2. This is the class name.
3. This is the type that we are parameterizing the class over.
4. This is the function type definition (or list of such) we need to define for `a` in order to make `a` an instance of `Ord`.

We can read the above code snippet as:

> Declare a class `Ord` that is parameterized on some type `a` where `a` has an `Eq` type class instance.
To make the type `a` an instance of `Ord`, you must define the `compare` function.
This function takes two values of the type `a` and returns a value with the type `Ordering`.

First, we'll create a toy datatype `ToyOrd`.

```haskell
data ToyOrd = Smol | Large Int
```

This data type has two constructors: `Smol`, which has no fields, and `Large`, which has a single `Int` field.
In order to make it an instance of `Ord`, we have to make it an instance of `Eq`:

```haskell
--  [1]  [2]  [3]
instance Eq  ToyOrd where
--         [4]
    Smol    == Smol    = True
    Large x == Large y = x == y
    _       == _       = False
```

1. We start making an instance with the `instance` keyword.
2. This is the class name we're making an instance of.
3. `ToyOrd` is the type that we're making an `Eq` instance for.
4. The `Eq` class defines a function `(==) :: a -> a -> Bool` and `(/=) :: a -> a -> Bool`. Since `(/=)` has a default implementation, we can only implement `(==)`.

Now that we've defined an `Eq` instance for our `ToyOrd` data type, we can define `Ord`.

```haskell
instance Ord ToyOrd where
    compare Smol Smol =           -- [1]
        EQ
    compare Smol (Large _) =      -- [2]
        LT
    compare (Large x) (Large y) = -- [3]
        compare x y
    compare (Large _) Smol =      -- [4]
        GT
```

1. Two `Smol` values are equal.
2. A `Smol` value is always less than a `Large`.
3. Two `Large` values are compared by their `Int` values.
4. A `Large` is always greater than a `Smol`.

Once you have a type class, you can write functions which *expect* an instance of that type class as an input.
Let's define the ordering operators:

```haskell
(<=) :: Ord a => a -> a -> Bool
a1 <= a2 =
    case compare a1 a2 of
        LT -> True
        EQ -> True
        GT -> False

(>) :: Ord a => a -> a -> Bool
a1 > a2 = not (a1 <= a2)
```

We specify that this function works *for all* types `a` provided that these types are an instance of the `Ord` type class.

# Similarity With Interfaces

Java interfaces allow you to specify a set of methods that an object supports, and, as of Java 8, default implementations for these methods.
So we can write an interface that does essentially the same thing as `Ord`.

```java
public interface Eq {
    default public bool equalTo(Eq a) {
        return ! this.notEqualTo(a);   
    }

    default public bool notEqualTo(Eq a) {
        return ! this.equalTo(a);
    }
}
```

This is the `Eq` interface in Java.
We're taking advantage of those default implementations.
If we don't override one of them, then we'll loop infinitely if we try to call a method.

```java
public interface Ord extends Eq {
    public Ordering compare(Ord other);
}

public enum Ordering {
    LT, EQ, GT
}
```

We can also write generic methods in terms of these interfaces.

```java
class OrdUtil {
    bool lessThanOrEqual(Ord a1, Ord a2) {
        Ordering result = a1.compare(a2);
        return result == LT || result == EQ;
    }
}
```

On the surface, these look similar.
However, there are a number of important differences!

# Differences!

### Differing types!

The type signature for the Haskell `compare` function is *very* specific about the types of it's arguments.

```haskell
compare :: Ord a => a -> a -> Ordering
```

This type signature says:

> The caller of this function can pick any type `a` that is an instance of `Ord`.
I will return an `Ordering`.

Note that the type signature *requires* that both parameters to `compare` have the same type!
It is illegal to write `compare Smol 10`.
The Java version allows *any* two objects to be passed, provided they implement the `Ord` interface.

The Java equivalent looks more like:

```java
public class OrdUtil {
    static <A extends Ord> bool lessThanOrEqual(A a1, A a2) {
        Ordering result = a1.compare(a2);
        return result == LT || result == EQ;
    }
}
```

This method signature introduces a generic type variable `A`, and says that `A` must extend/implement the `Ord` interface.
The method then takes two parameters, both of which have the same generic `A` type.

### Separation of Implementation

Java classes are defined in one place.
Any interface a class implements must be defined on that class.
Java doesn't handle sum types very well, so we'll just do `Large` from our `ToyOrd` class above.

```java
class Large implements Eq, Ord {
    public final int size;

    public Large(int size) {
        this.size = size; 
    }

    public bool equalTo(Eq other) {
        if (other instanceof Large) {
            Large other1 = (Large) other;
            return other1.size == this.size; 
        } 

        return false;
    }

    public Ordering compare(Ord other) {
        if (other instanceof Large) {
            Large other1 = (Large) other;
            if (other1.size < this.size) {
                return Ordering.LT; 
            }
            if (other2.size == this.size) {
                return Ordering.EQ; 
            }
            return Ordering.GT;
        } 

        throw new RuntimeException("what does this even mean");
    }
}
```

We've defined `compare` and `equalTo`.
Note that we have to do `instanceof` and type casting in order to properly implement these methods.
What does it even mean to try and compare two objects of arbitrary type?

Suppose that we've imported `Large` from some upstream package, and we've defined our own interface.

```java
interface SomeOtherPackage {
    public bool doSomeThing(int lol);
}
```

We are *completely* incapable of making `Large` implement our `SomeOtherPackage` interface!
Instead, we must wrap the class with a new class that we control, which implements the interface and otherwise delegates to `Large`.

```java
public class MyLarge implements SomeOtherPackage {
    public final Large large;

    public MyLarge(Large large) {
        this.large = large; 
    }

    public bool doSomething(int lol) {
        System.out.println("wut"); 
    }
}
```

Type classes *separate* the definition of data types and the instances of a class.
So, supposing that I imported the `ToyOrd` from another package, I can easily do:

```haskell
import JokesAreFun (ToyOrd(..))

class MyNewClass a where
    doSomething :: a -> Int -> IO ()

instance MyNewClass ToyOrd where
    doSomething Smol x = putStrLn "hahahaa yess"
    doSomething (Large x) y = putStrLn ("numbers! " ++ show (x + y))
```

# Return Type Polymorphism

Here's one of the *bigger* and more amazing things that type classes allow you to do.
We call it *return type polymorphism*.
And it's kind of obscene.

Let's define a Haskell type class for actions which can fail.

```haskell
--            [1]
class CanFail failable where
--  [2]       [3]
    oops :: failable a
--  [4]       [5]            [6]           [7]
    pick :: failable a -> failable a -> failable a
--  [8]
    win  :: a -> failable a
```

1. `failable` is the type variable name that we're using for the class.
2. `oops` is a *value* representing a failed computation.
3. We apply the type variable `a` to the class variable `failable`. So `failable` must take a generic type parameters.
4. `pick` is a function which looks at the two parameters.
5. If the first parameter is not a failure, then we accept it.
6. Otherwise, we return the 2nd parameter.
7. So the return value is going to allow us to choose a successful value from two possibilities, or fail entirely.
8. Finally, we give a way to succeed, but only if we take an `a` as a parameter.

We can easily make an instance for the `Maybe` type:

```haskell
--   [1]   [2]
data Maybe  a
--    [3]
    = Just  a
--    [5]
    | Nothing
```

1. `Maybe` is the name of the *type* we are declaring here.
2. It takes a single generic type variable, which we introduce and name as `a`.
3. It has two constructors. The first is `Just`, which takes a single parameter of the generic type `a`.
4. The second constructor `Nothing` does not take any type parameters.

```haskell
-- without annotations,

data Maybe a = Just a | Nothing

notAnInt :: Maybe Int
notAnInt = Nothing

hasAnInt :: Maybe Int
hasAnInt = Just 5
```

Now, let's write our `CanFail` instance!

```haskell
instance CanFail Maybe where
    oops = Nothing
    pick (Just a) _ = Just a
    pick Nothing (Just a) = Just a
    pick Nothing Nothing = oops
    win a = Just a
```

Now, we can write some functions in terms of `CanFail`.
We can write a safe division by zero function:

```haskell
safeDivision :: CanFail failable => Double -> Double -> failable Double
safeDivision x y =
    if y == 0 
       then oops
       else win (x / y)
```

This function signature is doing something very interesting here!
Let's translate it to plain English:

> `safeDivision` is a function which accepts two arguments of type `Double`, and returns a value having the type `failable Double` where `failable` is a *generic type variable* that the caller may pick, as long as that type has an instance of `CanFail`.

Woah!
The *caller* gets to pick the type?
That means I can write code like:

```haskell
someMathFunction :: Double -> Double -> Double
someMathFunction x y =
    let result = safeDivision x y in
    case result of
        Just number -> 
            number * 3
        Nothing ->
            0 
```

As the *caller* of `safeDivision` in this function, I am able to *select* the `Maybe` type.
What if there are other instances?

```haskell
data MaybeError a = Error String | Result a 

instance CanFail MaybeError where
    oops = Error "oops"
    pick (Result a) _ = Result a
    pick _ (Result a) = Result a
    pick ohNooo = ohNooo
    win a = Result a
```

Now I can *also* select the `MaybeError` type!
If I want to, I can also make it an instance of `IO`:

```haskell
-- simplified. Runs an action and catches an exception.
try :: IO a -> IO (MaybeError a)

instance CanFail IO where
    oops = throwException "oops"
    pick first second = do
        eResult <- try first
        case eResult of
            Result a ->
                return a
            Error exception ->
                second
    win a = return a
```

Now, we can use our `safeDivision` function in `IO`, just like it were `print` or similar!

```haskell
main :: IO ()
main = do
    putStrLn "Woah, look at us!" :: IO ()
    x <- safeDivision 3 2        :: IO Double
    putStrLn "the result was: "
    print x
    y <- safeDivision 3 0
    print "This never happens because we threw an exception!"
```

Return type polymorphism is super cool, and definitely one of the best things about type classes.
It's also one of the things that really sets it apart from interfaces or modules in other languages.
