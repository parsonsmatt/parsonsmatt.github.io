---
title: "OOPH: Data Inheritance"
date: 2017-02-17
layout: post
categories: programming
---

(this post is part of a series on Object Oriented Programming in Haskell -- see [tutorials](/tutorials/) for a table of contents)

> Does Haskell have inheritance?

Well, no, it doesn't, because Haskell does not have objects, and inheritance is a relationship between two objects.
Objects are a combination of internal state (data) and methods (behavior).
Since inheritance is a combination of both of these things, we'll need to treat them separately.

This post will approach data inheritance, since it is somewhat easier to deal with.

tl;dr: Decompose OO features into simple parts, reconsitute with FP


# The Objects: Java

Let's consider a simple example of data-only inheritance.
We'll use Java for this example.

```java
class Shape {
    public final int x;
    public final int y;

    public Shape(int x, int y) {
        this.x = x;
        this.y = y;
    }
}

class Circle extends Shape {
    public final int radius;
    
    public Circle(int x, int y, int radius) {
        super(x, y);
        this.radius = radius;
    }
}
```

Here we've defined a class `Shape` that contains the X and Y coordinates where it is located.
We extend `Shape` to create the `Circle` class, which also has a `Radius`.
Whenever we have a `Circle`, or any other class that extends `Shape`, we know we can access the `x` and `y` properties on that class.

In the introduction, I mentioned that inheritance can be broken down into simpler features.
Let's put on our "Objects and Messages" thinking caps, and think about what we *mean* with these constructs.

```java
Circle c = new Circle(1, 2, 3);
```

We'll read this as:

> Create a new Circle object with the values 1, 2, 3.

```java
c.x;
```

Generally, this is read as "access the property `x` on the object `c`."
Let's instead read it as:

> Send the message `x` to the object `c`.

This gets us more in the "pure" object oriented sense of things.
So, when we send the message `x` to the object `c`, how does it know how to respond?

First, it'll do a lookup on all of it's attributes.
`Circle` only has a `radius` attribute.
It doesn't have an `x` attribute.
It's not time to give up, though.
The next thing it does is delegate the message to the parent class.
`Shape` does have `x` defined, so we can respond with that value.

What if we do this: `c.toString()`?
Well, `Shape` doesn't have `toString()` on it's list of attributes defined.
Java (and most OOP languages) have an implicit `Object` class that is the superclass of all objects.
`Object` does have `toString()` defined, so `c` delegates to `Shape`, which then delegates to `Object`.

Super classes, mixins, traits, etc. are all -- conceptually -- just an automatic form of delegation.
"If I don't know how to handle something, ask this object."

If we were to ban the `extends` keyword in Java, then we could recover the same functionality with object composition:

```java
class Shape { 
    private final int x;
    private final int y;

    public Shape(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public int getX() {
        return this.x; 
    }

    public int getY() {
        return this.y; 
    }
}

class Circle {
    private final Shape shape;
    private final int radius;

    public Circle(int x, int y, int radius) {
        this.shape = new Shape(x, y);
        this.radius = radius;
    }

    public int getX() {
        return this.shape.getX(); 
    }

    public int gety() {
        return this.shape.getY(); 
    }

    public int getRadius() {
        return this.radius; 
    }
}
```

Oof, that's a lot of boilerplate.
No wonder people prefer inheritance.
In Java, you'd probably want to define an interface so that you can use `Shape`s polymorphically, but we'll ignore that for now.

# Port to Haskell

Now that we've simplified the implementation of data inheritance in Java to the core features of OOP, it's easy to port it to Haskell.

```haskell
data Shape = Shape { x :: Int, y :: Int }

data Circle = Circle { shape :: Shape, radius :: Int }
```

That's it!
We just compose two bits of data. 
That's the easy way.
However, we have some flexibility problems.

To access the `Circle`'s `x` parameter, we need to compose functions:

```haskell
circleX = x . shape
circleY = y . shape
```

Ideally, we'd like to be able to the `x` and `y` functions work on any type that has those attributes.
Haskell's solution for name overloading is the type class.

# Class it up!

Here's a new definition of our types:

```haskell
data Shape = Shape 
    { shapeX :: Int
    , shapeY :: Int 
    }

data Circle = Circle 
    { circleShape :: Shape
    , circleRadius :: Int 
    }
```

Now, we're prefixing the record fields with the type name.
This saves the more general names for the type classes.
For each field that we want to be polymorphic about, we create a type class and an instance for the classes:

```haskell
class HasX a where
    x :: a -> Int

instance HasX Shape where
    x = shapeX

instance HasX Circle where
    x = shapeX . circleShape
```

Now, we can use the function `x` on any type that we instantiate it for.
We can even retrofit existing types:

```haskell
instance HasX (Int, Int) where
    x = fst
```

This is reminiscent of monkey patching in Ruby, but rather than being a gross hack, it's an elegant way to extend types with new functionality.

# Modification of values

We've covered immutable *reading* of values, but we also want to be able to update values.
The idiomatic Java is presented here, with most of the boilerplate:

```java
class Shape {
    private int x;
    private int y;

    public Shape(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public int getX() { 
        return this.x; 
    }

    public void setX(int x) { 
        this.x = x; 
    }
}

class Circle extends Shape { 
    // pretend I'm not too lazy to
    // write out the boilerplate
}
```

Now, we want to be able to translate:

```java
Circle c = new Circle(1, 2, 3);
c.setX(4);
```

into idiomatic Haskell.

First, we'll have to translate the above into using immutable objects, since Haskell's data types are immutable.
Let's see what that looks like:

```java
class Shape {
    private final int x;
    private final int y;

    public Shape(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public Shape setX(int x) {
        return new Shape(x, this.y); 
    }

    // etc...
}
```

Rather than modifying the old Shape, we return a new Shape with the field changed.
Modifying a Circle is pretty similar.
We'll switch back to using object composition as well:

```java
class Circle {
    private final Shape shape;
    private final int radius;

    public Circle(int x, int y, int radius) {
        this.shape = new Shape(x, y);
        this.radius = radius;
    }

    public Circle setX(int x) {
        return new Circle(
            x,
            this.shape.getY(),
            this.radius
        );
    }
}
```

Alright, now we can directly translate this to Haskell:

```haskell
class SetX a where
   setX :: Int -> a -> a

instance SetX Shape where
    setX x (Shape _ y) = Shape x y

instance SetX Circle where
    setX x (Circle shape radius) = 
        Circle (setX x shape) radius
```

Now we can express a modification function:

```haskell
modifyX :: (HasX a, SetX a) => (Int -> Int) -> a -> a
modifyX f a = setX (f (getX a)) a
```

Neat!

# Lenses

So, you may notice that the boilerplate involved with the polymorphic accessors and setters is pretty intense.
Lenses solve this issue nicely.
I won't go into depth on how great lenses are and why learning them has made my life so much better, but I will recommend [this tutorial](https://hackage.haskell.org/package/lens-tutorial-1.0.2/docs/Control-Lens-Tutorial.html) and [this lengthy blog series](https://artyom.me/lens-over-tea-1).

Classy lenses work especially well for this problem:

```haskell
data Shape = Shape
    { _shapeX :: Int
    , _shapeY :: Int
    }

data Circle = Circle
    { _circleShape :: Shape
    , _circleRadius :: Int
    }

makeClassy ''Shape
makeClassy ''Circle

instance HasShape Circle where
    shape = circleShape
```

which give us some pretty nice helpers:

```haskell
someShape = Shape 1 2
someCircle = Circle someShape 3

moveShape = over x (+1) . over y (+1)
```

And we can use `moveShape` on both a `Shape` and a `Circle`, and truly, anything that implements the `HasShape` type class.

# Summary

The somewhat-mechanical process that we followed to reach this point was:

1. Take the fancy OOP features
2. Boil them down to the core features: objects and messages
3. Translate to immutable values
4. (somewhat) mechanically translate to Haskell

I suspect that this process will work fairly well for most of the things we'll run into on this series.
