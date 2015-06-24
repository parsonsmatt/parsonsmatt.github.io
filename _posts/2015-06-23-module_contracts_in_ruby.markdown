---
title: "Module Contracts In Ruby"
date: 2015-06-23
layout: post
categories: programming
---

## An Unobtrusive Bit of Safety

Programming to interfaces is pretty awesome.
Ruby's duck typing makes it easy to determine the interface that a dependent object needs.
Once you know the interface, you do dependency injection and duck-type.
Everything is wonderful, DRY, and the clear interfaces make it easy to test and reuse.

Ruby has a means of mixing in functionality to implement a duck-type, but it does not have the means of *specifying* a duck.
Java lets you treat concrete implementations as instances of an interface.
Haskell lets you define type classes where certain functions are defined for your data type.
Both of these are means of programming generically while retaining confidence about the correctness of the code relying on the interface.

I make a lot of mistakes, and I love it when the code blows up as close to the mistake as possible.
Ideally, either at compile-time or when the tests run.
Can we bring this functionality into Ruby, without compromising "Tell, don't ask", existing code, and the flexibility of duck typing?

## Yes!

Before implementing it, let's write out how we want to use it.
I'll start with a basic Ruby class that injects a dependency and uses it.

```ruby
class Spaceship
  def initialize(engine)
    @engine = engine
    @height = 0
    @speed = 0
  end

  def launch
    until @height > 100 do
      @height += @speed
      @speed += @engine.accelerate
      puts "YEAAAAAH SPAAAACE"
    end
    @engine.launch_fireworks
  end
end
```

Careful study of the above code indicates that the engine is expected to respond to `accelerate` and `launch_fireworks`.
Let's formalize that in a module.

```ruby
module SpaceEngine
  def accelerate
    5 * use_fuel
  end

  def launch_fireworks
    use_fuel
    puts "hooray space!"
  end
end
```
  
Now, if we want to make something that a Spaceship can use, it can include the module and be set.
We might inspect our current implementation, and note that both `accelerate` and `launch_fireworks` rely on a common message: `use_fuel`.

(you will not be going to space today)
