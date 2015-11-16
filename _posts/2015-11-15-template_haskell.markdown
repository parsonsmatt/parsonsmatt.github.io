---
title: "Template Haskell Is Not Scary"
date: 2015-11-15
layout: post
categories: programming
---

I learned about the power and utility of metaprogramming in Ruby.
Ruby metaprogramming is done by constructing source code with string concatenation and having the interpreter run it.
There are also some methods that can be used to define methods, constants, variables, etc.

In my [Squirrell](https://github.com/parsonsmatt/squirrell/) Ruby library designed to make encapsulating SQL queries a bit easier,
I have a few bits of metaprogramming to allow for some conveniences when defining classes.
The idea is that you can define a query class like this:

```ruby
class PermissionExample
  include Squirrell

  requires :user_id
  permits :post_id

  def raw_sql
    <<SQL
SELECT *
FROM users
  INNER JOIN posts ON users.id = posts.user_id
WHERE users.id = #{user_id} #{has_post?}
SQL
  end

  def has_post?
    post_id ? "AND posts.id = #{post_id}" : ""
  end
end
```

and by specifying `requires` with the symbols you want to require, it will define an instance variable and an attribute reader for you, and raise errors if you don't pass the required parameter.
Accomplishing that was pretty easy.
Calling `requires` does some bookkeeping with required parameters and then calls this method with the arguments passed:

```ruby
def define_readers(args)
  args.each do |arg|
    attr_reader arg
  end
end
```

Which you can kinda read like a macro: take the arguments, and call `attr_reader` with each.
The magic happens later, where I overrode the `initialize` method:

```ruby
def initialize(args = {})
  return self if args.empty?

  Squirrell.requires[self.class].each do |k|

    unless args.keys.include? k
      fail MissingParameterError, "Missing required parameter: #{k}"
    end

    instance_variable_set "@#{k}", args.delete(k)
  end

  fail UnusedParameter, "Unspecified parameters: #{args}" if args.any?
end
```

We loop over the arguments provided to `new`, and if any required ones are missing, error.
Otherwise, we set the instance variable associated with the argument, and remove it from the hash.

Another approach involves taking a string, and evaluating it in the context of whatever class you're in:

```ruby
def lolwat(your_method, your_string)
  class_eval "def #{your_method}; puts #{your_string}; end"
end
```

This line of code defines a method with your choice of name and string to print in the context of whatever class is running.

## wait this isn't haskell what am i doing here

Metaprogramming in Ruby is very much based on a textual approach to code.
You use Ruby to generate a string of Ruby code, and then you have Ruby evaluate the code.
JavaScript's metaprogramming is similar, though much less common.

If you're coming from this sort of background (as I was), then Template Haskell will strike you as different and weird.
You'll think "Oh, I know, I'll just use quasi quoters and it'll all work just right."
Nope.
You have to think very differently about metaprogramming in Template Haskell.
You're not going to be putting strings together that happen to make valid code.
This is Haskell, we're going to have some compile time checking!

## Constructing an AST

In Ruby, we built a string, which the Ruby interpreter then parsed, turned into an abstract syntax tree, and interpreted.
In Haskell, we'll skip the string step.
We'll build the abstract syntax tree directly using standard data constructors.
GHC will verify that we're doing everything OK in the construction of the syntax tree, and then it'll print the syntax tree into our source code before compiling the whole thing.
So we get two levels of compile time checking -- that we built a correct template, and that we used the template correctly.

One of the nastiest things about textual metaprogramming is that there's no guarantee that your syntax is right -- and it can be really hard to debug when doing more complicated stuff.
Programming directly into an AST makes it a lot easier to verify the correctness of what we write.
The quasiquoters are a convenience built around AST programming, but I'm of the opinion that you should learn the AST stuff first and then dive into the quoters when you have a good idea of how they work.

Alright, so let's get into our first example.
We've written a function `bigBadMathProblem :: Double -> Double` that takes a lot of time at runtime, and we want to write a lookup table for the most common values.
Since we want to ensure that runtime speed is super fast, and we don't mind waiting on the compiler, we'll do this with Template Haskell.
We'll pass in a list of common numbers, run the function on each to precompute them, and then finally punt to the function if we didn't cache the number.

```haskell
deriveLookupTable numbers =
```
