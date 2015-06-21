---
title: "Announcing Squirrell"
date: 2015-06-21
layout: post
categories: programming
---

## A kinda magical query gem

The application we're building where I work has a pretty complex data model. Our ActiveRecord models where getting more and more complex finders and scopes, with no end in sight. One particular query was unacceptably slow with ActiveRecord and too complex for Arel, so we had to drop down to SQL. On an instance method of a model.

Yeah, it was kinda gross. I didn't like it. I also couldn't really find a good place to put the code, either, and I wasn't able to find a gem or convention that handled this. So I decided to make one: [Squirrell](https://www.github.com/parsonsmatt/squirrell)!

## Query Object

A query object is a class whose responsibility is to query the database and return the result. ActiveRecord models make fine query objects, but have perhaps too much flexibility and power. Mocking `find`, `where`, `find_by` and variants leads to brittle test code, especially if you ever need to `includes`! Additionally, having such easy arbitrary access to the database spread throughout the application can make it difficult to pinpoint dependencies and interfaces.

Squirrell's query objects have an extremely simple API: `.find`. This makes it very easy to encapsulate their behavior, which makes for much easier testing. As a brief demonstration, I'll write up a simple query object for finding a user:

```ruby
class UserFinder
  include Squirrell

  requires :id

  def finder
    User.find(@id)
  end
end
```

And in the controller, we'd call it like:

```ruby
def show
  @user = UserFinder.find(id: params[:id])
end
```

And in our controller spec, we'd stub it like:

```ruby
expect(UserFinder).to receive(:find).and_return(instance_double(User))
```

So far, this is just delegating the find method to the User.
The real benefit comes when we need to change something on the query.
Perhaps `UsersController#show` now needs to include all of a user's friends in order to avoid an N+1 query.
We only need to change the query code in the `UserFinder` class. 
The controller doesn't need to change, and the controller spec doesn't need to change.

Contrast that with having the call to `User.find` directly in the controller.
If we want to add `includes(:friends)`, we'd need to alter the code in the controller *and* the spec.
Or let the spec hit the database, and then it's slow.

## Arel

Squirrell really shines when you're using it with Arel queries.
It provides a convenient location to put the code and a clean interface for using it.
Here's an example of how you'd use that:

```ruby
class NumberOfLikedPosts
  include Squirrell

  requires :user_id

  def arel
    users = User.arel_table
    posts = Post.arel_table
    likes = Like.arel_table

    users.join(likes).on(users[:id].eq(likes[:user_id]))
         .join(posts).on(likes[:post_id].eq(posts[:like_id]))
         .where(users[:id].eq(@user_id))
         .project(count(posts[:id))
  end
end
```

To use this, it's just `ArelExample.find(user_id: 1)` and it returns the result of the query.
ActiveRecord will return a pretty basic object, which you'll likely want to modify.
Squirrel provides a `process` hook that is called with the result of the query.
The following process method converts the above query result into an integer:

```ruby
class NumberOfLikedPosts
  # ...
  def process(result)
    result[:count].to_i
  end
end
```

## SQL

The main motivation for this gem was using raw SQL queries in Rails in a convention-over-configuration manner.
The query that needed to be optimized in SQL ended up with a 25x speed increase over ActiveRecord, but the code wasn't well organized and didn't have a clear API.
Testing it was tricky, and stubbing it even worse.

With Squirrell, it's great!

```ruby
class AverageAgeOverMinimum
  include Squirrell

  requires :min_age

  def raw_sql
    <<-SQL

SELECT COUNT(*) AS total, AVG(users.age) AS average
FROM users
WHERE users.age > #{@min_age}

    SQL
  end
end
```

And this query is executed with `ComplexSqlQuery.find(min_age: 18)`.
