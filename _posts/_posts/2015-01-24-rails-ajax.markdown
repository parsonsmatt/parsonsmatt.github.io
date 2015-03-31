---
title: Rails, Ajax, and you!
date: 2015-01-24
layout: post
category: programming
---

## Or, well, me

There are a bunch of tutorials online for implementing ajax functionality in Rails. It makes for a rather nice user experience, and provides a bit of a performance benefit from the perspective of the user. I know I enjoy using websites that provide this functionality. I pieced together the functionality that I wanted from a number of tutorials and the Rails guides, and I wanted to create a guide to show how I implemented them.

## Example application

The code for this will be demonstrated in a minimal Rails app. Check it out on [Github][repo]. For reference, I'll be using the following things:

* Ruby 2.2.0
* Rails 4.2.0

If you're viewing this from the future, this will hopefully make things more sensible for you.

### Getting Started

Create a new Rails app, setup your RVM/rbenv/chruby/etc as you like it, and we're set. In the tradition of basic Rails tutorials, this will be a blog with a Post.

{% highlight bash %}
$ rails generate scaffold Post title:string body:text
$ rake db:migrate
{% endhighlight %}

Now we have the default MVC for Posts. Fantastic. Let's make it work ajax-style.

### Expected Behavior

Here's how I want it to work:

1. When a user clicks 'New Post', they'll get the form injected into the page.
2. When a user submits 'New Post', it will POST the request to the server.
3. When the server processes the post, it will either re-render the form wth errors or remove the form and update the index.

## The `new` action

### The Controller

First, we'll need to enable responding to JavaScript requests in the controller. To do this, we create a familiar `respond_to` call in the `new` action. It'll look like this:

{% highlight ruby %}
# app/controllers/posts_controller.rb
...
def new
  @post = Post.new
  
  respond_to do |format|
    format.html
    format.js
  end
end
...
{% endhighlight %}

The controller will now respond to "script" requests with the `app/views/posts/new.js.erb` by convention. 

### The View

To request this, it's super simple. Edit the `index.html.erb` file and add `remote: true` to the `link_to` for New posts.

{% highlight ruby %}
# app/views/posts/index.html.erb
...
<%= link_to 'New Post', new_post_path, remote: true %>
...
{% endhighlight %}

Clicking "New Post" doesn't do anything, because we don't have a `new.js.erb` file yet. Create `app/views/posts/new.js.erb` with the following:

{% highlight javascript %}
alert("New post!");
{% endhighlight %}

Now, clicking the New link causes an alert to show up. That's all well and good, but we want to make a new post, not annoy users with `alert`s.

### Injecting a form

Our first step is to render the form. `render 'form'`. Easy, right? Rails provides a method `escape_javascript` which allows you to easily work with HTML in your embedded Ruby JavaScript files. `escape_javascript` is aliased to `j` for convenience. We'll then wrap that all up in a nice little jQuery object for convenience. We'll create a container `div` to wrap it, append the whole thing to the body, and be set. The result:

{% highlight javascript %}
var form = $('<%= j(render 'form') %>');
var wrapper = $('<div>').attr('id', 'new-post-form').append(form);
$('body').append(wrapper);
{% endhighlight %}

Not too bad. Clicking "New Post" now slaps the form into the page. Of course, you can click "New Post" multiple times and it just continues appending forms willy-nilly.

We want the form to submit over ajax as well, so edit the `form_for` method in the `_form.html.erb` and add `remote: true`. 

{% highlight ruby %}
<%= form_for @post, remote: true do |f| %>
{% endhighlight %}

This won't work, because we haven't setup the create action yet.

## Creation

### The Controller

As above, all we need to do is add `format.js` into the `respond_to` block for both branches of the if statement. This tells the server to respond with `create.js.erb` 

### create.js.erb

Alright, so what do we want to do?

* If the post saved successfully, 
    * then remove the form and update the table.
    * else re-render the form with errors.

We'll select the old form's div, then enter the check to see if there are errors. If there are, then we'll replace the div's html with the re-rendered form. If not, we'll render the post partial and insert it into the table.

{% highlight javascript %}
var oldForm = $('#new-post-form');
<% if @post.errors.any? %>
  oldForm.html('<%= j( render 'form' ) %>');
<% else %>
  oldForm.remove();
  $('tbody').prepend('<%= j(render @post) %>');
<% end %>
{% endhighlight %}

Actually running this will now cause an error, as the post partial hadn't been made yet. Silly Rails scaffold! Let's refactor the index page a bit. Replace the `<% @posts.each do |post| %>` loop with the following:

{% highlight ruby %}
# app/views/posts/index.html.erb
...
<tbody>
  <%= render @posts %>
</tbody>
...
{% endhighlight %}

and create `app/views/posts/_post.html.erb`:

{% highlight ruby %}
# app/views/posts/_post.html.erb
<tr>
  <td><%= post.title %></td>
  <td><%= post.body %></td>
  <td><%= link_to 'Show', post %></td>
  <td><%= link_to 'Edit', edit_post_path(post) %></td>
  <td><%= link_to 'Destroy', post, 
                   method: :delete, 
                   data: { confirm: 'Are you sure?' } 
       %>
  </td>
</tr>
{% endhighlight %}

Rails is clever enough to render each post using the partial, and we got some reuse in our script. New posts and creating posts now works as you'd want!

## Discussion

There are some neat benefits to this: 

### Portability

You can do a `<%= link_to 'New Post', new_post_path, remote: true %>` anywhere on your site and it will inject the form for you. That might not be great for a `Post`, but for a `ReportContent` or other model that would be great.

### Lightweight

Rails only needs to render much smaller objects for these, rather than a full application page. This is a good bit faster, both for your server and the client.

### Easy to implement

If you just want limited ajax for a few models, then this works great and is rather easy to setup.

It's not perfect, though:

### JavaScript!

Yeah, you're now having to keep track of two views for new/create, one of which is injecting content int your page. Does your site even have a place for it?

### Middle ground

This is a middle-ground solution. It's sitting somewhere inbetween a full-HTML solution and a single page application. If you were just sending JSON back and forth, then you'd have much easier time managing the view.

### Gets hairy quick

If you want to do something more complicated, this can get real tricky fast.

## Further thoughts:

* In my implementation of this for [MelodyScout][melodyscout] (on [Github][ms-repo]), I'm using the new/create to manage the insertion of a Bootstrap modal window. This looks and works very well!
* Since I want to have forms that work both over HTML and ajax, I added a `remote` parameter to my `_form.html.erb` object that would default to false unless passed. Forms rendered by default work over html, but the code `<%= render 'form', remote: true %>` makes it ajax.


[repo]: https://www.github.com/parsonsmatt/rails-ajax rails-ajax
[melodyscout]: https://www.melodyscout.com MelodyScout
[ms-repo]: https://www.github.com/melodyscout/melodyscout MelodyScout Repository
