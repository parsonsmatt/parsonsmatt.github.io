---
layout: post
title: "Nested Forms in Rails"
date:   2015-01-10
categories: programming
---

## A beginner's approach

I've got a project called TellMe, which is a music release notification service. The app does pretty much exactly what I want it do, and now all I need to do is code up some user friendly UIs and deploy it for great victory. I've been keeping all of my models pretty thin, which means a lot of related models. This calls for nested forms.

The simplest example of this is the relation between my `Release` and `ReleaseDate` classes:

{% highlight ruby %}
# app/models/release.rb
class Release < ActiveRecord::Base
  has_many :release_dates, dependent: :destroy
end

# app/models/release_date.rb
class ReleaseDate < ActiveRecord::Base
  belongs_to :release
end
{% endhighlight %}

The `ReleaseDate` class contains information about when a `Release` comes out, items that may be relevant for filtering, and is responsible for kicking off the daily release notification process. 

## The Form

I want it to be great. It should do something like this:

* Release Name: [text field]
* Release Description: [text area]
* Release Dates:
  * Date: [date] Country: [country] Delete? [ ]
  * Date: [date] Country: [country] Delete? [x]
  * Add Release Date

When you click 'Add Release Date', it should insert a new bullet above it for a new release date with all relevant information. How can we accomplish this?

## Back End

The first step is to allow the `Release` class to accept nested attributes in forms.

{% highlight ruby %}
# app/models/release.rb
class Release < ActiveRecord::Base
  has_many :release_dates
  accepts_nested_attributes_for :release_dates, allow_destroy: true
end
{% endhighlight %}

`allow_destroy` will allow us to destroy the object from the nested form. We want this. There's another attribute called `update_only` which prevents the nested form from creating new objects.

Second, the controller needs to be modified to allow these new parameters in:

{% highlight ruby %}
# app/controllers/releases_controller.rb
...
  def release_params
    params.require(:release).permit(:name, :description,
              release_dates_attributes: [:id, :country, :date, :_destroy])
  end
...
{% endhighlight %}

If `params[:release_date_attributes[:_destroy]]` evaluates to a truthy value, then the record will be marked for deletion. All of these parameters will get updated in a single transaction during `@release.save`.

Cool! The back end of the app now works pretty much exactly like I want it to. It gets a params has with a bunch of release date information and updates/creates/destroys accordingly.

## The View

Rails is smart enough to do some pretty cool stuff behind-the-scenes with nested form objects thanks to the `accepts_nested_attributes_for` method above. The `form_builder` object has a method `fields_for` which handles much of the hard work. My form code looks something like this at first:

{% highlight ruby %}
<%= form_for @release do |f| %>
  ...
  <ul>
    <%= f.fields_for :release_dates do |ff| %>
      <li>
        <%= ff.label :date %>
        <%= ff.date_field :date %>
        <%= ff.hidden_field :id %>
        <%= ff.label :_destroy %>
        <%= ff.check_box :_destroy %>
      </li>
    <% end %>
    <li>
      <%= link_to 'Add Date', '#' %>
    </li>
  </ul>
  ...
{% endhighlight %}

Rails is, as usual, rather intelligent. It knows that Release has-many ReleaseDates and that `@release.release_dates` is going to be an array. It will pull all of the objects out of the array and create those fields for each of them. If there aren't any objects in the collection, then it won't create anything. That is pretty cool! But it doesn't let us create new ones -- that's why I've added the  `link_to 'Add Date'` up there. We're going to come back to it and write up some JavaScript to make it work.

## Dynamic Forms

First, I want the 'Add Date' link to work dynamically. I'll add `remote: true` to the options. This causes Rails to implement the link as an AJAX request rather than true link. Where will the link go? Since I'm creating a new `ReleaseDate` associated with a `Release`, I'll make a route specifically for that: `new_release_release_date_path`. The `release_dates_controller` will need to set the release appropriately for the view. Lastly, I'll need an actual `new.js.erb` to implement the change.

Here are the changes:

{% highlight ruby %}
# app/views/releases/_form.html.erb
...
<%= link_to 'Add Release Date', new_release_release_date_path(@release), remote: true %>
...

# config/routes/.rb
...
resources :releases do
  resources :release_dates, only [:new, :destroy]
end
...

# app/controllers/release_dates_controller.rb
class ReleaseDatesController < ApplicationController
  before_action :set_release, only: [:new, :destroy]

  def new
    @release_date = @release.release_dates.build
  end

  def destroy
  end

  private

    def set_release
      @release = Release.find(params[:release_id])
    end

end
{% endhighlight %}
{% highlight javascript %}
# app/views/release_dates/new.js.erb
console.log("hello from <%= @release.name %>!");
{% endhighlight %}

Now, when I click on the link, the console gets a new message. So this is working exactly as I want it to so far!

## The JavaScript

First, I want to insert a new `li` into the form. This will hold the form for the new field. jQuery makes this fairly easy:

{% highlight javascript %}
// app/views/release_dates/new.js.erb
$('li#add-date').before(
  $('li').
    attr('class', 'release_date').
    append('hello!')
);
{% endhighlight %}

This selects the `li` with `id='add-date'`, goes before it in the list, and inserts a new `li` with `class='release_date'` and contents `hello!`. So now, I just need to append the form to the `li` and everything will be set, right? Unfortunately, while the following code looks right, it doesn't quite work:

{% highlight javascript %}
$('li#add-date').before(
  $('<li>').
    attr('class', 'release_date').
    <%= fields_for @release_date do |rd| %>
      append('<%= rd.label :date %>').
      append('<%= rd.date_field :date %>')
    <% end %>
);
{% endhighlight %}

Clicking the link creates the new li and it has a form that looks pretty much right, but the data attributes aren't correct, and the information doesn't get included in the `release_date_attributes` hash in the params.

Check out the resulting HTML:

{% highlight html %}
...
<!-- Original generated form: -->
<label for="release_release_dates_attributes_3_date">Date</label>
<input value="2015-01-06" type="date" name="release[release_date_attributes][3][date]"
  id="release_release_dates_attributes_3_date">
...
<!-- AJAXed form: -->
<label for="release_date_date">Date</label>
<input type="date" name="release_date[date]" id="release_date_date">
{% endhighlight %}

And when you click the link multiple times, the AJAX forms are going to have the same IDs and names, rendering them worthless. So it won't be that easy! One solution would be to use the new JavaScript code to scan the previous fields, capture the right elements, and add them. That appears to be precisely the solution that [Cocoon](https://rubygems.org/gems/cocoon) uses, and it also includes a bunch of helper methods to make the Rails code rather nice.
