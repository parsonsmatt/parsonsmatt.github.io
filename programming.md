---
layout: page
title: Programming
permalink: /programming/
---

An index of all of my programming blog posts.
I mostly talk about Haskell and functional programming.
At some point, I should really curate these into a more usable section.
Until then, though, a big ol' index dump is fine.

<div class="content list">
{% assign programmingPosts = site.posts |  where:"categories", "programming" %}
{% for post in programmingPosts %}
  <div class="list-item">
    <h2 class="list-post-title">
      <a href="{{ site.baseurl }}{{ post.url }}">{{ post.title }}</a>
    </h2>
    <div class="list-post-date">
      <time>{{ post.date | date_to_string }}</time>
    </div>
  </div>
{% endfor %}
</div>
