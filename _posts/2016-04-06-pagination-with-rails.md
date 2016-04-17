---
layout: post
title: Pagination with Rails
---

### Pagination
Pagination is the division of what could be one single page into multiple pages. Although a lot of the time a single page format is advantageous over having multiple pages, when dealing with things like search results or a list of blog posts, it makes more sense from a usability point of view to split them up into discrete pages, as long as you're not displaying a ridiculously low amount of content per page. 10-20 items per page seems to be a decent sweetspot between ultra-long single pages and pagination-with-5-items-gone-crazy-pages.

There *is* something called **infinite scroll**, which looks like a compromise between one-page sites and pagination, you can see it in action on the mobile site of Facebook and some other social media applications. It works by displaying a fixed number of items, like pagination does, but when you reach the bottom of the page the page suddenly **extends** and what was the bottom now becomes the top, with more items loaded below. I don't know the implementation details but it seems (to my amateur eyes) to be an application of [lazy evaluation](https://en.wikipedia.org/wiki/Lazy_evaluation), where the evaluation of an expression (something like `Post.all` for example) is delayed until the program actually needs it (expressions are usually evaluated immediately, as soon as the relevant lexical environments are entered). We'll take that up another day (perhaps in Lisp as I'm not sure that Ruby supports lazy evaluation).

### SQL's offset
SQL has a clause called `offset`, which fetches database rows starting from the row number entered through the `offset` clause. For example, calling `Post.all` would give us all `Post` objects.

    2.0.0-p648 :002 > Post.all
    Post Load (1.6ms)  SELECT "posts".* FROM "posts" ORDER BY "posts"."updated_at" DESC
    => #<ActiveRecord::Relation 
    [#<Post id: 12, title: "l", url: "l", description: "", user_id: 1, created_at: "2016-03-18 13:31:15", updated_at: "2016-03-18 13:31:15", slug: "l">, 
    #<Post id: 11, title: "k", url: "k", description: "", user_id: 1, created_at: "2016-03-18 13:31:10", updated_at: "2016-03-18 13:31:10", slug: "k">, 
    #<Post id: 10, title: "j", url: "j", description: "", user_id: 1, created_at: "2016-03-18 13:31:07", updated_at: "2016-03-18 13:31:07", slug: "j">, 
    #<Post id: 9, title: "i", url: "i", description: "", user_id: 1, created_at: "2016-03-18 13:31:02", updated_at: "2016-03-18 13:31:02", slug: "i">, 
    #<Post id: 8, title: "h", url: "h", description: "", user_id: 1, created_at: "2016-03-18 13:30:58", updated_at: "2016-03-18 13:30:58", slug: "h">, 
    #<Post id: 7, title: "g", url: "g", description: "", user_id: 1, created_at: "2016-03-18 13:30:54", updated_at: "2016-03-18 13:30:54", slug: "g">, 
    #<Post id: 6, title: "f", url: "f", description: "", user_id: 1, created_at: "2016-03-18 13:30:49", updated_at: "2016-03-18 13:30:49", slug: "f">, 
    #<Post id: 5, title: "e", url: "e", description: "", user_id: 1, created_at: "2016-03-18 13:30:45", updated_at: "2016-03-18 13:30:45", slug: "e">, 
    #<Post id: 4, title: "d", url: "d", description: "", user_id: 1, created_at: "2016-03-18 13:30:40", updated_at: "2016-03-18 13:30:40", slug: "d">, 
    #<Post id: 3, title: "c", url: "c", description: "", user_id: 1, created_at: "2016-03-18 13:30:35", updated_at: "2016-03-18 13:30:35", slug: "c">, ...]> 

    2.0.0-p648 :003 > Post.all.size
    (0.2ms)  SELECT COUNT(*) FROM "posts"
    => 12 

Now if you want all the `Post` objects except the first three, using `offset` you would do:

    2.0.0-p648 :015 > Post.all.offset(3)
    Post Load (0.4ms)  SELECT "posts".* FROM "posts" ORDER BY "posts"."updated_at" DESC LIMIT -1 OFFSET 3
     => #<ActiveRecord::Relation [#<Post id: 9, title: "i", url: "i", description: "", user_id: 1, created_at: "2016-03-18 13:31:02", updated_at: "2016-03-18 13:31:02", slug: "i">, 
     #<Post id: 8, title: "h", url: "h", description: "", user_id: 1, created_at: "2016-03-18 13:30:58", updated_at: "2016-03-18 13:30:58", slug: "h">, 
     #<Post id: 7, title: "g", url: "g", description: "", user_id: 1, created_at: "2016-03-18 13:30:54", updated_at: "2016-03-18 13:30:54", slug: "g">, 
     #<Post id: 6, title: "f", url: "f", description: "", user_id: 1, created_at: "2016-03-18 13:30:49", updated_at: "2016-03-18 13:30:49", slug: "f">, 
     #<Post id: 5, title: "e", url: "e", description: "", user_id: 1, created_at: "2016-03-18 13:30:45", updated_at: "2016-03-18 13:30:45", slug: "e">, 
     #<Post id: 4, title: "d", url: "d", description: "", user_id: 1, created_at: "2016-03-18 13:30:40", updated_at: "2016-03-18 13:30:40", slug: "d">, 
     #<Post id: 3, title: "c", url: "c", description: "", user_id: 1, created_at: "2016-03-18 13:30:35", updated_at: "2016-03-18 13:30:35", slug: "c">, 
     #<Post id: 2, title: "b", url: "b", description: "", user_id: 1, created_at: "2016-03-18 13:30:31", updated_at: "2016-03-18 13:30:31", slug: "b">, 
     #<Post id: 1, title: "a", url: "a", description: "", user_id: 1, created_at: "2016-03-18 13:30:27", updated_at: "2016-03-18 13:30:27", slug: "a">]> 

    2.0.0-p648 :007 > Post.all.offset(3).size
    (0.3ms)  SELECT COUNT(count_column) FROM (SELECT 1 AS count_column FROM "posts" LIMIT -1 OFFSET 3) subquery_for_count
    => 9 
    
And as you can see, it ignored the first 3 entries.

Obviously this is not the only way to do it, you could just hit the database with `Post.all` then filter from there. However as much as possible it's a good idea to let the database do the work of querying and returning results, since they're designed and optimized to do just that... I can't imagine you could beat the performance of something that was designed to return *billions* (PostgreSQL) of rows a second.

### SQL's Limit
SQL has another clause called `limit` which, as the name implies, limits the rows returned to a certain number.

    2.0.0-p648 :002 > Post.all.limit(3)
    Post Load (2.1ms)  SELECT "posts".* FROM "posts" ORDER BY "posts"."updated_at" DESC LIMIT 3
    => #<ActiveRecord::Relation [
    #<Post id: 12, title: "l", url: "l", description: "", user_id: 1, created_at: "2016-03-18 13:31:15", updated_at: "2016-03-18 13:31:15", slug: "l">, 
    #<Post id: 11, title: "k", url: "k", description: "", user_id: 1, created_at: "2016-03-18 13:31:10", updated_at: "2016-03-18 13:31:10", slug: "k">, 
    #<Post id: 10, title: "j", url: "j", description: "", user_id: 1, created_at: "2016-03-18 13:31:07", updated_at: "2016-03-18 13:31:07", slug: "j">]> 
    
### Putting it all together
So, with `clause` and `limit`, we now have the necessary building blocks to implement pagination. If we stop and think about it for a while, we would come to the conclusion that we could start with the limiting the objects presented to `\(n\)` objects per page. Let's say we choose 10 as an arbitrary number. We could then define something like this:

*posts_controller.rb*
{% highlight ruby %}
def index
@posts = Post.all.limit(10)
end
{% endhighlight %}

*index.html.erb*
{%highlight ruby %}
<div>
  <% @posts.each do |post| %>
    <%= link_to post.title, post_path(post) %>
  <% end %>
</div>
{% endhighlight %}

This is ok for the first page... but what about when we need to go to the second page? First we need to figure out the number of pages we would have, given the limit (number of items/page) and the offset (the current page we're in).

*posts_controller.rb*
{% highlight ruby %}
def index
@limit = 10 
offset = params[:offset].to_i * @limit ||=0 # offset equals the page we're in, so if we're in the first page 
                                           # offset = 0 * limit, in this case 10, fetching only the first 10 items. 
                                           # Second page, offset = 1 * limit, fetching 10 items after the first 10, etc.
@posts = Post.all.offset(offset).limit(limit)
end
{% endhighlight %}

So now everytime we call the index route, it will check what the `offset` value is in the session cookie, calculate the offset, and return the results based on the result. Now we need to setup number of pages displayed. This varies according to the number of entries, so if the entries are less than 10, only one page link is displayed, more than 10 but less than 20 means 2 pages, etc.

*index.html.erb*
{% highlight ruby %}
....
....
# ceil is called because you want the number rounded up, e.g. 16/10 should equal 2 not 1.6
<%= (Post.all.size.to_f/@limit).ceil.times do |n| %> 
<%= link_to (n + 1).to_s, posts_path(offset: n) %>
<% end %>
{% endhighlight %}

Let's try running through this: on the first page, as we established above, there is no offset value in the session cookie. That means that the `offset` variable = 0, and limit = 10; `@posts` then will equal the first 10 posts. If there are 16 posts in total, `\((16/limit).ceil\) = 2`, so two links will be generated at the bottom of the page that will look somewhat like this:

{% highlight html %}
<a href="posts?offset=0">1</a>
<a href="posts?offset=1">2</a>
{% endhighlight %}

So when the second link is called, the `offset` variable in the session (or `params[:offset]`) is 1, and the `index` method then calculates it's own value offset value which is:
<div>
\[params[:offset] \cdot limit \rightarrow 1 \cdot 10 \rightarrow offset\ = 10 \]
</div>
The expression that assigns a value to `@posts` can then be fully expanded to: `@posts = Post.all.offset(10).limit(10)`, which translates to **"get 10 items AFTER the first 10"**.

### To Conclude
There are times when pagination is not desirable, mainly when dealing with linear real-time events such as Facebook's or Twitter's feed, and in this case the afore-mentioned infinite scroll is superior. In other cases though, such as when you are dealing with query results, pagination is so far the best way to display multiple items.
