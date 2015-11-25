---
layout: post
title: Spacemacs and Rails
---

### Spacemacs Configuration File

I've been using [Spacemacs](https://github.com/syl20bnr/spacemacs) as my default
Emacs setup for a few months now, and aside from the learning curve required in
learning how to setup and configure layers, I've been loving it. However, any
complex system will, at one point or another, require that you dive into the
implementation to figure out the odd error message or for configuration, and
Spacemacs is no different. After a bit of searching and trial and error, I
finally got a working setup going.

So the first thing to do is to make sure that you enable the default Ruby on
Rails layers that come with Spacemacs.

{% highlight elisp %}
dotspacemacs-configuration-layers
'(
  ...
  ...
  (ruby :variables
        ruby-enable-ruby-on-rails-support t)
  ruby-on-rails)
{% endhighlight %}

This ensures that the Ruby on Rails layer (whatever Ruby on Rails mode is the
default of Spacemacs) is loaded and that some default Rails support is also
activated.

### Robe doesn't like You

After setting up `.spacemacs` I reloaded the config file and tried opening a
console with a Rails project. Thus the first snag.

    LoadError: cannot load such file -- pry
    from /home/mo/.emacs.d/elpa/robe-20150126.808/lib/robe/sash/doc_for.rb:1:in require' from /home/mo/.emacs.d/elpa/robe-20150126.808/lib/robe/sash/doc_for.rb:1:in'
    from /home/mo/.emacs.d/elpa/robe-20150126.808/lib/robe/sash.rb:1:in require' from /home/mo/.emacs.d/elpa/robe-20150126.808/lib/robe/sash.rb:1:in'
    from /home/mo/.emacs.d/elpa/robe-20150126.808/lib/robe.rb:1:in require' from /home/mo/.emacs.d/elpa/robe-20150126.808/lib/robe.rb:1:in'

It turns out that the default `Ruby` layer loads a tool called `Robe` that
handles code completion, method definition lookup, and provides information
about loaded classes and modules. So what's going on? The error seems to say
that it can't find `pry` but I definitely have [pry](http://pryrepl.org/)
installed. After a bit of searching I found out that I had to specify `pry` in
the project's `Gemfile`.

{% highlight ruby %}
gem 'pry', :group => :development
{% endhighlight %}

Et voilà, the console started.

    Loading development environment (Rails 4.2.5)
    2.0.0-p647 :001 >  => "robe on 46244" 
    2.0.0-p647 :002 >

### So What Can You Do with It?

Unlike Django, where the database models are written out by hand in the
`models.py` file, Model creation in Rails involves calling the `generate model`
command.

    $ bin/rails generate model Article title:string text:text

This particular command will generate a model called Article with a string field
called title and a text field called text. That's all nice and dandy, but I tend
to forget what fields are present in each model, and when I searched for the
model file, I found this:

{% highlight ruby %}
class Article < ActiveRecord::Base
end
{% endhighlight %}
*app/models/article.rb*

Well that's not very helpful... then later on after more digging around I found
the `db/schema.rb` file that contained more helpful information.

{% highlight ruby %}
ActiveRecord::Schema.define(version: 20151121170213) do
  create_table "articles", force: :cascade do |t|
    t.string   "title"
    t.text     "text"
    t.datetime "created_at", null: false
    t.datetime "updated_at", null: false
  end
end
{% endhighlight %}

Better, but still not very clear. I basically live in Common Lisp's REPL when I
use CL to develop, and I apparently had a Ruby REPL specifically for Rails (Why
else would it be called the Rails console???), so I figured there must be an
interactive way to check the fields in any given model.

[StackOverflow](http://stackoverflow.com/questions/5575970/activerecord-list-columns-in-table-from-console)
to the rescue.

    2.0.0-p647 :049 > Article
     => Article(id: integer, title: string, text: text, created_at: datetime, updated_at: datetime) 
    2.0.0-p647 :054 >  => nil 
    2.0.0-p647 :055 > Article.column_names
     => ["id", "title", "text", "created_at", "updated_at"] 
    2.0.0-p647 :076 > 

Now this I can work with, it's recognizably SQL which makes it easier to reason
with, at least for me.

For now that's all I can really do with it until I figure out the specific ways
that the Rails console relates to Rails projects, but I'm sure there's a lot of
functionality out there that will make a developer's life much easier.

### Additional Commands

The Ruby on Rails layer has some great built-in functions, even though the
keybindings are getting pretty long.

| Key binding | Description |
|:-----------:|:------------|
|SPC m r g c  | Go to current controller |
|SPC m r g d  | Go to DB schema |
|SPC m r g g  | Go to Gemfile |
|SPC m r g m  | Go to current model |
| SPC m r g r | Go to routes |
| SPC m r g v | Go to current view |

These are just a few of the many default keybindings, you can see a complete
list
[here](https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Bframeworks/ruby-on-rails).

