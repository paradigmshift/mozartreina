---
layout: post
title: Setting up Jekyll
---
For a while now I've been thinking of using **[Jekyll](https://github.com/mojombo/jekyll/wiki)** for my online work. Previously I'd only ever used **[Django](https://www.djangoproject.com/)** but it seemed like a bit of overkill for what I wanted to do here.

There are numerous tutorials out there for starting out with Jekyll, and for those that already have a running blog and just want to migrate, **[this](http://paulstamatiou.com/how-to-wordpress-to-jekyll)** is an  excellent guide (based on a Wordpress - Jekyll migration).

So what is Jekyll? Jekyll is a static website generator... in layman's terms, it will create a website out of, and according to, your text files. This article, for example, is written in a specific syntax/language called **[Markdown](http://daringfireball.net/projects/markdown/)**. Markdown runs the text given to it and modifies it according to its syntax. Markdown is integrated into Jekyll, so Jekyll takes this text file, runs markdown, then takes the result and further modifies it to create a standards-compliant html file. This means that you don't have to worry about writing raw HTML for every blog post or web page that you create and can just focus on writing the content.

So what are the benefits of using Jekyll instead of using a full-blown CMS for your blogging, personal websites, etc?

1. No database involved - all files are in html format. This keeps things simple and accessible, you can search for articles with **`grep`** for example.

2. Github integration - there are numerous tutorials out there that cover using Jekyll and Githug pages to host your blog. All you have to do is push your content to Github to update your site.

3. Files are in text format - one of the biggest advantages to this is that you can write your new pages/entries with your favorite text editor instead of going through a web interface. I, for example, am using emacs to do all my writing.

4. Static html - any half-decent webserver can serve 1000s of request / SECOND of static html files. If your website generates high traffic this becomes critical, websites that are generated on the fly on the other hand, can take up to 0.5 seconds to serve a single request. This is why a lot of blogs that are using PHP, Python, or any other language to create the requested pages on demand can all of a sudden have huge down times when they become popular; thousands or even just hundreds  of users requesting pages at the same time.

So what are the basic requirements?

- the [Ruby](http://www.ruby-lang.org/en/) programming language
- [rdiscount](https://github.com/rtomayko/rdiscount) - ruby implementation of markdown
- [pygment](http://pygments.org/) - cody syntax highlighting (optional)

**Jekyll** and **rdiscount** can both be installed using ruby gems.

<section class="shell">
    $ gem install jekyll
    $ gem install rdiscount
</section>

Jekyll expects a certain directory structure, which in its simplest incarnation is:

<section class="shell">
    drwxr-xr-x   _includes
    drwxr-xr-x   _layouts
    drwxr-xr-x   _posts 
    -rw-r--r--   _config.yml
</section>

The *_config.yml* file can actually be slightly complex, for now just put this in it:

<section class="code">
    auto: true
    markdown: rdiscount
    base_url: /
</section>

Here we're just telling Jekyll to use **rdiscount** as it's markdown implementation, that the base url is **`/`** (you will change this once you deploy the site, this is just for development), and that everytime you make a change to any of the files, the Jekyll server will rebuild the site automatically.

To start, create a basic *default* template page, which will for the basis for future pages, and put it in the *_layouts* folder. Mine for example is:

<section class="code">
{% highlight html %}
{% raw %}
    <!DOCTYPE html>
    <html>
    <head>
      <meta charset=utf-8 />
      <title> {% if page.title %} {{ page.title }} | {% endif %} Mozart Reina </title>
      <link rel="stylesheet" href="/css/styles.css" type="text/css" />
      <link rel="stylesheet" href="/css/syntax.css" type="text/css" />
    </head>
    <body>
    
      <header id="top">
        <h1> Math, Lisp, and general hackery </h1>
      </header>

      <nav role="navigation">
        <ul>
          <li><a href="/">Home</a></li>
          <li><a href="/entries/">Entries</a></li>
          <li><a href="/about/">About</a></li>
        </ul>
      </nav>
    
      <div id="main">
        {{ content }}
      </div>
      <footer>
        <p>@copy; Mozart Reina 2012 | All Rights Reserved. </p>
      </footer>
    </body>
    </html>
{% endraw %}
{% endhighlight %}
</section>

You'll notice the non-html tags **`{% raw %} {% if page.title %} {{ page.title }} | {% endif %} {% endraw %}`**, this is part of the **[Liquid](http://liquidmarkup.org/)** templating lanuage that Jekyll incorporates. It's a way of introducing code into your html templates and pages. In this case it's just telling Jekyll that if there is a page title to print it out, followed by a vertical bar.

But how does Jekyll recognize the page title?

Create an *index.html* file in the root directory and put this in:

<section class="code">
{% highlight html %}
{% raw %}
---
layout: default
title: Hey!
---
Hey there!
{% endraw %}
{% endhighlight %}
</section>

The triple dash `---` should be at the beginning of the file, no newlines before or after. Everything between the dashes are **[YAML front matter](https://github.com/mojombo/jekyll/wiki/YAML-Front-Matter)**, basically information that Jekyll processes. In this particular example, we're telling Jekyll to use the default layout (the one you just wrote in the _layouts folder) and that the title is "Hey!". When Jekyll reads this, it will create an html file from your default layout and insert "Hey!" wherever there is a {% raw %}**`{{ page.title }}`** {% endraw %}tag.

Everything after the triple dashes are content, so in the index file about to be created, Jekyll will place "Hey there!" in the section where we put {% raw %} **`{{content }}`** {% endraw %} in the layout file.

Now run the Jekyll server and point your browser to **[http://localhost:4000](http://localhost:4000)**:

<section class="shell">
    $ jekyll --server
</section>

You should see a page based on your default template with the content you specified in your *index.html* file.