---
layout: post
title: Soma-RSS, a Lisp-powered RSS feed creator
---
### The Project
As I mentioned previously in [Setting up Jekyll](http://mozartreina.com/setting-up-jekyll.html), I used Jekyll to setup this site, as I felt that using a framework like Django was a bit too much for what I needed.

A feature that I never thought about before but have started thinking about introducing recently is the humble [RSS](http://www.whatisrss.com/) feed. I feel that this has become an indispensable part of personal and commercial sites, aside from having a presence on social networks (something that I don't really feel is necessary for what I'm doing).

Having decided to incorporate it, I had to decide how I would implement it... I could either write the feed file manually and modify it everytime a new post was made, which seemed really stupid, or write some code that would automate the process (makes more sense doesn't it?).

Since I'm not using a database to store the posts and the whole site is static, I would have to write something that would take a directory, scour it for `.html` pages, extract certain information from them (title and some sort of description of the post), and build the [RSS XML](http://www.landofcode.com/rss-tutorials/rss-structure.php) file.

### Why Lisp?
I've implemented this in Lisp since it's been my language of choice for some time now (the only other one that I can claim to be somewhat proficient in is [Python](http://www.python.org/)) and it seemed like a good project to do to practice my (amateurish) skills.

### Implementing it
After looking at the XML structure that an RSS reader expected, I started banging out some code. The first thing I wrote was a function that would output the complete feed, ready for writing to a file on disk.

<section class="code">
{% highlight cl %}
    (defun create-entry (items)
      (let ((out (format nil "<?xml version='1.0'?>
                     <rss version='2.0'>
                     <channel> 
                     <title>Math, Lisp, and general hackery</title> 
                     <description>On-going documentation of my studies and projects</description> 
                     <link>http://mozartreina.com</link> 
                     ~{~a~}
                     </channel>
                     </rss>" items)))
        out))
{% endhighlight %}
</section>

This function would take as its argument a list that contained, pre-formatted, the entries or articles that would be read by the client reader.

This function is actually the final piece, or semi final if you're counting the part that writes the data to a file, of the code that is called. From here I did a complete U-turn and wrote the first part of code that would be called by the program, the part that looked into a directory and accessed the `.html` files.

<section class="code">
{% highlight cl %}
    (defun aggregate (dir path)
      (let ((entries (directory (concatenate 'string dir "/*.html")))
            (parsed-data '()))
        (mapcar (lambda(x)
                  (with-open-file (stream x
                                          :direction :input)
                    (let ((data (make-string (file-length stream))))
                      (read-sequence data stream)
                      (push (item-gen (parse-html data "<title>" "</title>")
                                      (pathname-name x)
                                      (parse-html data "<p>" "</p>")
                                      path)
                            parsed-data))))
                entries)
        parsed-data))
{% endhighlight %}
</section>

This actually started out as a much smaller function but then grew to be some sort of aggregator of the different bits of information returned by other, smaller, functions. It opens the directory passed as its first argument, lists all the `.html` files located inside, then opens them and passes them to several functions for data extraction. The output is a list of RSS entries in XML format that has to be inserted into the main XML feed file.

The functions that work on the information from the `.html` files that are accessed are a crude parser (and when I say crude I mean ***crude***) and another function that takes the parsed data and injects it into the individual XML entry structure.

The parse function (called `parse-html`) is used to extract the title from the `.html` file (if the file has no title tag then the program is screwed) and whatever is written between the first `<p> </p>` tags it encounters, this is used as the description for the entry.

The function itself, as I said, is very raw and  unimaginative. It takes the index location of two strings passed to it (here the `<title>` and `<p>` tags) then returns whatever is between the two strings using the `subseq` built-in function.

The other function responsible for taking the output of the `parse-html` function is called `item-gen`. It generates an XML representation of each post. This is then passed to the first function that was written, `create-entry`, which will take all the entries and creates the complete RSS feed structure.

Function `remove-quote`, which is called by `aggregate`, serves to remove any double quotation marks and `.html` tag marks (`>` and `<`). This is because they cause problems in the internal structure of the XML feed file if included.

This is then written to disk and named `feed.xml`.

<section class="code">
{% highlight cl %}
    (defun remove-quote (str)
      (let ((q (search "\"" str))
            (tag-beg (search "\<" str))
            (tag-end (search "\>" str)))
        (cond (q (progn (setf (char str q) #\space)
                        (remove-quote str)))
              (tag-beg (progn (setf (char str tag-beg) #\space)
                              (remove-quote str)))
              (tag-end (progn (setf (char str tag-end) #\space)
                              (remove-quote str)))
              (t str))))

    (defun parse-html (str beg end)
      (subseq str (search beg str) (search end str)))

    (defun item-gen (title address dscrp path)
      (let ((dscrp (remove-quote dscrp)))
        (format nil "<item>
                   <title> ~a </title>
                   <description> ~a </description>
                   <link> ~a/~a.html </link>
                   </item>" (subseq title 7) (subseq dscrp 3 50) path address)))

    (defun create-file (data)
      (with-open-file (stream "feed.xml"
                              :direction :output
                              :if-exists :supersede)
        (format stream "~a" data)))
{% endhighlight %}
</section>

### Calling the Program from the Command Line
Now unless you want to fire up the `REPL` and load the program everytime you want to use it, you're going to have to find a way to call it from the command line and pass arguments to it.

Enter stage right:
<section class="code">
{% highlight cl %}
    (defun run-from-shell ()
      (create-file (create-entry (aggregate (nth 1 *posix-argv*) (nth 2 *posix-argv*)))))
{% endhighlight %}
</section>

`*posix-argv*` is SBCL's implementation specific method for recalling command-line arguments. We're calling index 1 and 2 because the first argument is the command `sbcl` itself.

<section class="shell">
{% highlight console %}
CL@USER$ *posix-argv*
("sbcl")
{% endhighlight %}
</section>

Now to create the executable, at the `REPL` enter:
<section class="shell">
{% highlight console %}
CL@USER$ (sb-ext:save-lisp-and-die "soma.lisp" :toplevel #'run-from-shell :executable t)
{% endhighlight %}
</section>

`save-lisp-and-die` will save the present runtime environment, including variables, to `soma.lisp`. The `:toplevel` parameter designates the function that is to be invoked when `soma.lisp` is triggered.

### Bugs
Well, like I mentioned before, the program takes a few things for granted, like a `<title>` tag in the `.html` files, for which it has no fail-safe mechanism. A few other things like the main title and link of the feed are hard-coded in, since I basically wrote this for myself. It also assumes that all the `.html` files are found in one place and that the permalink structure is the same for all of them. I did however, take the time to make most of the code generic, allowing it to be used by others with what I hope is *minimal* modification.

The complete code, along with some documentation, can be found in my [GitHub](https://github.com/paradigmshift/soma-rss) repo.

