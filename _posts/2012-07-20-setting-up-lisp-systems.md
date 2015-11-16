---
layout: post
title: Setting up Lisp Systems
---
### What is a Lisp System?
The common definition of a Lisp System is actually an implementation, or dialect, of Lisp itself. SBCL for example is a Lisp system based on the ANSI standard. For our purposes however, we will be using it in the same way that it is used in [Zach Beane's post on Quickproject](http://xach.livejournal.com/278047.html), which forms the basis of this article. Actually this is just a paraphrasing of Zach's post, which I recommend that every beginner (and maybe not-so-beginners) read.

The terminology is slightly vague, it's probably best described as being akin to a library, as it is designed to be called by [ASDF](http://common-lisp.net/project/asdf/), but it does not have to be a set of user tools (which in my mind is what a library is, a set of reusable functions and/or class objects). Project would be a good way to define it, and that's actually the term the original article uses, but since ASDF itself uses the command `defsystem` to define it, we will use that term.

Essentially it is code that can be loaded by ASDF (along with ***any other dependencies*** that are described in the project files) and therefore used in other projects.

### Enough with the Etymology
So let's get started... we will assume that you have already loaded and installed [Quicklisp](http://www.quicklisp.org/), if not go ahead and do so.

So ASDF2, the latest incarnation of ASDF, will search for systems in the default directory **`~/.local/share/common-lisp/source`**. If this is where your lisp code lives or will be living then you don't need to do any further setup. If however, you want to put your code in another directory (mine for example lives in **`~/dev/lisp/`**) then create the following file in the following directory:

 `~/.config/common-lisp/source-registry.conf.d/projects.conf`

Add this to the file (in this example we will be using my toplevel lisp directory):
<section class="code">
{% highlight cl %}
    (:tree (:home "~/dev/lisp/"))
{% endhighlight %}
</section>

This tells ASDF that all your "systems" are located here.

Load [Quickproject](https://github.com/xach/quickproject) through Quicklisp and create a project:

<section class="shell">
{% highlight console %}
CL@USER$ (ql:quickload "quickproject")
To load "quickproject":
  Load 1 ASDF system:
    quickproject
; Loading "quickproject"

("quickproject")
CL@USER$<(quickproject:make-project "/home/mo/dev/lisp/my-tools/")
"my-tools"
{% endhighlight %}
</section>

The last directory in the argument passed to `make-project` is created, if not already existing, and is also used as the project name. `make-project` then populates the directory with four files:

<section class="shell">
{% highlight console %}
|-README.txt
|-my-tools.asd
|-package.lisp
|-my-tools.lisp
{% endhighlight %}
</section>

The project can now be called by ASDF and Quicklisp:

<section class="shell">
{% highlight console %}
CL@USER$(ql:quickload "my-tools")
To load "my-tools":
  Load 1 ASDF system:
    my-tools
; Loading "my-tools"
[package my-tools]
("my-tools")
{% endhighlight %}
</section>

Of course the source files are empty so nothing's going to happen, but at least we have now understood an easy way of creating Lisp systems.

### Where does the Code go?
So now that we have a system that can be called and loaded by ASDF, and therefore also Quicklisp, we should start writing down some code... but where do we put it? And what are all the other files for?

So the functions and internal workings of the project will be put in `my-tools.lisp`

<section class="code">
{% highlight cl %}
(defun switch (a b lst)
  "Switch elements at position a & b with each other"
  (let ((newlst (copy-list lst)))
    (psetf (nth a newlst) (nth b newlst)
           (nth b newlst) (nth a newlst))
    newlst))

(defun permute (lst)
  ;; reduces the results from start-algo function
  (let ((final nil))
    (mapcar (lambda (x)
              (if (= (length x) (length lst))
                  (push x final)))
            (start-algo lst))
    final))

(defun start-algo (lst)  
  (let ((container nil))
    (push (list (car lst) (cadr lst)) container)
    (push (switch 0 1 (car container)) container)
    (setf lst (cddr lst))
    (dotimes (n (length lst))
      (mapcar (lambda (x)
                (setf x (push (car lst) x))
                (push x container)
                (dotimes (i (1- (length x)))
                  (push (switch i (1+ i) (car container)) container)))
              container)
      (setf lst (cdr lst)))
    container))
{% endhighlight %}
</section>

This is some old code that I wrote that lists all possible permutations of a list. It's incredibly clunky and inefficient, but it will serve for our demo purposes.

So now that we have the code in `my-tools.lisp`, what about the other files? `README.txt` is pretty self-explanatory, but what does `package.lisp` contain?

`(quickproject:make-project)` automatically creates a namespace for our project and puts that in `package.lisp`. These are the default contents:

<section class="code">
{% highlight cl %}
;;;; package.lisp

(defpackage #:my-tools
  (:use #:cl))
{% endhighlight %}
</section>

This is also where you would include any external libraries or projects if you want to call them without package prefixes. As Zach's original post has an example of this, I won't be including it here (why then am I writing this if one can read the original article? good question...).

Now on to `my-tools.asd`. This is the file that describes to ASDF what the project consists of and what external projects have to loaded upon loading our project.

<section class="code">
{% highlight cl %}
;;;; my-tools.asd

(asdf:defsystem #:my-tools
  :serial t
  :components ((:file "package")
               (:file "my-tools")))
{% endhighlight %}
</section>

If we want to call an external library, for example [Drakma](http://weitz.de/drakma/), then we would add `:depends-on (#:drakma)` like so:

<section class="code">
{% highlight cl %}
;;;; my-tools.asd

(asdf:defsystem #:my-tools
  :serial t
  :depends-on (#:drakma)
  :components ((:file "package")
               (:file "my-tools")))
{% endhighlight %}
</section>

So let's try loading (or reloading) our new system. Remember, `my-tools.lisp` now contains our code so we should be able to call the functions in it.

<section class="shell">
{% highlight console %}
CL@USER$ (ql:quickload "my-tools")
To load "my-tools":
  Load 1 ASDF system:
    my-tools
; Loading "my-tools"

("my-tools")
{% endhighlight %}
</section>

Now to test the functions found in `my-tools.lisp`:

<section class="shell">
{% highlight console %}
CL@USER$ (my-tools::permute '(1 2 3 4))
((4 1 2 3) (1 4 2 3) (1 2 4 3) (1 2 3 4) (4 1 3 2) (1 4 3 2) (1 3 4 2)
 (1 3 2 4) (4 3 1 2) (3 4 1 2) (3 1 4 2) (3 1 2 4) (4 2 1 3) (2 4 1 3)
 (2 1 4 3) (2 1 3 4) (4 2 3 1) (2 4 3 1) (2 3 4 1) (2 3 1 4) (4 3 2 1)
 (3 4 2 1) (3 2 4 1) (3 2 1 4))
CL@USER$
{% endhighlight %}
</section>

It works! We can now load independent systems (and all their dependencies) through Quicklisp, making them ridiculously easy to use in other projects.

### Namespace

There is one glaring problem with the way we have organized the code however... if we try to call the function without the namespace prefix like so...

<section class="shell">
{% highlight console %}
CL@USER$ (permute '(1 2 3 4))
; in: PERMUTE '(1 2 3 4)
;     (PERMUTE '(1 2 3 4))
; 
; caught STYLE-WARNING:
;   undefined function: PERMUTE
; 
; compilation unit finished
;   Undefined function:
;     PERMUTE
;   caught 1 STYLE-WARNING condition
; Evaluation aborted on #<UNDEFINED-FUNCTION PERMUTE {1004BCECE3}>.
{% endhighlight %}
</section>

... the `REPL` will complain. Of course we can just do what we did earlier and call it with double colons `my-tools::permute` but this is generally considered bad form, [read this post on packages](http://www.bookshelf.jp/texi/onlisp/onlisp_27.html) for the why. Instead we can add this to `package.lisp`.

<section class="code">
{% highlight cl %}
;;;; package.lisp

(defpackage #:my-tools
  (:use #:cl)
  (:export #:permute))
{% endhighlight %}
</section>

Now the function is publicly available, we can call it with a single colon. First we reload the whole system, then try the newly available function.

<section class="shell">
{% highlight console %}
CL@USER$(ql:quickload "my-tools")
To load "my-tools":
  Load 1 ASDF system:
    my-tools
; Loading "my-tools"
[package my-tools]
("my-tools")
CL@USER$(my-tools:permute '(1 2 3 4))
((4 1 2 3) (1 4 2 3) (1 2 4 3) (1 2 3 4) (4 1 3 2) (1 4 3 2) (1 3 4 2)
 (1 3 2 4) (4 3 1 2) (3 4 1 2) (3 1 4 2) (3 1 2 4) (4 2 1 3) (2 4 1 3)
 (2 1 4 3) (2 1 3 4) (4 2 3 1) (2 4 3 1) (2 3 4 1) (2 3 1 4) (4 3 2 1)
 (3 4 2 1) (3 2 4 1) (3 2 1 4))
{% endhighlight %}
</section>

### Adding source files
For larger projects you'll probably split the code between several files, e.g. when I was experimenting with writing a simple CMS, I split the code into four files:

* models.lisp which was where the content layout, etc. was described
* urls.lisp which dealt with creating the individual urls of each page
* views.lisp which contained code for viewing the posts
* init.lisp, the part that called the dependencies and the individual source files

To load your individual files at the time that the system is called, you update the system definition and add in the files with the `:components` parameter.

<section class="code">
{% highlight cl %}
(asdf:defsystem #:my-tools
  :serial t
  :depends-on (#:drakma)
  :components ((:file "urls")
               (:file "models")
               (:file "views")
               (:file "init")
               (:file "package")
               (:file "my-tools")))
{% endhighlight %}
</section>

One thing to keep in mind is that since the `:serial` parameter is set to `t`, the files will load sequentially, so you have to order them in such a way that the files that are not dependent on each other are called first.

### Bundling your Systems with Newly-Created Systems

You might create a whole new project but are dependent on some previous system's code, e.g. again using my CMS as an example, I had to load the system `aserve`, which is a  webserver called [AllegroServe](http://www.franz.com/support/documentation/current/doc/aserve/tutorial.html) by the guys over at [Franz](http://www.franz.com).

You can do this by specifying the wanted library/project/system when you create a new project.

<section class="shell">
{% highlight console %}
 CL@USER$ (quickproject:make-project "~/dev/lisp/my-cms/"
                       :depends-on '(aserve))
 "my-cms"
 CL@USER$
 CL@USER$ (net.aserve:start :port 8000)
 #<NET.ASERVE:WSERVER port 8000 {1002C5F453}>
 CL@USER$
{% endhighlight %}
</section>

As you can see, once the system `aserve` is loaded, the packages that are bundled with it, like `net.aserve`, become available.

### From here...
There are a few other things that are found on Zach's page that I haven't discussed, again I highly recommend going over [his post](http://xach.livejournal.com/278047.html) on the subject.

Happy Hacking!
