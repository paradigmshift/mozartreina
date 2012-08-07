---
layout: post
title: Unit Testing in Common Lisp
---
### A Little about the History of Testing in Software Development
According to the [wikipedia entry on testing](http://en.wikipedia.org/wiki/Software_testing#History), testing, or writing unit-tests, sprung from a desire to formally separate debugging (which can be described as "fixing" errors *when they appear*) from assertion or verification (making sure that the code is correct). 

Whereas debugging can be seen as a reactive practice, *testing* is seen as a proactive method in which you don't wait for a bug to show up but actively try to produce one. This can be seen in the [work](http://www.amazon.com/The-Software-Testing-Glenford-Myers/dp/0471043281) of [Glenford Myers](http://en.wikipedia.org/wiki/Glenford_Myers), who emphasized *breakage testing*, or "a successful test is one that finds a bug".

Nowadays the Agile or Extreme development methods have popularized testing by espousing something called [Test-Driven Development](http://en.wikipedia.org/wiki/Test-driven_development), where unit tests are actually written before the code that they are supposed to test.

Common Lisp itself has a very strong background in [Regression Testing](http://en.wikipedia.org/wiki/Regression_testing), tests that, as a primary purpose, ensure that whatever changes have been made, through updates or other channels, don't break existing implementations.

### Lisp-Unit
[Lisp-Unit](http://www.cs.northwestern.edu/academics/courses/325/readings/lisp-unit.html) is a test library for Common Lisp that supports unit tests. This is the library that I've chosen to use for my future projects.

Lisp-Unit can be installed through [Quicklisp](http://www.quicklisp.org/):

<section class="shell">
    CL-USER> (ql:quickload "lisp-unit")
    To load "lisp-unit":
      Load 1 ASDF system:
        lisp-unit
    ; Loading "lisp-unit"

    ("lisp-unit")
</section>

### Usage
Tests are defined with `define-test`:
<section class="code">
**(define-test `\(\mbox{name exp}_1\mbox{ exp}_2\)`...)**
</section>

<section class="shell">
    CL-USER> (defun by-5 (x)
               (* x 5))
    BY-5
    CL-USER> (define-test by-5
               (assert-equal 10 (by-5 2))
               (assert-equal 20 (by-5 4)))
    BY-5
    CL-USER> 
</section>
*here we've defined a custom function `by-5` that takes any number and multiplies it by 5, and defined a test, also called `by-5` which will try to ensure that if we pass 2 and 5 to the function `by-5`, it will return 10 and 20*

And run with `run-tests`:
<section class="code">
**(run-tests)**
</section>

<section class="shell">
    CL-USER> (run-tests)
    BY-5: 2 assertions passed, 0 failed.
    ; No value
</section>

As you can see, the function passed the two test-cases we gave it. If, however, we passed an input that didn't pass the test...

<section class="shell">
    CL-USER> (define-test by-5
               (assert-equal 10 (by-5 2))
               (assert-equal 20 (by-5 4))
               (assert-equal 30 (by-5 10)))
    BY-5
    CL-USER> (run-tests)
    BY-5: (BY-5 10) failed: 
    Expected 30 but saw 50
    BY-5: 2 assertions passed, 1 failed.
    ; No value
</section>

*here we are passing 10 to the function `by-5` and are expecting 30 as a result*

The results say that we were expecting 30 but got 50 instead, 2 tests passed and 1 failed.

This particular example also highlights an important fact, that the tests themselves must be correct for them to be reliable gauges of correctness. The function `by-5` is designed to multiply any digit passed to it by 5, which in this case it did. The test passed it 10 and got 50, but it was expecting 30! This is a case of the test itself being wrong and not the function.

Ok, so now that we've shown a particular example of how this testing library works, let's get down to the specifics: what are the possibl expressions that can be passed to the testing function?

### Assertion Forms
The most common assertion form is expressed in this manner:
<section class="code">
**(assert-equal `\(\mbox{value form}\)`)**
</section>
where value is the expected value that the function is supposed to return, and form is the function that is being tested.

Variables are evaluated in the lexical environment, meaning that you can define a test that examines multiple assertions at the same time.

<section class="code">
{% highlight cl %}
    (define-test my-test
      (dotimes ( i 5)
        (assert-equal i (my-function i))))
{% endhighlight %}
</section>

The other assertion expression most commonly used is `assert-true`, which will fail if the test or condition passed returns false.

<section class="code">
**(assert-true `\(\mbox{test}\)`)**
</section>

<section class="shell">
    CL-USER> (lisp-unit:assert-true (> 7 3))
    T
    CL-USER> (lisp-unit:assert-true (> 7 10))
    (> 7 10) failed: 
    Expected T but saw NIL
    NIL
</section>

### Lisp-Unit in your Packages
Production-level code is almost always defined in its own namespace, or package. In the case of prototype testing or exploratory programming, where the code will usually live in the `cl-user`, then the tests can also be defined here. In all other cases however, it is infinitely better to create a package and usually also a [system](http://mozartreina.com/setting-up-lisp-systems.html) to make deployment and maintenance easier.

Lisp-Unit tests are easily integrated into the package ecosystem. If you have a package that is defined thus:

<section class="code">
{% highlight cl %}
    CL-USER> (defpackage :my-package
               (:use :cl)
               (:export #:my-func-1
                        #:my-func-2))
    #<PACKAGE "MY-PACKAGE">
    CL-USER> (in-package :my-package)
    #<PACKAGE "MY-PACKAGE">
    MY-PACKAGE> (defun my-func-1 (x)
                  .....
                (defun my-func-2 (y)
                  .....
{% endhighlight %}
</section>

Then you can define the testing package in this manner:

<section class="code">
{% highlight cl %}
    CL-USER> (defpackage :my-package-tests
               (:use :cl :lisp-unit :my-package))
    #<PACKAGE "MY-PACKAGE-TESTS">
    CL-USER> (in-package :my-package-tests)
    #<PACKAGE "MY-PACKAGE-TESTS">
    MY-PACKAGE-TESTS> (define-test my-test
                        (dotimes (i 10)
                          (assert-equal i (my-func-1 i))))
    MY-TEST
    MY-PACKAGE-TESTS> (run-tests)
    MY-TEST: 10 assertions passed, 0 failed.
    ; No value
{% endhighlight %}
</section>

And voila! Integration ahs been achieved! You can then take these two `.lisp` files and combine them to form one coherent system using [Quickproject]((http://xach.livejournal.com/278047.html).

### In Closing
While my personal coding style is not anywhere near as test-centric as those found in the Test-Driven-Development and the afore-mentioned Agile and Extreme Programming movements, I do believe that testing has its place in the development of software, especially software that is intended for public use and that will be maintained and extended by other developers.

Libraries such as `Lisp-Unit` make writing these sort of tests easy and relatively pain-free, and I think that it would be a shame not to take advantage of these sort of tools.