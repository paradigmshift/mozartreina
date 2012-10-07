---
layout: post
title: Lazy Evaluation
---

### Lazy and Eager
**Lazy Evaluation** is a technique in which expressions, and the functions in which they're found, are evaluated only when the value that they are supposed to return is needed. This in contrast to **Eager Evaluation**, where the expressions are evaluated immediately during run time.

Let's do a quick example to show how both methods work, say we have a simple function that adds two numbers together:

<section class="code">
{% highlight cl %}
    (defun example (x y)
      (+ x y))
{% endhighlight %}
</section>

Now we'll create a list and populate it with the values returned by this function:

<section class="shell">
{% highlight console %}
CL@USER$ (loop repeat 4
              for x = 1 then (setf x (incf x))
              collect (example x (1+ x)))
(3 5 7 9)
{% endhighlight %}
</section>

As you can see, the list is populated by the results of the function. We first called function and passed it the values 1 and 2, then 2 and 3, and so on. The values were calculated immediately and put into the list.

Now let's see what happens when we use lazy evaluation. First we define a function and make it use lazy evaluation:

<section class="code">
{% highlight cl %}
    (defun lazy-example (x y)
      (lazy (+ x y)))
{% endhighlight %}
</section>

Then we try to populate the list with the results from our lazy function:

<section class="shell">
{% highlight console %}
CL@USER% (loop repeat 4
              for x = 1 then (setf x (incf x))
              collect (lazy-example x (1+ x)))
(#<CLOSURE (LAMBDA # :IN LAZY-EXAMPLE) {10049BECFB}>
 #<CLOSURE (LAMBDA # :IN LAZY-EXAMPLE) {10049BED5B}>
 #<CLOSURE (LAMBDA # :IN LAZY-EXAMPLE) {10049BEDBB}>
 #<CLOSURE (LAMBDA # :IN LAZY-EXAMPLE) {10049BEE1B}>)
{% endhighlight %}
</section>

The list elements are referencing the function objects and not the results! What's going on here?

In this case the function has not been evaluated, and will remain in this state until we tell it explicitly to perform the calculation, like so:

<section class="shell">
{% highlight console %}
CL@USER$ (force (lazy-example 1 2))
3
{% endhighlight %}
</section>

To get the values of the previous list, we just call `force` on all the elements.

<section class="shell">
{% highlight console %}
CL@USER$ (mapcar #'force
          (loop repeat 4
              for x = 1 then (setf x (incf x))
              collect (lazy-example x (1+ x))))
(3 5 7 9)
{% endhighlight %}
</section>

### So What's the Point?
Ok, seems like a cool party trick... but how is this relevant to software development in general? Are there any instances where we wouldn't want the computer to evaluated all functions immediately and only when we specifically tell it to?

#### Case Study #1: Control Structures in Programming Languages
Control Structures are the common  forms **`if`**, **`or`**, etc. found in almost all programming languages. The only way these can be properly implemented is with lazy evaluation, since flow of the program will branch out between two (or more) possible choices and not all will be evaluated.

<section class="shell">
{% highlight console %}
CL@USER$ (defun exp1 ()
           (print "exp1 is evaluated"))
EXP1
CL@USER$ (defun exp2 ()
           (print "exp2 is evaluated"))
EXP2
CL@USER$ (if t
             (exp1)
             (exp2))

"exp1 is evaluated" 
{% endhighlight %}
</section>

As you can see, only the first expression was evaluated, because the **`if`** form prevents the immediate execution of the second expression. Instead, if we place both expressions in a form that executes them immediately:

<section class="shell">
{% highlight console %}
CL@USER$ (and (exp1) (exp2))

"exp1 is evaluated" 
"exp2 is evaluated" 
{% endhighlight %}
</section>

This delay in evaluation is also what enables a form like **`or`** to exist, since **`or`** functions by checking for the returned value from a list of expressions or values. Because **`or`** only needs to verify one instance of `t` to be returned, it checks the list of expressions passed to it one by one until it finds a `t` or any value other than `nil` or `False` (in languages like Python). The expressions that are passed to it are not evaluated immediately because there is no need, only one needs to return a `t` for **`or`** itself to return a `t`. 

Let's see this in action with SBCL:

<section class="shell">
{% highlight console %}
CL@USER$ (defun exp2 ()
           nil)
CL@USER$ (exp2)
NIL
CL@USER$ (macroexpand `(or (exp1) (exp2)))
(LET ((#:G891 (EXP1)))
  (IF #:G891
      #:G891
      (OR (EXP2))))
T
CL@USER$ (or (exp1) (exp2))

"exp1 is evaluated" 
{% endhighlight %}
</section>

Here, with `macroexpand`, we can see exactly how the interpreter/compiler treats the **`or`** form. **`or`** in this case is a macro that expands into an **`if`** form. As we have shown above, **`if`** itself is implemented with lazy evaluation, by association so does **`or`**. If the first evaluation does not produce a [Boolean True](http://en.wikipedia.org/wiki/Boolean_data_type), then recursion takes place with another call to **`or`** and the remaining values passed as arguments.

A better example would be to pass undefined functions. If the undefined function were evaluated, an error would occur:

<section class="shell">
{% highlight console %}
CL@USER$ (exp3)

; in: EXP3
;     (EXP3)
; 
; caught STYLE-WARNING:
;   undefined function: EXP3
; 
; compilation unit finished
;   Undefined function:
;     EXP3
;   caught 1 STYLE-WARNING condition
; Evaluation aborted on #<UNDEFINED-FUNCTION EXP3 {1004F1C1A3}>.
{% endhighlight %}
</section>

Yet, as we shall soon see, **`or`** will not evaluate the undefined function as long as a function that is placed before it in the arguments evalutes to `t`:

<section class="shell">
{% highlight console %}
CL@USER$ (macroexpand `(or (exp1) (exp2) (exp3) (exp4)))
(LET ((#:G894 (EXP1)))
  (IF #:G894
      #:G894
      (OR (EXP2) (EXP3) (EXP4))))
T
CL@USER$ (or (exp1) (exp2) (exp3) (exp4))

"exp1 is evaluated" 
{% endhighlight %}
</section>

If we place the undefined function before a valid function:

<section class="shell">
{% highlight console %}
CL@USER$ (or (exp3) (exp2) (exp1))

; in: OR (EXP3)
;     (EXP3)
; 
; caught STYLE-WARNING:
;   undefined function: EXP3
; 
; compilation unit finished
;   Undefined function:
;     EXP3
;   caught 1 STYLE-WARNING condition
; Evaluation aborted on #<UNDEFINED-FUNCTION EXP3 {100537C653}>.
{% endhighlight %}
</section>

#### Case Study # 2: Costly Operations (and Large Data Sets)
Let's say that you have a program that shows all possible permutations of 4 sets of elements, but in reality only has to calculate one such set, the one that is chosen by the user. Without **Lazy Evaluation**`\(^\text{TM}\)` all the permutations of all the sets would be calculated where only one such set need be, the one chosen by the user.

Here is the cost in terms of resources and time of one such calculation, assuming that the set has 9 elements:

<section class="shell">
{% highlight console %}
CL@USER$ (time (permute '(1 2 3 4 5 6 7 8 9)))
Evaluation took:
  0.702 seconds of real time
  0.666622 seconds of total run time (0.366642 user, 0.299980 system)
  [ Run times consist of 0.507 seconds GC time, and 0.160 seconds non-GC time. ]
  95.01% CPU
  1,190,735,261 processor cycles
  124,387,952 bytes consed
{% endhighlight %}
</section>

So if you have code that looks like this....

<section class="code">
{% highlight cl %}
(defun choose-one ()
  (let* ((a (permute '(1 2 3 4 5 6 7 8 9)))
         (b (permute '(10 11 12 13 14 15 16 17 18 19)))
         (c (permute '(a b c d e f g h i)))
         (d (permute '(j k l m n o p q r)))
         (results (list a b c d)))
    (print "Choose one to permute (a, b, c, or d):")
    (format t "~a" (nth (read) results))))
{% endhighlight %}
</section>

... where all the sets are passed to `permute` *before* the user chooses one, you can just imagine what will happen to your system resources. The fact is, it's all pretty useless because we're interested in only one result.

`\(\ast\)`*Actually the above code won't even run in my copy of SBCL, undoubtedly because the permute function, which I coded, is woefully inefficient, but this just shows what the consequence is of running expensive operations.*

How about if we apply lazy evaluation to the problem? Then the computer would only calculate the permutations once the value is referenced or called, therefore performing only one such calculation.

We just make the permute functions lazy with the `lazy` function (this is not part of the ANSI Standard so you'll have to roll your own) and show the results with the `force` function.

<section class="code">
{% highlight cl %}
(defun choose-one ()
  (let* ((a (lazy (permute '(1 2 3 4 5 6 7 8 9))))
         (b (lazy (permute '(10 11 12 13 14 15 16 17 18 19))))
         (c (lazy (permute '(a b c d e f g h i))))
         (d (lazy (permute '(j k l m n o p q r))))
         (results (list a b c d)))
    (print "Choose one to permute (a, b, c, or d):")
    (time (force (nth (read) results)))))
{% endhighlight %}
</section>

<section class="shell">
{% highlight console %}
CL@USER$ (choose-one)

"Choose one to permute (a, b, c, or d):" 0

Evaluation took:
  1.838 seconds of real time
  0.336644 seconds of total run time (0.319979 user, 0.016665 system)
  [ Run times consist of 0.163 seconds GC time, and 0.174 seconds non-GC time. ]
  18.34% CPU
  3,120,591,136 processor cycles
  124,460,000 bytes consed
{% endhighlight %}
</section>

So here the functions are not evaluated until the user chooses which permutation he wants, then the function tells the computer to do the calculation with the call to `force`. The code runs without problems and only one evaluation is done, conserving resources (and in this case, preventing a crash).

This specific case actually occurs quite often in game AI code, where the AI has to generate a tree of all possible moves and rate them accordingly so it can decide how best to beat the player. Generating a list of all possible moves is incredible expensive, resource-wise, and also quite wasteful, as a majority of the moves calculated will never be made in the game.

Take a chess opening move. The player can move any of his eight pawns, either one or two squares forward. Leaving the other pieces aside for now, this is a total of 16 possible moves. For each of these 16 possibilities, the AI then has to create another tree of possible moves based on each possible move. Say he only moves his pawns as well, he can move any of his eight pawns, one or two squares forward, for **EACH** of the possible moves done by the player. That's `\(16^3\)`, or 4096 possibilities! This quickly gets out of hand, and we're only taking about pawn movements for the first two turns! The clincher though is that only one move will be made, so all other calculated possibilities are thrown away as they are now irrelevant!

The way around this is lazy evaluation, where the specific possibilities are calculated only once the player has made a specific move.

### Implementing Lazy Functions
Several languages, such as [Haskell](http://www.haskell.org/haskellwiki/Haskell) and [Clojure](http://clojure.org/) have lazy evaluation built-in as the default behavior. Common Lisp however, does not, so if you want to take advantage of lazy data structures or lazy functions you have to roll your own implementation. Fortunately, this is easy to do.

When you come down to it, lazy data structures or functions don't *hide* the result of execution or value, *it actually doesn't perform the calculation at all*. With this in mind, you can implement laziness by assigning the functions that you want to be lazy to variables. This is similar to what happens when you pass a function to another function as one of its arguments. The function that is passed is not called, but merely saved as an argument variable by the other function. To evaluate it, you have to use `funcall`, and only then does the interpreter call the function.

<section class="code">
{% highlight cl %}
(defmacro lazy (&body body)
  (let ((value (gensym))
        (forced (gensym)))
    `(let ((,value nil)
           (,forced nil))
       (lambda ()
         (unless ,forced
           (setf ,value (progn ,@body))
           (setf ,forced t))
         ,value))))
{% endhighlight %}
</section>

`\(\ast\)` *Implementation taken from Conrad Barski's Land of Lisp.*

So here you can see one possible implementation of the lazy paradigm. The macro `lazy` takes a series of expressions, usually the innards of a function, and saves those expressions under gensym of value. To evaluate the expressions, it must be called with `funcall`.

<section class="shell">
{% highlight console %}
CL@USER$ (defun add (x y)
           (lazy (+ x y)))
ADD
CL@USER$ (add 1 2)
#<CLOSURE (LAMBDA () :IN ADD) {1003F30A4B}>
CL@USER$ (funcall (add 1 2))
3
{% endhighlight %}
</section>

This way any recursion present in the function is not performed, giving you the ability to create and manage *infinite* data sets.

<section class="shell">
{% highlight console %}
CL-USER> (defparameter *numbers*
           (labels ((f (n)
                      (cons n (lazy (f (1+ n))))))
             (f 1)))
*NUMBERS*
CL@USER$ *numbers*
(1 . #<CLOSURE (LAMBDA # :IN F) {1002DA3AAB}>)
CL@USER$ (funcall (cdr *numbers*))
(2 . #<CLOSURE (LAMBDA # :IN F) {1002FE572B}>)
CL@USER$ (funcall (cdr (funcall (cdr *numbers*))))
(3 . #<CLOSURE (LAMBDA # :IN F) {10030FCCCB}>)
{% endhighlight %}
</section>

As you can see, `*numbers*` returns a list with a number, and the un-evaluated recursion which adds 1 to the number. If this were implemented without the call to `lazy` when doing the recursion, it would loop infinitely until the stack was exhausted or terminated by the user.

This way, you can create what amounts to an *infinite* list without exhausting the stack because the functions don't recurse ***until `funcall` is called on it.*** In effect, it's a stop sign for the function and `funcall` is the green light.

### Consequences
**Lazy Evaluation** is not without its downsides however, the biggest being a performance hit, as the computer now has to store a reference to the computation, and the result once the computation is done (this is called *memoization*, and is incredible useful if when used properly, but can be wasteful when not needed). As such, lazy evaluation should only be used when truly needed, as it can slow down code performance and create bottlenecks.

### Further Reading
[The Real Point of Laziness](http://existentialtype.wordpress.com/2011-04-24/the-real-point-of-laziness/)
[Non-Trivial Lazy Evaluation](http://www.stackoverflow.com/questions/7868507/non-trivial-lazy-evaluation/7868790#7868790)