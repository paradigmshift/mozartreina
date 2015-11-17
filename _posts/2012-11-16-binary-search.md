---
layout: post
title: Binary Search & Newton-Raphson Root Finding
---

### The Two Methods and Their Uses
Binary Search is a technique found in the field of Computer Science that is used to find, or search for, an element in a sorted list/array. It is incredibly efficient, with a complexity of `\(O(log\space n)\)`, also known as *[logarithmic run time](http://stackoverflow.com/questions/2307283/what-does-olog-n-mean-exactly)*.

Newton-Raphson is a method for finding the square root of a number, similar to the iterative method of Heron of Alexandria. It involves making an initial guess of the square root, then refining that guess through a specific formula.

So what do these two methods, one an efficient technique to find an element in a list-like structure, and the other a root calculator, have in common?

### Binary Search in-depth (sort of)
Let's first present the Binary Search method, a.k.a the Binary Chop. The technique is actually quite simple to execute. Take a list represented as a line, the list contains the elements from 0 to 10:

<div id="box1" class="jxgbox center-block", style="height:5em; width:30em;"></div>
<script type="text/javascript">
 var board = JXG.JSXGraph.initBoard('box1', {boundingbox: [-1,-1,12,1]});
 var p1 = board.create('point', [0,0], {name:"0 Min"});
 var p2 = board.create('point', [10,0], {name:"10 Max"});
 var li = board.create('line', [p1, p2], {straightFirst: false, straightLast: false});
</script>

Now let's say that the element we want to find is 4. One way to do this would be to go to the beginning of the list and check every element one at a time. If the element we are searching for is right at the beginning we're lucky, if not we could go all the way to the end of the list before finding it.

Binary Search solves this problem by "guessing" where the element will be by adding the least variable it could be (in this case 0) and the largest (10), and getting the average. Thus it's first guess would be 5:

<div id="box2" class="jxgbox center-block" style="height:5em; width:30em;"></div>
<script type="text/javascript">
 var board = JXG.JSXGraph.initBoard('box2', {boundingbox: [-1,-1,12,1]});
 var p1 = board.create('point', [0,0], {name:"0 Min"});
 var p2 = board.create('point', [10,0], {name:"10 Max"});
 var p3 = board.create('point', [5,0], {face:"x", name:"g"});
 var li = board.create('line', [p1, p2], {straightFirst: false, straightLast: false});
</script>

Now it does a check, is 5 the element it was looking for? Nope. Is 5 greater than the element wanted? Yes. So the largest variable (thus far 10), is changed the guess it just made (we know that 5 is larger than the element wanted, so we keep that as the upper bound, lower bound remains unchanged at 0). The rest of the list is thrown away, our new list looks like this:

<div id="box3" class="jxgbox center-block" style="height:5em; width:30em;"></div>
<script type="text/javascript">
 var board = JXG.JSXGraph.initBoard('box3', {boundingbox: [-1,-1,12,1]});
 var p1 = board.create('point', [0,0], {name:"0 Min"});
 var p2 = board.create('point', [5,0], {name:"5 Max"});
 var li = board.create('line', [p1, p2], {straightFirst: false, straightLast: false});
</script>

Again we add the upper bound and lower bound together and get the average (
`\((0 + 5)/2\)`), this is the new guess.

<div id="box4" class="jxgbox center-block" style="height:5em; width:30em;"></div>
<script type="text/javascript">
 var board = JXG.JSXGraph.initBoard('box4', {boundingbox: [-1,-1,12,1]});
 var p1 = board.create('point', [0,0], {name:"0 Min"});
 var p2 = board.create('point', [5,0], {name:"5 Max"});
 var p3 = board.create('point', [2.5,0], {face:"x",name:"g"});
 var li = board.create('line', [p1, p2], {straightFirst: false, straightLast: false});
</script>

Again we check, is the guess greater than the value we're looking for? This time's it's a no... so is it less than the value wanted? Yes, 2.5 is less than 4. In this case, the upper bound is changed to 2.5, our previous guess, while the upper bound remains unchanged. This is the new line:

<div id="box5" class="jxgbox center-block" style="height:5em; width:30em;"></div>
<script type="text/javascript">
 var board = JXG.JSXGraph.initBoard('box5', {boundingbox: [-1,-1,12,1]});
 var p1 = board.create('point', [2.5,0], {name:"2.5 Min"});
 var p2 = board.create('point', [5,0], {name:"5 Max"});
 var li = board.create('line', [p1, p2], {straightFirst: false, straightLast: false});
</script>

Again the upper bound and lower bound are added and averaged to produce our new guess. The average is 3.75:

<div id="box6" class="jxgbox center-block" style="height:5em; width:30em;"></div>
<script type="text/javascript">
 var board = JXG.JSXGraph.initBoard('box6', {boundingbox: [-1,-1,12,1]});
 var p1 = board.create('point', [2.5,0], {name:"2.5 Min"});
 var p2 = board.create('point', [5,0], {name:"5 Max"});
 var p3 = board.create('point', [3.75,0], {face:"x",name:"g"});
 var li = board.create('line', [p1, p2], {straightFirst: false, straightLast: false});
</script>

As you can see, as the formula iterates, the list gets smaller and smaller as more of it is thrown away and the guesses get closer and closer. Eventually (in the next two iterations) the code will find the element it was looking for. This method is extremely useful in large data sets, where the lists searched have elements that number in the hundreds if not thousands.

#### Lisp Implementation

<section class="code">
{% highlight cl %}
(defun chop (val lst &optional guess)
  (cond ((not guess) (chop val lst (nth (ash (length lst) -1) lst)))
        ((and (eq (length lst) 1) (not (equal val guess))) (print "not found"))         
        ((> guess val) (progn (print guess)
                              (chop val (subseq lst 0 (ash (length lst) -1)))))
        ((< guess val) (progn (print guess)
                              (chop val (subseq lst (ash (length lst) -1)))))
        (t guess)))
{% endhighlight %}
</section>

We're printing out the computer's guesses so that we can see how well the algorithm performs. The syntax is as follows: `chop` is followed by the value that we're looking for and the list from which we perform the search.

<section class="shell">
{% highlight console %}
CL@USER$ (chop 6 '(1 2 3 4 5 6))

4 
5 
6
CL@USER$ (chop 21 (loop for x from 1 to 1000 collect x))

501 
251 
126 
63 
32 
16 
24 
20 
22 
21
{% endhighlight %}
</section>

In the first run, we use quite a small list, only 6 elements, and it gave us the value after 2 guesses. In the second one, we used a list of 1,000 elements and asked it to find the element `21`. Here it took 9 guesses... not bad for such a big list. Now how about if we give it a list of 10,000 elements? Will it take 10 times as much (90 guesses) as the list with 1,000 elements?

<section class="shell">
{% highlight console %}
CL@USER$ (chop 101 (loop for x from 1 to 100000 collect x))

50001 
25001 
12501 
6251 
3126 
1563 
782 
391 
196 
98 
147 
122 
110 
104 
101
{% endhighlight %}
</section>

Well it took more than 9... 4 more to be exact! Nowhere close to 10 times as much, even though the previous list was 10 times smaller. This tells us that this particular algorithm is incredibly efficient and is perfect for dealing with [Big Data](http://www-01.ibm.com/software/data/bigdata/).

### Newton-Raphson

In it's original form, the Newton-Raphson formula for finding roots is this:

$$ X_1 = X_0 - \frac{f(X_0)}{f'(X_0)} $$

This is an interactive formula, where the next value of `\(X_1\)` is found by substituting the present value of `\(X_1\)` into the algorithm as `\(X_0\)`.

#### Lisp Implementation and Sample Run

<section class="code">
{% highlight cl %}
(defun nr (rt guess)
  (if (> (abs (- (expt guess 2) rt)) 0.01)
      (progn (print guess)
            (nr rt (- guess (/ (- (expt guess 2) rt) (* guess 2.0)))))
    (print guess)))
{% endhighlight %}
</section>

This will print out every guess the program makes until it comes up with a *reasonably* accurate answer.

<section class="shell">
{% highlight console %}
CL-USER> (nr 25 1)

1 
13.0 
7.4615383 
5.406027 
5.0152473 
5.0000234 
5.0000234
{% endhighlight %}
</section>

We can see that it's very quick and can produce an incredibly accurate answer (in this case the accuracy is hard-coded into the code and will give us an answer that is within 0.01 to the correct answer, this of course can be adjusted).

### Comparing Newton-Raphson and Binary Search for Root Finding

We haven't yet written an example that shows how Binary (or Bisection) Search can be used for root finding...

<section class="code">
{% highlight cl %}
(defun chop (val guess &optional limit)
  (cond ((not limit) (chop val guess val))
        ((< (abs (- (expt guess 2) val)) 0.01) guess)
        ((> (expt guess 2) val) (progn
                                  (print (/ guess 2.0))
                                  (chop val (/ guess 2.0) guess)))
        (t (progn
                                                 (print (/ (+ guess limit) 2.0))
                                                 (chop val (/ (+ guess limit) 2.0) limit)))))
{% endhighlight %}
</section>

This is quite crude, but will work for demonstration purposes. We're also printing out each guess the code makes so that we can see how the guesses progress. Let's see what the code can do when given the same root to find as the Newton-Raphson algorithm.

<section class="shell">
{% highlight console %}
CL@USER$ (chop 25 2)

7.25 
3.625 
5.4375 
2.71875 
4.078125 
4.7578125 
5.0976563 
2.5488281 
3.8232422 
4.460449 
4.7790527 
4.9383545 
5.0180054 
2.5090027 
3.763504 
4.3907547 
4.70438 
4.8611927 
4.939599 
4.978802 
4.9984035 
5.0082045 
2.5041022 
3.7561533 
4.382179 
4.6951914 
4.851698 
4.929951 
4.969078 
4.9886413 
4.9984226 
5.0033135 
2.5016568 
3.7524853 
4.377899 
4.690606 
4.84696 
4.9251366 
4.964225 
4.9837694 
4.9935417 
4.9984274 
5.0008707 
5.0008707
CL@USER$
{% endhighlight %}
</section>

So we can see that it does give a decent answer, the Newton-Raphson code though is much more accurate when set with the same precision test (< 0.01), and much quicker. Our binary search algorithm took 43 guesses while Newton-Raphson came up with the answer after 6 guesses.

Is there a way that the two can be combined to further improve what the Newton-Raphson can algorithm can do? What can the Binary Search technique bring to the table?

### Using Binary Search's Upper and Lower Bounds to Prevent Drift

Binary Search's min and max limit can be used to ensure that if the guess provided by the Newton-Raphson algorithm is outside of the boundaries (since we know that no number's square can be more than half of that number we use that as the initial upper limit), then instead of using that guess for the next iteration, we take the number between the upper and lower limits and use that instead. That way we can avoid unnecessary iterations (although, to be honest, in the few trials I've run I've only ever found the improvement to be one iteration less at best).

Implementation of the combined algorithms:

<section class="code">
{% highlight cl %}
(defun nr-new (rt guess)
  (- guess (/ (- (expt guess 2) rt) (* guess 2.0))))

(defun nr-chop (rt)
  (labels ((find-root (val guess upper-b lower-b)
                      (print guess)
                      (cond ((< (abs (- (expt guess 2) val)) 0.01)
                             guess)
                            ((not (and (> guess lower-b) (< guess upper-b)))
                             (find-root val (/ (+ upper-b lower-b) 2.0) upper-b lower-b))
                            ((> (expt guess 2) upper-b)
                             (find-root val (nr-new val guess) guess lower-b))
                            (t (find-root val (nr-new val guess) upper-b guess)))))
    (find-root rt (/ rt 4.0) (/ rt 2.0) 2)))
{% endhighlight %}
</section>

In the `nr-chop` function, we first set accuracy to 0.01. If the guess is not good enough we then check to see if `\(\text{lower} < \text{guess} < \text{upper}\)`, which means that we're making sure that the guess falls within the known boundaries. If it doesn't, then we discard the guess and instead use the midpoint between the boundaries as the next guess. If the guess falls within the boundaries but is greater than the upper boundary when squared, we then set the guess as the new upper boundary. If it is less than the upper boundary when squared, we instead set reset the lower boundary to the guess.

I've also set the initial guess to the midpoint of the starting boundaries (beginning upper boundary is half of the given number), though you can change this around and it will not affect the number of iterations (that's the point of combining the two algorithms).

First we'll compare the algorithms when a fourth of the value entered as it's initial guess.

In action:
<section class="shell">
{% highlight console %}
CL@USER$ (nr-chop 100)

25.0 
14.5 
10.698276 
10.022788 
10.000026 
10.000026
CL@USER$ (nr 100 25)

25 
14.5 
10.698276 
10.022788 
10.000026 
10.000026
CL@USER$

{% endhighlight %}
</section>

Ok so that was expected, after all they're basically using the same underlying formula so they should come up with the same solution in the same number of iterations.

How about if we switch up the initial guess? 

<section class="shell">
{% highlight console %}
CL@USER$ (nr 100 99)

99 
50.00505 
26.002424 
14.92411 
10.812339 
10.030516 
10.000047 
10.000047
CL@USER$
{% endhighlight %}
</section>

We can see that the pure version iterated 6 times when given 99 as it's initial guess. Can the hybrid do better?

<section class="code">
{% highlight cl %}
(defun nr-chop (rt)
  (labels ((find-root (val guess upper-b lower-b)
                      (print guess)
                      (cond ((< (abs (- (expt guess 2) val)) 0.01)
                             guess)
                            ((not (and (> guess lower-b) (< guess upper-b)))
                             (find-root val (/ (+ upper-b lower-b) 2.0) upper-b lower-b))
                            ((> (expt guess 2) upper-b)
                             (find-root val (nr-new val guess) guess lower-b))
                            (t (find-root val (nr-new val guess) upper-b guess)))))
    (find-root rt (- rt 1) (/ rt 2.0) 2)))
{% endhighlight %}
</section>

<section class="shell">
{% highlight console %}
CL@USER$ (nr-chop 100)

99 
26.0 
14.923077 
10.812054 
10.030495 
10.000047 
10.000047
CL@USER$
{% endhighlight %}
</section>

As you can see, we changed the initial guess to the value entered minus 1 (in this case 99). However, the algorithm rejected the first guess given by the Newton-Raphson formula, which would have been 50.00505 as when we ran the pure formula, but instead substituted the initial guess with 26 (upper boundary + lower boundary divided by 2). This enabled one less iteration and slightly more accurate guesses (actually the algorithm did iterate the same number of times, it just rejected the initial guess and gave a new one).

### That's the Great Optimization?
If this were the end-all-be-all for the hybrid algorithms involving Newton-Raphson and binary search then it would have a died an ignoble death almost immediately, yet this combination is used in the field (in graphing calculators for example) quite often.

#### Non-convergence
There are actually scenarios where the Newton-Raphson formula cannot provide a [clear answer](http://www.damtp.cam.ac.uk/lab/people/sd/lectures/nummeth98/figure4.htm#FF_4). However, this happens when you try to find the root of a **polynomial** or a system of equations.

For example, we implemented Newton-Raphson to look for square roots as follows:

<div> $$
\text{guess}_1 = {guess}_0 - \frac{\text{number entered} - \text{guess}^2}{\text{guess}^2}
$$ </div>

This is quite a simple expression and the derivative is stable, ensuring that the formula converges on a point with ever increasing accuracy.

If however, you try to solve for the root of this particular function with Newton-Raphson:

<div> $$
f(x) = x^3 - 2x + 2
$$ </div>

Initial guess is `\(0\)`

<div>
First iteration:
`\( x_1 = x_0 - \frac{f(x)}{f'(x)} = 0 - \frac{2}{-2} \\
x_1 = 1\)`
</div>

<div>
Second iteration:
`\( x_2 = 1 - \frac{f(1)}{f'(1)} = 1 - \frac{1}{1} \\
x_2 = 0\)`
</div>

Uh oh... we an see exactly where this is going... the points oscillate between 0 and 1.

#### Binary Search to the Rescue
How about if we used the binary search method? First thing is to find the upper and lower bounds, with polynomials this means two points that have opposite signs (positive and negative).

`\( f(0) = 0^3 - 2(0) + 2 = 2 \)`

So one point is `\(0\)`

`\( f(-2) = _2^3 - 2(-2) + 2 = -2 \)`

The other point is `\(-2\)`

So the root is between `\(0\)` and `\(-2\)`:

`\( \frac{0 + -2}{2} = -1 \)`

Now plug that in to the equation: `\( -1^3 - 2(-1) + 2 = 3 \)`. Since 3 is positive and we have to maintain one positive and one negative result as the upper and lower bounds for the root, `\(-1\)` takes the place of `\(0\)` as the upper limit. 

Iterate

`\( \frac{-1 + -2}{2} = -1.5 \)`.

Plugging in `\(-1.5\)` results in `\(1.625\)`, another positive number, so again `\(-1.5\)` replaces `\(-1\)` as the upper limit. On the next iteration, `\(-1.75\)` is the midpoint and yields `\(0.140625\)` when plugged in. Eventually you will end up with `\(-1.769\)`, which, when plugged into the equation:

<div>
`\[ (-1.769)^3 - 2(-1.769) + 2 = 0.0021 \]`
</div>

Close enough I think that we can declare `\(-1.769\)` as the root for the polynomial.

Whew! So as you can see, what's happened is that due to a **"bad"** initial guess, the Newton-Raphson formula failed to find a solution, while the binary search method just kept plodding along.

This is precisely the reason that graphing calculators contain a hybrid algorithm for root finding. If the root to be found is a root of a fixed integer or float, then the Newton-Raphson method is brutally effective. Change that to the root of a polynomial and things can become problematic, as the algorithm can start to oscillate between two points without converging (there are a few other monkey wrenches as well).

### Further Reading

* [Newton-Raphson](http://www.math.ubc.ca/~anstee/math184/184newtonmethod.pdf)
* [Bisection Method for Finding the Root of a Polynomial](http://en.wikipedia.org/wiki/Bisection_method#Example:_Finding_the_root_of_a_polynomial)
