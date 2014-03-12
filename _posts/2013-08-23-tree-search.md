---
layout: post
title: Searching Trees
---
### Tree-like Data Structures
Trees are data structures that have a parent-child, or hierarchical,
architecture. Binary trees, JSON, HTML, and programming languages'
[AST](http://www.cse.ohio-state.edu/software/2231/web-sw2/extras/slides/21.Abstract-Syntax-Trees.pdf) are examples of such structures, and the ease of creating
parsers for such constructions make them ubiquitous in the field of
computing.

The methods (and code) discussed here are based on a chapter of Peter
Norvig's [Paradigms of Artificial Intelligence Programming](http://norvig.com/paip.html), those
of you who have read PAIP will find much of the code familiar.

<img class="centerimage" src="/img/binary-tree.gif" />
<p class="centertext">A Binary Tree</p>

### A Problem of Search
The abundance of these objects, and the way they are put to use, mean
that it is imperative that there be an efficient method to search and
reason about them. This is an old problem, as these structures have
been around for about as long as the field of Computer Science itself,
and over the decades several methods have been perfected, their use
dependent on the problem space and how much we know about them
beforehand.

### The Search algorithm
There are many search methods but the main algorithm remains the same:
check the current state to see if current state matches the goal, if
not then get the next nodes from the current state and append them to
other the other states that weren't checked, and run the algorithm
again.

There needs to be a function that will sort the un-checked states
before the main algorithm checks them, this is so that we can optimize
the amount of time we spend on the problem.

We also need a function that will produce the next nodes, or
successors,  from the current state.

<section class="code">
{% highlight cl %}
(defun tree-search (states goal-p successor combiner)
  (print states)
  (cond ((null states) nil)
        ((funcall goal-p (first states)) (first states))
        (t (tree-search (funcall combiner (funcall successor (first states))
                                 (rest states))
                        goal-p successor combiner))))
{% endhighlight %}
</section>

Here we have a function `tree-search` that takes a list of states to
check, `goal-p` which is the function that checks to see if the state
being looked at is the goal or not, the successor function which
passes the successive nodes from the current state to the combiner,
and the combiner function then combines the successive states with the
other states that weren't looked at and passes this list as the new
list of states that have to be checked.

<section class="code">
{% highlight cl %}
;; this is a function that will give the successors of a
;;node in a binary tree
(defun binary-tree (x)
  (list (* x 2) (1+ (* x 2))))
{% endhighlight %}
</section>

### Depth-first Search
Depth-first search is a method that dives deep into the tree and
checks only one of the successive nodes of the current state, and if
the successive node is not the goal, then it will check only one of
*that node's* successors. It will defer looking at the second
successors of each node until it has reached the end of the tree,
wherein it will go back and look at the second successor and repeat the process.

<img class="centerimage" src="/img/dps.png" style="width:250px"/>

This can be easily implemented with the `append` function, where we
take the successive nodes from the current state and append them to
the other un-checked states.

Take for example the starting state of 1, the successive nodes are 2 and 3, according to the binary-tree function we defined above. Depth-first will look at node 2, then ask our successor function to give it the next nodes from node 2, 4 and 5, and append those to the states it *hasn't looked at*, node 3. On the next pass, `tree-search` is called with the states 4, 5, and 3. It will look at node 4, then get *it's* successors, 8 and 9, and append those to 5 and 3, and so on.

*It will keep on looking at the first successor until
the tree ends*.

<section class="code">
{% highlight cl %}
(defun depth-first-search (state goal-p successor)
  (tree-search (list state) goal-p successor #'append))

;; goal check
(defun is (num)
  #'(lambda (x)
      (eql x num)))
{% endhighlight %}
</section>

Let's try running it:
<section class="shell">
{% highlight console %}
CL@USER$ (depth-first-search 1 (is 16) #'binary-tree)

(1) 
(2 3) 
(4 5 3) 
(8 9 5 3) 
(16 17 9 5 3) 
16
{% endhighlight %}
</section>

As you can see , depth-first only checks the left-hand side (the first
successor node it is handed) and completely disregards the rest until
it reaches the end. In the event the binary-tree has no end and the
state it is looking for is not on the left-hand side it will keep on
going forever.

<section class="shell">
{% highlight console %}
CL@USER$ (depth-first-search 1 (is 12) #'binary-tree)

(1) 
(2 3) 
(4 5 3) 
(8 9 5 3) 
(16 17 9 5 3) 
(32 33 17 9 5 3) 
(64 65 33 17 9 5 3) 
(128 129 65 33 17 9 5 3) 
........................
........................
{% endhighlight %}
</section>

### Breadth-first Search
Breadth-first Search can be called seen as the opposite of depth-first
search, in the sense that instead of going deep into the tree, it will
check both left and right hand sides of the tree before proceeding to
the next depth level.

So for node 1 that has as its successors node 2 and node 3, it will
check both nodes before checking their successors, nodes 4, 5, 6,
and 7.

<img src="/img/BFS.gif" class="centerimage" style="width:250px" />

<section class="code">
{% highlight cl %}
;; puts the successor nodes behind the previous un-checked nodes
(defun prepend (new old)
  (append old new))

(defun breadth-first-search (state goal-p successor)
  (tree-search (list state) goal-p successor #'prepend))
{% endhighlight %}
</section>

Let's try applying `breadth-first-search` to the previous problem of
finding 12 (depth-first search failed on an infinite binary tree).

<section class="shell"/>
{% highlight console %}
CL@USER$ (breadth-first-search 1 (is 12) #'binary-tree)

(1) 
(2 3) 
(3 4 5) 
(4 5 6 7) 
(5 6 7 8 9) 
(6 7 8 9 10 11) 
(7 8 9 10 11 12 13) 
(8 9 10 11 12 13 14 15) 
(9 10 11 12 13 14 15 16 17) 
(10 11 12 13 14 15 16 17 18 19) 
(11 12 13 14 15 16 17 18 19 20 21) 
(12 13 14 15 16 17 18 19 20 21 22 23) 
12
{% endhighlight %}
</section>

As you can see, infinite trees pose no risk to breadth-first search,
as long as the solution is in the tree. This is in contrast to
depth-first search, which just kept on going.

Of course this doesn't mean that breadth-first search is superior to
depth-first search, it just means that the more we know about the
problem space, the easier it is to determine the optimal algorithm, or
even if a particular algorithm should be used in the first place.

### Cost Functions
Until now, all we've done is check each node that is passed to us by
the combiner, without determining which nodes have a higher
possibility of being the node we are looking for. In the case of the
binary tree above, we know that node 4 is closer to node 5 than node
10, and yet we don't do any sort of filtering beyond the arrangement
of the nodes.

We can apply a rudimentary type of filtering in the form of cost
functions, where the cost function will determine which nodes have a
higher probability of being the node we are looking for.

In the above case, we can write a `diff` function that gives us the
distance between said node and the target node. We then use the `diff`
function as a *key parameter* to the function `sort`.

<section class="code">
{% highlight cl %}
(defun diff (num)
  #'(lambda (x)
      (abs (- x num))))

;; sorts in ascending order, according to the score given by the cost-fn
(defun sorter (cost-fn)
  #'(lambda (new old)
      (sort (append new old) #'< :key cost-fn)))

(defun best-first-search (state goal-p successor cost-fn)
  (tree-search (list state) goal-p successor (sorter cost-fn)))
{% endhighlight %}
</section>

Let's try applying it to searching for 12:

<section class="shell">
{% highlight console %}
CL@USER$ (best-first-search 1 (is 12) #'binary-tree (diff 12))

(1) 
(3 2) 
(7 6 2) 
(14 15 6 2) 
(15 6 2 28 29) 
(6 2 28 29 30 31) 
(12 13 2 28 29 30 31) 
12
{% endhighlight %}
</section>

As you can see, `sorter` sorts them according to their distance from
node 12, so it searches 7 before 6, 14 before 15, 15 before 6, 6
before 2, and finally arrives at 12.

### Further Guidance
The current solution isn't bad, we've gone from not finding the
solution, to finding the solution in a brute-force sort of way, to
refining the successor nodes that we're handed to try to guide the
search.

Can we do better?

What if we had a better way to sort the results, giving us the more
likely nodes earlier? In the last example we used the cost function
`diff` to give us the distance between the target and the current
node. The problem here is that we can actually overshoot our target:
the last run ranked 14 and 15 over 6 because they are technically
closer to node 12... even though they come after the target node.

This is where knowledge of the problem space can help optimize search
results. With no prior knowledge, the `diff` function would probably
be the optimal solution; however since we do in fact know that the
nodes are arranged in numerical order, obviously if the current node
is greater than the target node we know it's pointless to even look at
it.

In this case we can write a cost function that looks at the current
node and if it's greater than the target, assign it a really big score
so that the sort function will place it at the back of the queue.

<section class="code">
{% highlight cl %}
;; most-positive-fix-num returns the largest integer allowed in the system
(defun price-is-right (num)
  #'(lambda (x)
      (if (> x num)
          most-positive-fixnum
          x)))
{% endhighlight %}
</section>

<section class="shell">
{% highlight console %}
CL@USER$ (best-first-search 1 (is 12) #'binary-tree (price-is-right 12))

(1) 
(2 3) 
(3 4 5) 
(4 5 6 7) 
(5 6 7 8 9) 
(6 7 8 9 10 11) 
(7 8 9 10 11 12 13) 
(8 9 10 11 12 14 15 13) 
(9 10 11 12 16 17 14 15 13) 
(10 11 12 18 19 16 17 14 15 13) 
(11 12 20 21 18 19 16 17 14 15 13) 
(12 22 23 20 21 18 19 16 17 14 15 13) 
12
{% endhighlight %}
</section>

So the function is working, we can see from the output that only the
numbers below 12 are being considered, but it actually took the search
algorithm longer to find the target node... does this cost-function
actually have merit?

Introducing: [Beam Search](https://en.wikibooks.org/wiki/Artificial_Intelligence/Search/Heuristic_search/Beam_search).

### Beam Search
Beam Search is a modification of breadth-first search, the difference
being that it selects a number of nodes from the nodes returned by the
combiner and looks only at those... it will not consider every
successor that is returned. The amount of nodes that it will look at
is determined by the beam width.

<section class="code">
{% highlight cl %}
(defun beam-search (state goal-p successor cost-fn beam-width)
  (tree-search (list state) goal-p successor #'(lambda (new old)
                                                 (let ((sorted (funcall (sorter cost-fn) new old)))
                                                   (if (> beam-width (length sorted))
                                                       sorted
                                                       (subseq sorted 0 beam-width))))))
{% endhighlight %}
</section>

Beam search, limiting the successive nodes to the first 2 returned by `successor`.

<section class="shell">
{% highlight console %}
CL@USER$ (beam-search 1 (is 12) #'binary-tree (price-is-right 12) 2)

(1) 
(2 3) 
(3 4) 
(4 6) 
(6 8) 
(8 12) 
(12 16) 
12
{% endhighlight %}
</section>

The efficiency of beam search depends on the width of the beam, give it a smaller width and there is a chance that it will miss the target but it's quicker, give it a larger width and there is a bigger chance of finding the node but there is a computation penalty.

Beam search, limiting the successive nodes to the first 3 returned by `successor`.

<section class="shell">
{% highlight console %}
CL@USER$ (beam-search 1 (is 12) #'binary-tree (price-is-right 12) 3)
(1) 
(2 3) 
(3 4 5) 
(4 5 6) 
(5 6 8) 
(6 8 10) 
(8 10 12) 
(10 12 16) 
(12 20 21) 
12
{% endhighlight %}
</section>

As you can see, a width of 3 takes longer since the `search-tree` function has to look at more states. Interestingly, if you narrow the beam to a width of 1 you get depth-first search and if you open it up you get breadth-first search.

### Wrapping Up
We can see that there are several strategies that deal with searching
trees, and knowledge of the problem space is crucial to determining
which approach will work best.

Further Reading:

- [Tree Traversal](https://en.wikipedia.org/wiki/Tree_traversal)
- [Binary Search Tree](http://eternallyconfuzzled.com/tuts/datastructures/jsw_tut_bst1.aspx)
