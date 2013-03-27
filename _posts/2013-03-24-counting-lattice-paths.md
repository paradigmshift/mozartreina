---
layout: post
title: Counting Lattice Paths
---

### First of all, what is a Lattice Point
Lattice points are points in a [Point Lattice](http://mathworld.wolfram.com/PointLattice.html) where two or more gridlines intersect.

<img class="centerimage" src="/img/LatticePoints_1000.gif"/>
*there is only one lattice point here, in the center, as that is the only point where the grid lines intersect*

### So what are Lattice Paths
Lattice Paths are paths that lead from one lattice point to the next. 

<img class="centerimage" src="/img/binomialpath1.png"/>
<section class="centertext">*two paths, from the leftmost corner to the rightmost, through a 1x1 lattice*</section>

<img class="centerimage" src="/img/binomialpath2.png"/>
<section class="centertext">*six paths through a 2x2 lattice*</section>

### Counting the number of possible paths from a starting point to an endpoint
So how would one, given the need or want, go about counting the number of possible paths from one point to another? After a few failed attempts at creating my own method (all of which failed horribly), I ended up using the method described [here by Robert Dickau](httt://www.robertdickau.com/lattices.html).

Here, the solution is derived by counting the number of paths it takes to get to adjacent points and adding them together.

<img class="centerimage" src="/img/binomialpaths4.png"/>

All points from the rightmost side to the top and bottom take 1 step to reach, so they are marked with one. The next point, diagonal to the rightmost point, takes 2 steps, because each point adjacent to it (the premise is that from the start point, you can only go towards the right, NE or SE) takes one step to get to. The point diagonal to that one, going towards the endpoint, takes 6 steps because the points adjacent to it both take 3 steps, and so on.

Now this is a great solution but to use it effectively when dealing with large lattices we will have to come up with a program that can do the counting for us. Even a small 4x4 grid contains 70 possible paths, it would be ridiculous for us not to use a computer to do this repetitive task (not to mention that computers are far more accurate and are not prone to *"human error"*).

<img class="centerimage" src="/img/binomialpath4.png" />

### Translating the method into code
So the method's premise is counting the all paths that have 1 step and working from there to come up with all points that require 2 steps, and from there to 3 steps and so on. How would this translate to code?

While it would be possible to come up with an algorithm that would walk all paths with 1 step and from there work out the paths with 2 steps and so forth, there is actually an easier method. I actually came up with this myself after staring at the number of paths in the above 4x4 grid, though I'm sure it must be listed somewhere as a method for counting lattice paths.

If we take a look at the first  northeastern rows in the 4x4 grid, we'll see that the number of paths are as follows:

`\[ \begin{bmatrix} 1 & 1 & 1 & 1 & 1 \\ 1 & 2 & 3 & 4 & 5 \end{bmatrix}\]`

If we take the third and fourth rows, we end up with the following matrix:

`\[ \begin{bmatrix} 1 & 1 & 1 & 1 & 1 \\ 1 & 2 & 3 & 4 & 5 \\ 1 & 3 & 6 & 10 & 15 \\ 1 & 4 & 10 & 20 & 35 \\ 1 & 5 & 15 & 35 & 70 \end{bmatrix}\]`

You'll note an interesting symmetry where the second row matches the second column, the third row the third column, etc. Yet that didn't really help any in coming up with a theoretical formula for calculating the next row from the preceeding one.

Then I realized that one didn't have to come up with both adjacent points to determine the next one, since the adjacent points mirrored each other (the adjacent points to the second point on the second row are both 1 for example and the point itself is 2). All you have to do is come up with one point and multiply it by 2, that would yield the next point. The pattern that i reasoned out was that multiplying the second element of the top row by 2 would yield the path count below that element. Then adding that number to the next element in the top row would yield the count below that element and so forth.

Starting from the first row:

`\[ \begin{bmatrix} 1 & 1 & 1 & 1 & 1 \end{bmatrix}\]`

Take the second element and multiply it by 2, placing it below the second element:

`\[ \begin{bmatrix} 1 & 1 & 1 & 1 & 1 \\ & 2\end{bmatrix}\]`

Add that to the next element in the above row (2 + 1):

`\[ \begin{bmatrix} 1 & 1 & 1 & 1 & 1 \\ & 2 & 3\end{bmatrix}\]`

And keep going on going:

`\[ \begin{bmatrix} 1 & 1 & 1 & 1 & 1 \\  & 2 & 3 & 4 & 5 \end{bmatrix}\]`

Then work on the third row by taking the second element of the second row (3) and repeating the process, 3 * 2 = 6 hence:

`\[ \begin{bmatrix} 1 & 1 & 1 & 1 & 1 \\  & 2 & 3 & 4 & 5 \\ & & 6\end{bmatrix}\]`

Take 6 and add it to the next element in the second row (4):

`\[ \begin{bmatrix} 1 & 1 & 1 & 1 & 1 \\  & 2 & 3 & 4 & 5 \\ & & 6 & 10\end{bmatrix}\]`

And then take that and add it to the last element in the second row:

`\[ \begin{bmatrix} 1 & 1 & 1 & 1 & 1 \\  & 2 & 3 & 4 & 5 \\ & & 6 & 10 & 15\end{bmatrix}\]`

This algorithm allows you determine the number of paths from on edge to the other without actually drawing a diagram and manually counting the paths.

Lisp implementation

<section class=code>
{% highlight cl %}
  (defvar *path-count* '())

  (defun initial-steps (width)
    (let ((steps '()))
      (dotimes (i width)
        (push 1 steps))
      steps))

  (defun count-next-steps (previous)
    (if (second previous)
      (let ((step (* (second previous) 2))
            (previous-copy (cddr previous))
            (steps '()))
        (push step steps)
        (loop until (null previous-copy)
           do (progn (incf step (car previous-copy))
                     (push step steps)
                     (setf previous-copy (cdr previous-copy))))
        (reverse steps))
      previous))

  (defun count-paths (width)
    (setf *path-count* '())
    (push (initial-steps width) *path-count*)
    (dotimes (i (1- width)) 
      (push (count-next-steps (car *path-count*)) *path-count*)))

{% endhighlight %}
</section>

Let's try running it with a 3x3 grid:

<section class=shell>
{% highlight console %}
CL@USER$ (count-paths 4)

NIL
CL@USER$ *PATH-COUNT*
((20) (6 10) (2 3 4) (1 1 1 1))
{% endhighlight %}
</section>

<section class=centertext>*note that for a 3x3 grid, we entered 4 as the width, this si because 3x3 grids have 4 lattice points at the top </section>

The program returns the rows (in reverse order), with the last row being the total number of paths. A 3x3 grid indeed has 20 possible paths, we can see that when we inspect the 4x4 grid above, if we restrict it to a 3x3 grid the last lattice point will have 20 steps.

How about 4x4? We know, again from the above grid, that 4x4 grids have 70 possible paths.

<section class=shell>
{% highlight console %}
CL@USER$ (count-paths 5)
NIL
CL@USER$ *PATH-COUNT*
((70) (20 35) (6 10 15) (2 3 4 5) (1 1 1 1 1))
{% endhighlight %}
</section>

### Wrapping it up

Further testing is possible but I think we'll find that the above solution is accurate, at least when we take into account certain rules of movement from one lattice point to the next (no backward movements, always going either NE or SE).

There is another reason that I am quite confident of this method, I used it to solve a math problem in a very popular website and the answer was validated.

Still, if someone can find some flaw in the code or logic, I would be very happy if you would send a message detailing where and why. Also, anything on the algorithm I "discovered" would be appreciated as well, it seems very unlikely that someone hasn't thought about this way of counting lattice paths.