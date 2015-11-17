---
layout: post
title: Extrema and Inflection Points
---
### As Usual, The Definitions Come First
We'll be using a few technical math terms, so let's define them first:

* #### Extrema
Extrema are both the maximum and minimum values that a function takes at a point either within a certain domain (a subset of possible values) or the global domain (the entire set of possible values).

<div id="box1" class="jxgbox center-block" style="width:250px; height:250px"></div>
<script type="text/javascript">
 var board = JXG.JSXGraph.initBoard('box1', {boundingbox: [-0.2, 7, 1.2, -6], axis:true});
 var p1 = board.create('point', [0.1,6], {name:"Maxima"});
 var p2 = board.create('point', [0.3,-3.15], {name:"Minima"});
 board.create('functiongraph', [function(x){return Math.cos(3*3.14*x)/x;},0.1,1.1]);
</script>

* #### Concave Upwards
A curve whose slope increases as `\(x\)` increases. Resembles an upright **U**.
<div id="box3" class="jxgbox center-block" style="width:250px; height:250px"></div>
<script type="text/javascript">
 var board = JXG.JSXGraph.initBoard('box3', {boundingbox: [-5, 5, 5, -5], axis:true});
 board.create('functiongraph', [function(x){return x*x;},-3,3]);
</script>

* #### Concave Downwards
A curve whose slope decreases as `\(x\)` decreases. Resembles an upside-down **U**.
<div id="box4" class="jxgbox center-block" style="width:250px; height:250px"></div>
<script type="text/javascript">
 var board = JXG.JSXGraph.initBoard('box4', {boundingbox: [-5, 5, 5, -5], axis:true});
 board.create('functiongraph', [function(x){return -x*x;},-3,3]);
</script>


* #### Inflection Points
The points in a curve where the slope changes from positive (concave upwards) to negative (concave downwards) and vice-versa. They are also called **Undulation Points**.

<div id="box2" class="jxgbox center-block" style="width:250px; height:250px"></div>
<script type="text/javascript">
 var board = JXG.JSXGraph.initBoard('box2', {boundingbox: [-5, 10, 5, -5], axis:true});
 var p1 = board.create('point', [0, 0], {name:"Inflection Point"});
 board.create('functiongraph', [function(x){return x*x*x;},-3,5]);
</script>

So basically **Inflection Points** are the places where the slope becomes either **Concave Upwards** from being **Concave Downwards** or the opposite; they are places where the rate of change of the ***slope itself*** changes from positive to negative or vice versa. **Maxima** and **Minima**, on the other hand, are the highest and lowest points where the rate of change of the curve, or *slope*, is 0.

### How Do We Determine the Inflection Point?
So how do we calculate the Inflection Point of a curve? Eye-balling isn't really a scientific solution and isn't accurate.

Here we will introduce the concept of the [`\(2^\text{nd}\)` Derivative](http://calculusapplets.com/secondderiv.html). 

The `\(2^\text{nd}\)` Derivative is the derivative of the derivative, or in plainspeak, the rate of change of the slope. The slope, if you will recall, is the rate of change of the curve, which can be both constant (as in a straight line) or not (concave or curving lines). In the case of a curving line, the `\(2^\text{nd}\)` Derivative is used to determine how much the curve of the line is changing.

When using Lagrange's notation it is denoted as:
<div>
`\[ \large{f''(x)} \]`
</div>

When we have a point in the curve that has a slope of 0, `\(f'(x) = 0\)`, then this is a possible *Inflection Point*. We then check by calculating the `\(2^\text{nd}\)` Derivative at the same point.

But first, how to determine the `\(2^\text{nd}\)` Derivative? This is simply done, you just take `\(f'(x)\)` and take the derivative of that. 

For example, `\(f'(x)\)` of `\(f(x) = x^3\)` is `\(3x^2\)`. The derivative of `\(f'(x) = 3x^2\)` is `\(f''(x) = 6x\)`, you simply apply the derivative formula same formula to the `\(1^\text{st}\)` Derivative that you did to the original function.

Once you have the `\(2^\text{nd}\)` Derivative, you then try to determine the [Critical Points](http://www.cliffsnotes.com/study_guide/Critical-Points.topicArticleId-39909,articleId-39888.html), or points where the slope of the curve is `\(0\)`. 

Continuing with our example:
 
<div>
`\(f(x) = x^3 \\
f'(x) = 3x^2 \\
f''(x) = 6x \)`
</div>

`\(3x^2 = 0\)` when `\(x = 0\)`, so `\((0,0)\)` is a Critical Point and a possible candidate for the *Inflection Point*. To determine whether it is an Inflection Point or not, we then have to see if the same `\(x\)` value that was used to find the Critical Point (`\(x = 0\)`) also produces 0 when plugged into the `\(2^\text{nd}\)` Derivative formula.

<div>
`\(f''(x) = 0 \Rightarrow 6x = 0 \\
x = 0 \)`
</div>

In this particular case, if there is an Inflection Point, it **has** to be at `\((0,0)\)` (this is because `\(x = 0\)` is the only value that results in 0 when plugged into the first and second derivative). Yet this isn't enough, the derivative of the slope must change on either side of the point for it to qualify as an Inflection Point. We have to take two points on either side of the point and determine their values.

<div>
`\(x = 1 \\
f''(x) = 6(1) \rightarrow 6 \\
x = -1 \\
f''(x) = 6(-1) \rightarrow -6
\)`
</div>

We can see that when we plug in 1, which is at the right side of 0, we get a positive result (6) and with -1, which is at the left side of 0, we get a negative result (-6). Hence, the point `\((0,0)\)` satisfies all the requirements for it to be considered an *Inflection Point*.

<div id="box5" class="jxgbox center-block" style="width:250px; height:250px"></div>
<script type="text/javascript">
 var board = JXG.JSXGraph.initBoard('box5', {boundingbox: [-5, 5, 5, -5], axis:true});
 var p1 = board.create('point', [0,0], {name:"Inflection Point"});
 board.create('functiongraph', [function(x){return x*x*x;},-3,3]);
</script>


What if there are no values that output 0 with both `\(f'(x)\)` and `\(f''(x)\)`? Does that mean that there is no Inflection Point? 

In fact there are cases where the Inflection Point is not found on a [stationary point](http://en.wikipedia.org/wiki/Stationary_point), we'll go into them later.

**Inflection Points**

* Must result in 0 when plugged into `\(f''(x)\)`.
* Signs on either side of the point must be opposite to each other.

### Finding the Minima and Maxima Points
We've discussed finding the Inflection Point(s) on a curve, now how about the *Minima* and *Maxima* points?

Finding these points is easy, according to [Fermat's Theorem](http://en.wikipedia.org/wiki/Fermat's_theorem_(stationary_points) they can only be located at Critical Points.

`\(\ast\)` *crickets chirping* `\(\ast\)` 

Say what? They are located at Critical Points? But we just showed that ***Inflection Points*** are located at Critical Points! So how can the Minima and Maxima points be found there as well?

Actually, like we mentioned before, Inflection Points *may* be located on *Critical Points* if the `\(x\)` value that was plugged into the `\(1^\text{st}\)` Derivative formula to obtain `\(0\)`, when plugged into the `\(2^\text{nd}\)` Derivative formula, ***also produces 0 along with a change in signs on either side of the point***.

As usual, this is best demonstrated with an example.

Let's take the formula `\(f(x) = x^4-8x^2\)`.

<div>
`\(f'(x) = 4x^3 - 16x\\
f''(x) = 12x^2 - 16 \)`
</div>

Now to find the Critical Points:

<div>
`\(f'(x) = 0 \Rightarrow 4x^3 - 16x = 0 \\
4x(x^2-4) = 0 \\
4x(x+2)(x-2) = 0 \\
x = 0, 2, -2 \\
x = (0,0), (-2, -16), (2, -16) \)`
</div>

<div id="box6" class="jxgbox center-block" style="width:250px; height:250px"></div>
<script type="text/javascript">
 var board = JXG.JSXGraph.initBoard('box6', {boundingbox: [-5, 5, 5, -20], axis:true});
 var p1 = board.create('point', [0,0], {name:"Critical Point"});
 var p2 = board.create('point', [-2,-16], {name:"Critical Point"});
 var p3 = board.create('point', [2,-16], {name:"Critical Point"});
 board.create();
</script>


So these are the Critical Points, but are they all ***Inflection Points***? Now we have to apply the `\(x\)` values to the `\(2^\text{nd}\)` Derivative:

<div>
`\(f''(x) = 12x^2 - 16 \\
12(-2)^2 - 16 = 32 \\
f''(x) = 12x^2 - 16 \\
12(2)^2 - 16 = 32 \)`
</div>

Well, plugging in -2  and 2 both produced positive results, so the points `\((-2, -16), (2, -16)\)` ***are not*** an Inflection Points! What about `\((0,0)\)`?

<div>
`\(f''(x) = 12x^2 - 16 \\
12(0)^2 - 16 = -16 \)`
</div>

Here the result is a negative number, still not 0, and therefore not an Inflection Point!

So where does that leave the points `\((-2, -16), (2, -16)\)` and `\((0,0)\)`? These points are the *local* (or global depending on the domain of the function) **Minima** and **Maxima** Points, the points on the graph with the (relatively) highest and lowest values.

### Non-Stationary Inflection Points
As we've seen in the previous example, there were no Critical Points that were Inflection Points. In this case, to determine the Inflection Point(s), we just set `\(f''(x)\)` to 0.

<div>
`\(f''(x) = 12x^2 - 16x \\
12x^2 - 16x = 0 \\
x^2 = \frac {16} {12} \\
x = 1.1547 \)`
</div>
 
Now we test on either side of point 1.1547 to see if the signs change:

<div>
`\(x = 1.1547 + 0.1 \\
12(1.2547)^2 - 16(1.2547) = 2.891 \\
x = 1.1547 - 0.1 \\
12(1.0547)^2 - 16(1.0547) = -3.526 \)`
</div>

So when we shift to the right the sign is positive so the curve is concave upwards, and when we shift to the left the sign is negative and so concave downwards.

Conclusion? `\(x = 1.1547\)` is an Inflection Point!

<div id="box7" class="jxgbox center-block" style="width:250px; height:250px"></div>
<script type="text/javascript">
 var board = JXG.JSXGraph.initBoard('box7', {boundingbox: [-5, 5, 5, -20], axis:true});
 var p1 = board.create('point', [0,0], {name:"Maxima"});
 var p2 = board.create('point', [-2,-16], {name:"Minima"});
 var p3 = board.create('point', [2,-16], {name:"Minima"});
 var p4 = board.create('point', [1.547,-13.412], {name:"Inflection Point"});
 var p5 = board.create('point', [-1.547,-13.412], {name:"Inflection Point"});
 board.create('functiongraph', [function(x){return x*x*x*x-8*(x*x);},-3,3]);
</script>


`\(\ast\)` *`\(x = -1.1547\)` is also an Inflection Point, since `\(f''(-1.547) = 0\)`*.
