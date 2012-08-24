---
layout: post
title: Derivatives of Functions
---
### Derivatives?
A derivative of a function, in plainspeak, is the rate of change between points of the function when graphed. For example, for the function `\(f(x) = x^2\)`, when `\(x = 2\)` then `\(y = 4\)`, when `\(x = 4\)` then `\(y = 16\)`. When graphed, you get:

<div id="box" class="jxgbox"></div>
<script type="text/javascript">
 var board = JXG.JSXGraph.initBoard('box', {boundingbox: [-2, 20, 10, -2], axis:true});
 var p1 = board.create('point',[2, 4], {name: "2,4"});
 var p2 = board.create('point',[4, 16], {name: "4,16"});
 board.create('functiongraph', [function(x){return x*x;},-3,5]);
</script>

The derivative in this case is the difference between the two points, also called the [slope](http://www.purplemath.com/modules/slope.htm), which is calculated with the following formula:

`\[
\large{m = \frac{y_2 - y_1} {x_2-x_1}} \\
\]`

In this example that would be `\(\frac {16 - 4} {4 - 2} = \frac {12} 2 = 6 \)`. The rate of change between the two points is 6... with one caveat. Since the derivative was derived from two points, it is only accurate when used with these two points. The farther you get from these two points, the less accurate the slope becomes.

If for example the second point is `\(\left(3,9\right)\)` instead of `\(\left(4,16\right)\)`, then `\(\frac {9 - 4} {3 - 2} = \frac 5 1 = 5\)`. The rate of change is then **5** instead of **6**, as in the previous case.

Before we get go any further...

### What for?
So what is the point of calculating the rate of change between two points? What are real-world examples where this is relevant?

One application is in the calculation of moving bodies. Say an object (maybe a car?) is traveling around a corner and suddenly starts to skid, with the slope of the point where the car starts to skid, you can predict where the car will end up (it will travel in a [tangent](http://en.wikipedia.org/wiki/Tangent) to the curve).

*car skidding at point `\((0,0)\)`, predicted line of travel in red*

<div id="box2" class="jxgbox"></div>
<script type="text/javascript">
 var board = JXG.JSXGraph.initBoard('box2', {boundingbox: [-5, 10, 5, -5], axis:true});
 var p1 = board.create('point', [0, 0]);
 board.create('functiongraph', [function(x){return x*x;},-3,5], {firstArrow:true});
 board.create('functiongraph', [function(x){return 0;}, 0,4],{strokeColor:'red', strokeWidth:3, dash:1, lastArrow:true});
</script>

Other uses of *Differentiation* (another term for calculating the derivative) include finding the minimum and maximum surface areas for given volumes (minimum amount of metal to create a cylinder of a certain volume, etc.) and determining the acceleration and velocity of a certain object from a given position ([Velocity and Acceleration from the position equation](http://www.jtaylor1142001.net/calcjat/Solutions/Diffapp1/DA1_1/avdiff.html)).

### Notation and Formula
There are several notations in use for representing derivatives.

[Leibniz](http://en.wikipedia.org/wiki/Gottfried_Wilhelm_Leibniz), who independently of Isaac Newton came up with infinitesimal calculus, uses the notation `\(\large{\frac {dy} {dx}} \)` for derivatives.

[Lagrange's](http://en.wikipedia.org/wiki/Joseph_Louis_Lagrange) notation uses the `\(\prime\)` symbol in conjunction with function notation `\(f'(x)\)`.

[Euler](http://en.wikipedia.org/wiki/Leonhard_Euler) used a differential operator, denoted as `\(D\)`, which is prefixed to the function. This is seen as `\(D_xy\)`.

[Newton](http://en.wikipedia.org/wiki/Isaac_Newton) used *dot notation*, which involves placing a dot over the dependent variable as seen in `\(\dot y = \frac {dy} {dt}\)`.

The formula for deriving the derivative of a function has to calculate the rate of change between the points in the graphed function. The standard formula is:
`\[
f'(x) = \frac {f(x + e) - f(x)} e
\]`
This is actually exactly the same as the slope formula presented above. `\(f(x + e)\)` is nothing more than `\(y_2\)`, where `\(e\)` is the difference between `\(y_1\)` and `\(y_2\)` therefore `\(y_1 + e = y_2\)` (when the variable `\(x\)` is plugged into the function, `\(y\)` comes out the other end). The bottom part of the equation has been simplified, in it's full form it would read `\(\frac {f(x + e) - f(x)} {(x + e) - x}\)`. Canceling the positive and negative `\(x\)` leaves just `\(e\)`.

#### Example
Revisiting the function `\(f(x) = x^2\)` above, we've already shown how to determine the derivative, or slope, when two points are given. Again, the slope will quickly become more and more inaccurate as we go farther and farther from the given `\(x, y\)` coordinates. If we want to find the rate of change at any given point, we will have to use the afore-mentioned formula.

`\(f(x) = x^2 \\
f'(x) = \frac {f(x + e) - f(x)} e \\
f'(x) = \frac {(x + e)^2 - x^2} e \\
f'(x) = \frac {x^2 + 2xe + e^2 - x^2} e \\
f'(x) = 2x + e \\
\)`

Uh oh... now what? How do we handle `\(e\)`? `\(e\)` as you may recall is the difference between one point and the next, but since we're trying to determine the derivative of the *function* and not two points, how do we solve for `\(e\)`? This is where we introduce the concept of **limits**.

Limits are an integral part of Calculus, but there are several types of limits. We will be applying the *limit of a function*, a short explanation and example of this can be found in the [wikipedia entry on limits](http://en.wikipedia.org/wiki/Limit_(mathematics)). To give a really brief example, say that you have a function `\(f(x)\)`, a number, `\(C\)` and the expression `\(f(x) = L\)`. As the variable `\(x\)` gets closer to `\(C\)`, the output of `\(f(x)\)` also gets closer to `\(f(x) = L\)`. When `\(x = C\)`, then `\(f(x) = L\)` becomes true, as `\(x\)` moves farther and farther away from `\(C\)`, the evaluation of `\(f(x)\)` also moves farther and farther away from `\(f(x) = L\)`.

A short example is in order to demonstrate this idea. Say `\(L = 5\)` and `\(C = 10\)` and `\(f(x) = \frac x 2\)`.

`\(\mbox{If } x = 5, f(x) = \frac 5 2 = 2.5 \\
\mbox{If } x = 6, f(x) = \frac 6 2 = 3 \\
\mbox{If } x = 7, f(x) = \frac 7 2 = 3.5 \\
\mbox{If } x = 8, f(x) = \frac 8 2 = 4 \\
\text{and so on...} \\
\mbox{when } x = C, f(x) = \frac {10} {2} = 5 \\
\)`

So we say that as `\(x\)` approaches the value `\(L\)`, the statement `\(f(x) = L\)` becomes true. The notation for a limit is `\(\lim\limits_{x\rightarrow c}\)`, where `\(x\)` is approaching `\(c\)`. The complete expression of the above example would be:

`\[
\lim_{x\rightarrow C}f(x) = L
\]`

So now that we have defined limits, how do they apply to differentiation?

Remember that `\(e\)` is the *distance* or *difference* between two points, but since we aren't solving for the difference between specific points but for the rate of change at *any* point, the solution here is to introduce a limit wherein we state that as `\(e\)` approaches `\(0\)` (when `\(e = 0\)` there is only one point), the equation becomes accurate.

`\[
f'(x) = \lim_{e\rightarrow 0} 2x + e \\
\]`

So going back to our equation:

`\(f'(x) = 2x + 0 \\
f'(x) = 2x
\)`

### One more?
Let's do one more example to make sure that we understand how derivatives work. 

`\[
f(x) = 20\sqrt x, x = 4
\]`

We are looking for the tangent line at the given `\(x\)` coordinate. This means that we will calculate the derivative (remember that the derivative is just another name for the slope), then plug it into the equation for tangent lines:

`\[
y = k(x - a) + f(a)
\]`

*`\(k\)` is the slope, and `\(a\)` is the given `\(x\)` coordinate.*

So let's get on with it!

`\(\begin{array}{ll}
f'(x) &= \frac {f(x + e) - f(x)} e \\
& = \frac {f(4 + e) - f(4)} e \\
& = \frac {20\sqrt{4 + e} - 20 * 2} e \\
& = 20 \frac {\sqrt{4 + e} - 2} e * \frac {\sqrt{4 + e} + 2}{\sqrt{4 + e} + 2} \\
& = 20 \frac {4 + e - 4} {e{\sqrt{4 + e} + 2}} \\
& = \frac {20e} {e{\sqrt{4 + e} + 2}} \\
& = \frac {20} {\sqrt{4 + e} + 2} \\
& = \frac {20} {\sqrt{4 + 0} + 2} \\
& = \frac {20} 4 \\
& = 5
\end{array}\)`

So the derivative is `\(5\)`, now to plug it into the tangent line equation (also called the *[point slope formula](http://en.wikipedia.org/wiki/Linear_equation#Point.E2.80.93slope_form)*):

`\(y = 5(x - 4) + 40 \\
y = 5x + 20 \\
\)`

This is the equation of the line tangent to `\(f(x) = 20\sqrt x\)` at point `\((4,40)\)`. Let's graph the equations so that we can visualize the results better.

<div id="box3" class="jxgbox"></div>
<script type="text/javascript">
 var board = JXG.JSXGraph.initBoard('box3', {boundingbox: [-1, 80, 20, -1], axis:true});
 var p1 = board.create('point', [4, 40], {name:"4,40"});
 board.create('functiongraph', [function(x){return 20*(Math.sqrt(x));},0,10],{lastArrow:true});
 board.create('functiongraph', [function(x){return 5*x+20;}, 0,10],{strokeColor:'red', strokeWidth:3, dash:1, lastArrow:true,});
</script>

*red is the tangent line equation*

### What's next?
For those interested in going deeper into derivatives and the mathematical techniques associated with it, I suggest the following resources:

* [Khan Academy on the Chain Rule](http://www.khanacademy.org/math/calculus/v/the-chain-rule)
* [Wikipedia entry on Applications of Derivatives](http://en.wikipedia.org/wiki/Differential_calculus#Applications_of_derivatives)
* [Manga Guide to Calculus](http://nostarch.com/mg_calculus.htm)