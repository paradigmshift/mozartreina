---
layout: post
title: Functions
---
### Definition of a Function
Functions are nothing more than equations where you can plug in an input variable `\(x\)` and get an output variable `\(y\)` in return.
`\[
x \xrightarrow{\text{input}}\boxed{function}\xrightarrow{\text{output}} y
\]`
Functions describe the relations, causalities, and changes between one variable (*the input*) and the next (*the output*). In function notation it is expressed as:
`\[
\Large y = f(x)
\]`

### Examples
*These examples are taken from [The Manga Guide to Calculus](http://nostarch.com/mg_calculus.htm) (highly recommended)*

**An example of Causality**

The frequency of a cricket's chirp `\(y\)` is determined by temperature `\(x\)` with the function `\(y = 7x-30\)`. 
Given `\(x\)` is `\(27^\circ C\)`:

`\(\begin{align}
y &= 7x - 30 \\
  &= 7(27) - 30 \\
y &= 159 \mbox{ chirps per minute}
\end{align}\)` 

**An example of Changes**

The speed of sound `\(y\)` in meters per second (`\(m/s\)`) changes in relation to the temperature `\(x^\circ C\)`.

`\(\begin{align}
y &= v(x) \\
  &= 0.6x + 331 
\end{align}\)`

`\[\begin{array}{lll|lll}
x & = & 15^\circ C & x & = & -5^\circ C \\
y & = & v(15) & y & = & v(-5) \\
& = & 0.6(15) + 331 && = & 0.6(-5) + 331 \\
& = & 340 \mbox{m/s} && = & 328 \mbox{m/s} \\
\end{array}\]`

**An example of Relations**

Conversion between `\(x^\circ\)` Farenheit to `\(y^\circ\)` Celsius.

`\[\begin {array}{lll|lll}
y & = & f(x) & x & = & 50^\circ F\\
& = & \frac 5 9 (x - 32) && = &\frac 5 9 (50 - 32) \\
&&&& = & 10^\circ C 
\end{array}\]`

### As an aside...
***Composition of functions*** is the combination of two or more functions.

`\[
x \rightarrow \boxed{f} \rightarrow f(x) \rightarrow \boxed{g} \rightarrow g(f(x))
\]`

In computer science, functions that can be passed to other functions as input (*arguments*) are called **[first class functions](http://en.wikipedia.org/wiki/First-class_function)**.

### Graphing Functions
Graphing functions is quite simple, you plug in any variable `\(x\)` into the function and take the result `\(y\)` and use them as your `\(x, y\)` coordinates.

`\(f(x) = 2x - 1\)`
`\[\begin{array}{c|c|c}
x & 2x - 1 & (x,y) \\
\hline
1 & 1 & (1,1) \\
2 & 3 & (2,3) \\
3 & 5 & (3,5)
\end{array}\]`

<div id="box" class="jxgbox"></div>
<script type="text/javascript">
 var board = JXG.JSXGraph.initBoard('box', {boundingbox: [-6, 6, 6, -6], axis:true});
 var p1 = board.create('point',[1,1], {name: "1,1"});
 var p2 = board.create('point',[2,3], {name: "2,3"});
 var p3 = board.create('point',[3,5], {name: "3,5"});
 var li = board.create('line',[p1,p2], {strokeColor:'#0D1DA6',strokeWidth:1});
</script>

You might ask what the purpose of graphing a function is... Graphs are visual representations of data and the effect that they have on each other (sound familiar? that's what a function is supposed to show). These relations are easier to understand when graphed since humans are primarily visual creatures. Graphs are a perfect way to see and predict trends; it is such an effective way that even very young children can comprehend what graphs are saying.

### Further Reading

* [Math is fun](http://www.mathsisfun.com/sets/function.html)
* [What is a Lambda function](http://stackoverflow.com/questions/16501/what-is-a-lambda-function)
* [Lambda Calculus - *formalizing mathematics through functions*](http://en.wikipedia.org/wiki/Lambda_calculus)