---
layout: post
title: Functional Pipelines
---

### Functional Pipelines? What's that?
I came across this term when working through [Barski's](http://www.oreillynet.com/pub/au/3688) [Land of Lisp](http://landoflisp.com/) (be sure to check out his site [Lisperati](http://www.lisperati.com) as well!) on page 309. I've googled around a bit and there seems to be no formal definition on the web, [Pipeline Programming][] comes up frequently as a result of my queries, and although it seems similar i don't quite think that it's an exact explanation for what we're about to do. So for all practical purposes we'll stick with Barski's definition: *"A sucession of functions that operate, one after another, on a big chunk of data."*

But before we get into that, we should think about what the important concepts in functional programming are and what it really means to *"think functionally"*.

[Pipeline Programming]: http://en.wikipedia.org/wiki/Pipeline_(software)

### Data
According to Paul Callaghan in his article [Thinking Functionally with Haskell](http://pragprog.com/magazines/2012-08/thinking-functionally-with-haskell), the most important concept in functional style programming is this: 

***It's all about data***

You should read the post to for a deeper look at his ideas and reasons behind them, but it boils down to this: The code should focus on what kind of data is being entered and the various transformations of the data that have to be done in order to achieve our goals.

This thinking is perfect for discussing and understanding the technique that Barski talks about and demonstrates in his chapter on writing a game in the functional style. We won't go into too much detail here, instead I will try to show the broad strokes and the logic that is being applied. For those who want a more specific example, go buy the book and read Chapter 15.

### Pipelines and Data
The basic premise here is that, instead of modifying the input we are given, we take the data and pipe it into functions, which create *new* sets of data based on their particular algorithms, without touching the original arguments. The term *pipeline* is used because the functions each take the output of other functions as their input, thus chaining them together. This is called the [composition of functions](http://www.mathsisfun.com/sets/functions-composition.html) in math (this differs from the concept of passing around [first-class functions](http://en.wikipedia.org/wiki/First-class_function) wherein functions take ***other*** functions, not their results, as their arguments).

Let's do a quick example, just to explain the basic concept. Suppose we want a list (or array, or tuple, or... you get the idea) of all the natural numbers, under a thousand, that are multiples of either 3 or 5. In Lisp I can define a function that gives me the results thus:

<section class="code">
{% highlight cl %}
    (defun under-1000 ()
      (loop for i from 0 to 999
            when (or (eq (mod i 3) 0) (eq (mod i 5) 0))
            collect i))
{% endhighlight %}
</section>

When run:

<section class="shell">
    CL-USER> (under-1000)
    (0 3 5 6 9 10 12 15 18 20 21 24 25 27 30 33 35 36 39 40 42 45 48 50 51 54 55 57
     60 63 65 66 69 70 72 75 78 80 81 84 85 87 90 93 95 96 99 100 102 105 108 110
     111 114 115 117 120 123 125 126 129 130 132 135 138 140 141 144 145 147 150
     153 155 156 159 160 162 165 168 170 171 174 175 177 180 183 185 186 189 190
     192 195 198 200 201 204 205 207 210 213 215 216 219 220 222 225 228 230 231
     234 235 237 240 243 245 246 249 250 252 255 258 260 261 264 265 267 270 273
     275 276 279 280 282 285 288 290 291 294 295 297 300 303 305 306 309 310 312
     315 318 320 321 324 325 327 330 333 335 336 339 340 342 345 348 350 351 354
     355 357 360 363 365 366 369 370 372 375 378 380 381 384 385 387 390 393 395
     396 399 400 402 405 408 410 411 414 415 417 420 423 425 426 429 430 432 435
     438 440 441 444 445 447 450 453 455 456 459 460 462 465 468 470 471 474 475
     477 480 483 485 486 489 490 492 495 498 500 501 504 505 507 510 513 515 516
     519 520 522 525 528 530 531 534 535 537 540 543 545 546 549 550 552 555 558
     560 561 564 565 567 570 573 575 576 579 580 582 585 588 590 591 594 595 597
     600 603 605 606 609 610 612 615 618 620 621 624 625 627 630 633 635 636 639
     640 642 645 648 650 651 654 655 657 660 663 665 666 669 670 672 675 678 680
     681 684 685 687 690 693 695 696 699 700 702 705 708 710 711 714 715 717 720
     723 725 726 729 730 732 735 738 740 741 744 745 747 750 753 755 756 759 760
     762 765 768 770 771 774 775 777 780 783 785 786 789 790 792 795 798 800 801
     804 805 807 810 813 815 816 819 820 822 825 828 830 831 834 835 837 840 843
     845 846 849 850 852 855 858 860 861 864 865 867 870 873 875 876 879 880 882
     885 888 890 891 894 895 897 900 903 905 906 909 910 912 915 918 920 921 924
     925 927 930 933 935 936 939 940 942 945 948 950 951 954 955 957 960 963 965
     966 969 970 972 975 978 980 981 984 985 987 990 993 995 996 999)
</section>

Now we get to the first pipeline. What if we want a list of all natural numbers under a thousand, multiples of 3 or 5, and even? We could rewrite the original function to include this capability but that would be sub-optimal in a few ways. First of all you never know when you want a complete list, not just the even numbers. Second of all you would miss out on creating another function that can be used elsewhere, not just in this scenario.

So we'll write a function that takes a list as its argument and returns, without touching the original input, a new list that consists of only the even members of the first list.

<section class="code">
{% highlight cl %}
    (defun my-even (lst)
      (loop for i in lst
            when (eq (mod i 2) 0)
            collect i))
{% endhighlight %}
</section>

Now once we apply it to the results of the `under-1000` function we get...

<section class="shell">
    CL-USER> (my-even (under-1000))
    (0 6 10 12 18 20 24 30 36 40 42 48 50 54 60 66 70 72 78 80 84 90 96 100 102 108
     110 114 120 126 130 132 138 140 144 150 156 160 162 168 170 174 180 186 190
     192 198 200 204 210 216 220 222 228 230 234 240 246 250 252 258 260 264 270
     276 280 282 288 290 294 300 306 310 312 318 320 324 330 336 340 342 348 350
     354 360 366 370 372 378 380 384 390 396 400 402 408 410 414 420 426 430 432
     438 440 444 450 456 460 462 468 470 474 480 486 490 492 498 500 504 510 516
     520 522 528 530 534 540 546 550 552 558 560 564 570 576 580 582 588 590 594
     600 606 610 612 618 620 624 630 636 640 642 648 650 654 660 666 670 672 678
     680 684 690 696 700 702 708 710 714 720 726 730 732 738 740 744 750 756 760
     762 768 770 774 780 786 790 792 798 800 804 810 816 820 822 828 830 834 840
     846 850 852 858 860 864 870 876 880 882 888 890 894 900 906 910 912 918 920
     924 930 936 940 942 948 950 954 960 966 970 972 978 980 984 990 996)
</section>

So you can see how the argument of `my-even` is given as the result of `under-1000`. Of course `my-even` will take any list as its argument, and this is also why it is a much better idea to decouple the two functions in this manner. If you need to apply an "even number filter" on other lists, other results of functions, then you can just call `my-even` and pass them those results. If we had coded the "even number filter" into the function `under-1000` itself, we wouldn't be able to use it in any other place.

Now what if we want to add the results of `my-even` and `under-1000` together? We would just write another function that would take a list and add their elements, returning the total.

For this example I'll use an in-built feature of Lisp called `reduce`.

<section class="shell">
    CL-USER> (reduce #'+ (my-even (under-1000)))
    115836
</section>

You can see a three-tier chaining of functions here. The results of `under-1000` are *piped* into the function `my-even`, which filters them and removes the odd numbers, these are then further *piped* into `reduce`, which takes all the elements and adds them up.

`\[
\boxed{\text{under-1000}} \rightarrow \boxed{\text{my-even}} \rightarrow \boxed{\text{reduce}}
\]`

Nowhere in the code is there any sort of variable assignment or use of **mutable data**, the results of each functions are entirely new data sets; the argument that was passed into each of them remains untouched.

### The Game Example
So say that we have a 2 player game, and we have to find a way to code it in such a way that it can be expanded into a 3, 4, or even 5 player game. We might or might not want to change the rules, and we will probably want to add an AI opponent later on. 

To make the addition of an AI player easier, and to showcase the pipeline technique, we will create code that will output a `game-tree`, a list of all possible moves from a starting position.

We will need 3 basic chunks of code:

* The `game-tree` function
* A chunk that handles the human input
* The AI code 

We will feed the initial data from the `game-tree` function (a list of all possible moves) to the `handle-human` part(and AI if we're playing as one player) of our code, the `handle-human` function will then further refine the list of possible moves (based on the human player's choices) until there are no possible moves and there is a winner or a draw is declared.

### `game-tree`
So let's take a look at what the `game-tree` function is composed of. Basically it will take as an input a starting board (this is randomly generated by another function, it contains starting positions), the player whose turn it is, and a list of all possible moves (these are represented as board positions) from that particular board representation. It then calls itself and passes the new board positions (the possible moves) to itself, thus creating a new list of possible board combinations based on the previous possible movements.

<section class="code">
`\(\large{ \text{game-tree} \\
{\rm~~}\Rightarrow \text{player} \\
{\rm~~}\Rightarrow \text{board} \\
{\rm~~}\Rightarrow \text{possible-moves} \\
{\rm~~~~}\Rightarrow \text{game-tree} \\
{\rm~~~~~~}\Rightarrow \text{player} \\
{\rm~~~~~~}\Rightarrow \text{board} \\
{\rm~~~~~~}\ldots \ldots \ldots \\
{\rm~~~~~~}\ldots \ldots \ldots \\
}\)`
</section>

Let's generate a random example to give some visual imagery to what we're talking about.

<section class="shell">
<section class="highlight">
    (0 #((0 2) (1 2) (1 1) (1 1))
</section>
     (((0 2)
       (0 #((0 1) (1 2) (0 1) (1 1))
        ((NIL
          (1 #((0 1) (1 2) (0 1) (1 1))
           (((1 0)
             (1 #((1 1) (1 1) (0 1) (1 1))
              ((NIL (0 #((1 1) (1 1) (0 1) (1 1)) NIL)))))))))))
      ((0 3)
       (0 #((0 1) (1 2) (1 1) (0 1))
        ((NIL
          (1 #((0 1) (1 2) (1 1) (0 1))
           (((1 3)
             (1 #((0 1) (1 1) (1 1) (1 1))
              ((NIL (0 #((0 1) (1 1) (1 1) (1 1)) NIL)))))
            ((1 0)
             (1 #((1 1) (1 1) (1 1) (0 1))
              ((NIL (0 #((1 1) (1 1) (1 1) (0 1)) NIL)))))))))))))
</section>

This is the result of calling the `game-tree` function.

The higlighted section designates the player whose turn it is, `0` is for player `A` and `1` is for player `B`; at the moment it is player `A`'s turn. The next bit is the present board: here it's saying that player `A` occupies one square of the board with 2 dice, while player `B` has 3 squares with 2, 1, and 1 die respectively.

<section class="shell">
    (0 #((0 2) (1 2) (1 1) (1 1))
<section class="highlight">
     (((0 2)
</section>
       (0 #((0 1) (1 2) (0 1) (1 1))
        ((NIL
          (1 #((0 1) (1 2) (0 1) (1 1))
           (((1 0)
             (1 #((1 1) (1 1) (0 1) (1 1))
              ((NIL (0 #((1 1) (1 1) (0 1) (1 1)) NIL)))))))))))
<section class="highlight">
      ((0 3)
</section>
       (0 #((0 1) (1 2) (1 1) (0 1))
        ((NIL
          (1 #((0 1) (1 2) (1 1) (0 1))
           (((1 3)
             (1 #((0 1) (1 1) (1 1) (1 1))
              ((NIL (0 #((0 1) (1 1) (1 1) (1 1)) NIL)))))
            ((1 0)
             (1 #((1 1) (1 1) (1 1) (0 1))
              ((NIL (0 #((1 1) (1 1) (1 1) (0 1)) NIL)))))))))))))
</section>

These instead are the possible moves of `A` (the first number designates the player). He can either go to the third square or the fourth square (the count starts at 0, 0 is square 1, 1 is square 2, etc.)

<section class="shell">
    (0 #((0 2) (1 2) (1 1) (1 1))
<section class="highlight">
     (((0 2)
       (0 #((0 1) (1 2) (0 1) (1 1))
        ((NIL
          (1 #((0 1) (1 2) (0 1) (1 1))
           (((1 0)
</section>
             (1 #((1 1) (1 1) (0 1) (1 1))
              ((NIL (0 #((1 1) (1 1) (0 1) (1 1)) NIL)))))))))))
      ((0 3)
       (0 #((0 1) (1 2) (1 1) (0 1))
        ((NIL
          (1 #((0 1) (1 2) (1 1) (0 1))
           (((1 3)
             (1 #((0 1) (1 1) (1 1) (1 1))
              ((NIL (0 #((0 1) (1 1) (1 1) (1 1)) NIL)))))
            ((1 0)
             (1 #((1 1) (1 1) (1 1) (0 1))
              ((NIL (0 #((1 1) (1 1) (1 1) (0 1)) NIL)))))))))))))
</section>

If `A` goes to square 3 then the results will be as follows, It is still player `A`'s turn, and he will occupy squares 1 and 2 with 1 die each, while `B` will have squares 2 and 4 with 2 and 1 die respectively.

However with the present board he has no more possible moves, hence the `NIL`, and the turn will pass to `B`, who has only one move open to him.

So we can see that `game-tree` will create a [tree](http://www.squidoo.com/computer-trees#module37206622) of all possible moves given a starting position. This is the data that we will pass into our functions, which will then transform the data (in our case as you will soon see, it will merely chop off irrelevant moves) and then recursively pass it again into the said functions.

### `handle-human`
Now we pass on to the function that handles the human decisions regarding which squares to attack. The code looks like this

<section class="code">
`\(\large{ \text{handle-human} \leftarrow \text{tree}\\
{\rm~~}\Rightarrow \text{list of moves for player} \\
{\rm~~~~}\rightarrow \text{choose-a-move} \\
{\rm~~~~~~}\Rightarrow \text{returns the branches (or nodes) belonging to that move (let's call this new-tree)} \\
{\rm~~~~~~}\Rightarrow \text{handle-human} \leftarrow \text{new-tree}\\
}\)`
</section>

So what's happening here? `handle-human` takes a tree with all the possible moves from the present position as its argument, displays the moves to the player, and waits for a choice. The player chooses a move, then `handle-function` will take the branches of the tree that belong to the move (which contain all the possible moves from that position), discard the other nodes (they are no longer relevant to the game), and call itself with the newly-revised tree as the new input.

Let's show a possible execution as an example. We'll use the same tree as in the previous example to make it easier:

<section class="shell">
    (0 #((0 2) (1 2) (1 1) (1 1))
     (((0 2)
       (0 #((0 1) (1 2) (0 1) (1 1))
        ((NIL
          (1 #((0 1) (1 2) (0 1) (1 1))
           (((1 0)
             (1 #((1 1) (1 1) (0 1) (1 1))
              ((NIL (0 #((1 1) (1 1) (0 1) (1 1)) NIL)))))))))))
      ((0 3)
       (0 #((0 1) (1 2) (1 1) (0 1))
        ((NIL
          (1 #((0 1) (1 2) (1 1) (0 1))
           (((1 3)
             (1 #((0 1) (1 1) (1 1) (1 1))
              ((NIL (0 #((0 1) (1 1) (1 1) (1 1)) NIL)))))
            ((1 0)
             (1 #((1 1) (1 1) (1 1) (0 1))
              ((NIL (0 #((1 1) (1 1) (1 1) (0 1)) NIL)))))))))))))
</section>

`handle-human` will display two possible moves to player `A`.

<section class="shell">
    (0 2) -> square one to three
    (0 3) -> square one to four
</section>

Let's say `A` chooses the second choice, square one to four. `handle-human` then takes the nodes belonging to that particular choice and discard the rest.

<section class="shell">
    (0 #((0 2) (1 2) (1 1) (1 1))
     (((0 2)
       (0 #((0 1) (1 2) (0 1) (1 1))
        ((NIL
          (1 #((0 1) (1 2) (0 1) (1 1))
           (((1 0)
             (1 #((1 1) (1 1) (0 1) (1 1))
              ((NIL (0 #((1 1) (1 1) (0 1) (1 1)) NIL)))))))))))
<section class="highlight">
      ((0 3)
       (0 #((0 1) (1 2) (1 1) (0 1))
        ((NIL
          (1 #((0 1) (1 2) (1 1) (0 1))
           (((1 3)
             (1 #((0 1) (1 1) (1 1) (1 1))
              ((NIL (0 #((0 1) (1 1) (1 1) (1 1)) NIL)))))
            ((1 0)
             (1 #((1 1) (1 1) (1 1) (0 1))
              ((NIL (0 #((1 1) (1 1) (1 1) (0 1)) NIL)))))))))))))
</section>
</section>

This new tree is then passed again to `handle-human` which will determine if there are possible moves from the new position. According to the tree there are none, as evidenced by the `NIL`. The turn then passes to player `B`, who has two choices: square two to three or square two to one.

<section class="shell">
      ((0 3)
       (0 #((0 1) (1 2) (1 1) (0 1))
        ((NIL
          (1 #((0 1) (1 2) (1 1) (0 1))
<section class="highlight">
           (((1 3)
</section>
             (1 #((0 1) (1 1) (1 1) (1 1))
              ((NIL (0 #((0 1) (1 1) (1 1) (1 1)) NIL)))))
<section class="highlight">
            ((1 0)
</section>
             (1 #((1 1) (1 1) (1 1) (0 1))
              ((NIL (0 #((1 1) (1 1) (1 1) (0 1)) NIL)))))))))))))

</section>

Player `B` takes the first choice, square two to three, and `handle-human` again picks up all the child nodes belonging to that particular branch and drops the rest.

<section class="shell">
<section class="highlight">
           (((1 3)
             (1 #((0 1) (1 1) (1 1) (1 1))
              ((NIL (0 #((0 1) (1 1) (1 1) (1 1)) NIL)))))
</section>
</section>

And so on until there are no possible moves and a draw or a winner is declared.

### So what happened exactly?
So what's so special about the way this game was coded?

What stands out here is that there were no changes of state with regards to the data; the initial data was not modified in any way.

1. `game-tree` takes a starting board and produces all possible moves.
2. `handle-human` takes the tree and displays the moves available.
3. player makes his choice, and `handle-human` makes a new tree based on the choice made.
4. go back to step 2

This is an exemplary representation of the concept ***"data first"***. The code was based around the data given by `game-tree` and worked around transforming that data based on player decisions. Each transformation gave rise to a **new** tree; the old data was not touched, hence there was no assigning of variables, no book-keeping of state, of any kind.

This is what ***Functional Pipelines*** are all about, taking initial data and passing it around from function to function, each function taking the result of the previous function as its input. The advantage of this type of programming is that since there is no (or minimal) variable assignment, you can more or less be sure that the functions will always have the same result if they are called with the same input. This makes debugging code incredibly easy and hassle-free.

*Of course some state is inevitable, but the idea here is to keep it to an absolute minimum.*

### Where to Go from Here
The complete source (the AI code is missing from this example) can be found in the book [Land of Lisp](http://landoflisp.com). 

Joel's [Can your Programming Language do This?](http://www.joelonsoftware.com/items/2006/08/01.html) (talks about first-class functions and the map and reduce functions)

