---
layout: post
title: SLDB, or the Slime Debugger
---

### Introducing the debugger
I've been using the [Slime](http://common-lisp.net/project/slime/) developing environment for writing Lisp code for sometime now... makes me wonder how I ever did without it. As I got more and more familiar with Slime and its various features, I also started getting into using emacs for other things (IRC, email, etc.) but that's another story for another time, the last thing we need is another Emacs post (there are so many, I highly recommend [DJCB's page on Emacs](http://emacs-fu.blogspot.com)).

One aspect that I never delved into in any detail though was the debugger, after all, I had the mighty `print` statement at my disposal for all my debugging needs didn't I?

Except that I started to come across certain limitations with this approach, which led me to start tracing going through code manually, trying to find out if the variable was a list or array, was the output a nested list or not, etc. I started thinking that there must be a better way and then I suddenly remembered that there was an in-built debugger that I constantly interacted with (mainly all I did was press `q` to get out of it) and that I had never explored its full potential. Granted that documentation on the use of the debugger was scarce (this is the world of Lisp after all), and so I had to do a lot self-experimentation.

### Invoking the beast
SLDB will automatically pop-up whenever you try to compile code that has errors, such as calling functions with missing arguments, syntax issues in the code, etc. It looks like this:

<section class="shell">
    The variable A is unbound.
       [Condition of type UNBOUND-VARIABLE]

    Restarts:
     0: [RETRY] Retry SLIME REPL evaluation request.
     1: [*ABORT] Return to SLIME's top level.
     2: [ABORT] Abort thread (#<THREAD "repl-thread" RUNNING {1003B90113}>)

    Backtrace:
      0: (SB-KERNEL::UNBOUND-SYMBOL-ERROR-HANDLER ..)
      1: (SB-KERNEL:INTERNAL-ERROR #.(SB-SYS:INT-SAP #X7FFFF4975680) #<unavailable argument>)
      2: ("foreign function: #x418B2F")
</section>

This is basically saying that we tried to use a non-existing variable `A`, and the options are either to:

1. Retry the exact same evaluation (*I guess in the hopes that the computer made a mistake?*)
2. Return to the REPL with the last command not evaluated
3. Abort the entire thread

The Backtrace will show the step by step evaluation of the commands up to the point that the error occured.

And the code that produced it was this:

<section class="shell">
    CL-USER> (defun test (a b)
               (+ a b))
    TEST
    CL-USER> (test 1 a)
</section>

So we defined a function `test` that took two arguments, `a` and `b`, and tried to add them with the `+` operator. The `+` operator can only combine numbers, it doesn't work for strings, or combinations of numbers and strings. This is demonstrated when we try to run the following code:

<section class="shell">
    (+ "hello" "there")
    ....
    ....
    ....
    The value "hello" is not of type NUMBER.
       [Condition of type TYPE-ERROR]
</section>

In the previous example, we tried to add together a number (1) and a variable `a`, which was [unbound](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node58.html), which means it had no inherent value. This error dropped us into the debugger immediately, where we were presented with the following options to try to correct the error.

### Backtraces
Backtraces, as mentioned earlier, are a record of the code being evaluated by the compiler up until it encounters an error and cannot continue. While most of it is incomprehensible to all except a very select few...

<section class="shell">
    Backtrace:
      0: (SB-KERNEL::OBJECT-NOT-TYPE-ERROR-HANDLER ..)
          Locals:
            SB-DEBUG::ARG-0 = :<NOT-AVAILABLE>
            SB-DEBUG::ARG-1 = #.(SB-SYS:INT-SAP #X7FFFF5215BC0)
            SB-DEBUG::ARG-2 = #<SB-ALIEN-INTERNALS:ALIEN-VALUE :SAP #X7FFFF5215740 :TYPE (* (STRUCT SB-VM::OS-CONTEXT-T-STRUCT))>
            SB-DEBUG::ARG-3 = (149 21)
      1: (SB-KERNEL:INTERNAL-ERROR #.(SB-SYS:INT-SAP #X7FFFF5215740) #<unavailable argument>)
          Locals:
            SB-DEBUG::ARG-0 = #.(SB-SYS:INT-SAP #X7FFFF5215740)
            SB-DEBUG::ARG-1 = :<NOT-AVAILABLE>
      2: ("foreign function: #x418B2F")
          [No Locals]
</section>
*Backtrace with all variables exposed*

...there is very useful information that can be gleaned from them. One of the most imortant things that can be retrieved are the values of variables at each stage of the execution.

For example, with the following source:

<section class="code">
  {% highlight cl %}
    (defun test (lst)
      (print (+ (car lst) (cadr lst)))
      (test (cdr lst)))
  {% endhighlight %}
</section>

<section class="shell">
    CL-USER> (test '(1 2 3 4 5 6)))
</section>

The debugger will give us:

<section class="shell">
    Argument Y is not a NUMBER: NIL
       [Condition of type SIMPLE-TYPE-ERROR]

    Restarts:
     0: [RETRY] Retry SLIME REPL evaluation request.
     1: [*ABORT] Return to SLIME's top level.
     2: [ABORT] Abort thread (#<THREAD "repl-thread" RUNNING {1003BD8113}>)

    Backtrace:
      0: (SB-KERNEL:TWO-ARG-+ 6 NIL)
          Locals:
            SB-DEBUG::ARG-0 = 6
            SB-DEBUG::ARG-1 = NIL
      1: (TEST (6))
</section>

The first thing to look at here is the first line at the top, telling us that our function just tried to use `NIL` as a number. Wait a minute... there is no instance of `nil` anywhere in our code, or the initial list passed onto the function... so where could it have come from?

The second important piece is the first frame in the Backtrace `0: (SB-KERNEL:TWO-ARG-+ 6 NIL)` it looks like we just tried to pass `6` and `NIL` to the operator `+`. This is confirmed once we look at the variables in that instance of execution, under `Locals:` (meaning local variables), we see that the first argument `ARG-0` is 6, and the second one `ARG-1` is `NIL`. 

We know that 6 is the last number in the list we passed to the function, and since the function applies `cdr` to the list through each iteration, it must mean that the error occured once the function reached the end of the list. This is verified when we look at the second stack in the backtrace `1: (TEST (6))`. It says we tried to call our function with one argument (this makes sense because the list gets smaller and smaller every time the function is called). 

So we eventually get to the point where 6 is the final remaining element in the list, and therefore the **first** element, and yet we still don't know where `NIL` comes from...

Actually it all makes perfect sense, remember that a list always contains `NIL` at the end, since a list is merely an abbreviation of the construct `cons` and `cons` cells always have a `NIL` at the end.

(cons 1 nil) + (cons 2 nil) `\(\rightarrow\)` (cons 1 (cons 2)) `\(\rightarrow\)` (list 1 2)

The `NIL` at the end allow the linking of other cons cells to the existing chain, the end of the list will always contain a `NIL` object. When we get to the end of the list, our function tried to add the last number (6) and the `NIL` object together, as evidenced in `ARG-0` and `ARG-1`, this created an error and dropped us into the debugger. 

### The Inspector
The Inspector is a tool that is used to determine the current status of a data structure or object. It displays (in human-readable form) a short summary of the object followed by a list of the internal state. Here you can modify the bound (and unbound) values, reset them, inspect them recursively.

*example taken from the **slime** section of [lisp-book](http://lisp-book.org)*

<section class="shell">
    CL-USER> (defclass foo () (a  b c))
    #<STANDARD-CLASS FOO>
    CL-USER> (make-instance *)
    #<FOO {100426DEE3}>
    CL-USER> 
</section>

Enter the Inspector with `C-c I` and enter `*` when asked for the value to inspect (this tells the inspector to inspect the lastest value evaluated by the REPL). You should be presented with the following output.

<section class="shell">
    #<FOO {100426DEE3}>
    --------------------
    Class: #<STANDARD-CLASS FOO>
    --------------------
     Group slots by inheritance [ ]
     Sort slots alphabetically  [X]

    All Slots:
    [ ]  A = #<unbound>
    [ ]  B = #<unbound>
    [ ]  C = #<unbound>

    [set value]  [make unbound]
</section>

As you can see there is a short description of the object at the top and summary of the current state. You can select the slots and either change or clear the bound values. Set the value of slot `A` to 45, for example, then go back to the REPL and verify that the change has been implemented.

<section class="shell">
    #<FOO {100426DEE3}>
    --------------------
    Class: #<STANDARD-CLASS FOO>
    --------------------
     Group slots by inheritance [ ]
     Sort slots alphabetically  [X]

    All Slots:
    [X]  A = 45
    [ ]  B = #<unbound>
    [ ]  C = #<unbound>

    [set value]  [make unbound]
</section>

<section class="shell">
    CL-USER> (slot-value * 'a)
    45
</section>

You can see that the instance of class `foo` that we modified in the debugger has been updated.

### Stepping through manually
Another great way to debug code, and one which until recently I've always one manually, is to step through the code, inspecting the variables at each point. An easy way to do this is to add a [break point](http://en.wikipedia.org/wiki/Breakpoint), this is triggered by inserting `(break)` at the point in the source where you want to inspect the current state. Using the previously defined function `test`, we add `(break)` just before we call the function recursively. This will allow us to see what happens to the variable `lst` as we call the function again and again.

<section class="code">
  {% highlight cl %}
    (defun test (lst)
      (print (+ (car lst) (cadr lst)))
      (break)
      (test (cdr lst)))
  {% endhighlight %}
</section>

This will produce the following response from the debugger:

<section class="shell">
    break
       [Condition of type SIMPLE-CONDITION]

    Restarts:
     0: [CONTINUE] Return from BREAK.
     1: [RETRY] Retry SLIME REPL evaluation request.
     2: [*ABORT] Return to SLIME's top level.
     3: [ABORT] Abort thread (#<THREAD "repl-thread" RUNNING {1003B90113}>)

    Backtrace:
      0: (BREAK "break")
      1: (TEST (1 2 3 4 5))
          Locals:
            SB-DEBUG::ARG-0 = (1 2 3 4 5)
      2: (SB-INT:SIMPLE-EVAL-IN-LEXENV (TEST '(1 2 3 4 5)) #<NULL-LEXENV>)
      3: (EVAL (TEST '(1 2 3 4 5)))
</section>

As we can see from the top description, the debugger was invoked by the `break` statement. We can then inspect the local variables at `1: (TEST (1 2 3 4 5))` and see that the function `test` was evaluated with the list `(1 2 3 4 5)`. We can then continue stepping through the code by either choosing **`0`** from the restart options displayed or by pressing **`s`**. The second iteration of the function will then give us:

<section class="shell">
    break
       [Condition of type SIMPLE-CONDITION]

    Restarts:
     0: [CONTINUE] Return from BREAK.
     1: [RETRY] Retry SLIME REPL evaluation request.
     2: [*ABORT] Return to SLIME's top level.
     3: [ABORT] Abort thread (#<THREAD "repl-thread" RUNNING {1003B90113}>)

    Backtrace:
      0: (BREAK "break")
      1: (TEST (2 3 4 5))
          Locals:
            SB-DEBUG::ARG-0 = (2 3 4 5)
</section>

This is almost exactly the same as the previous message, except that the values which are passed onto the function are now `(2 3 4 5)` instead of `(1 2 3 4 5)`. This is because our function chops off the first element of the list each time it calls itself. As you continue stepping through, you can observe as the function will continue to truncate the arguments.

<section class="shell">
    Backtrace:
      0: (BREAK "break")
      1: (TEST (3 4 5))
          Locals:
            SB-DEBUG::ARG-0 = (3 4 5)
    ...

    Backtrace:
      0: (BREAK "break")
      1: (TEST (4 5))
          Locals:
            SB-DEBUG::ARG-0 = (4 5)

    ...

    Backtrace:
      0: (SB-KERNEL:TWO-ARG-+ 5 NIL)
      1: (TEST (5))
          Locals:
            SB-DEBUG::ARG-0 = (5)
    
</section>

### Learning more
We've only just scratched the surface of using the debugger, there are many more techniques and features that are discussed elsewhere. I suggest reading the *slime* section of the [lisp-book](http://lisp-book.org), as well as the [SLDB chapter of the Slime manual](http://common-lisp.net/project/slime/doc/html/Debugger.html#Debugger) for a more complete list of commands available. 

I also recommend hanging out at the **IRC** chatrooms and asking the more veteran hackers about using SLDB. `#lisp` at [irc.freenode.net](http://freenode.net/) is full of helpful people *as long as you ask relevant and intelligent* questions.

And last but not least is **experiment**!! Everytime you get dropped into the debugger because of some error, poke around the stack and just try to understand what's going on, it will seem like a lot gobbleygook in the beginning but eventually you will become familiar with the error messages and start to understand what the debugger is trying to say.

Have fun! 