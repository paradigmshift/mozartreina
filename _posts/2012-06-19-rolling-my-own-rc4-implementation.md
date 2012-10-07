---
layout: post
title: Rolling my own RC4 Implementation
---

### What is RC4

RC4, or the "Rivest Cipher", named after its creator Ron Rivest, is a popular stream cipher used in encryption protocols like [WEP](http://en.wikipedia.org/wiki/Wired_Equivalent_Privacy) and [WPA](http://en.wikipedia.org/wiki/Wi-Fi_Protected_Access). A stream cipher is a system where plaintext is combined with a *cipher digit stream* (for us mortals, a group of, usually random, numbers) to create a "ciphertext" stream. The cipher digit stream is generated from a seed value, which also servers as the decryption key for the ciphertext stream.

### Ok, so why are you creating your own implementation?

In the wake of the famous [Sony exploits](http://www.wired.com/gamelife/2011/05/sony-online-entertainment-hack/), as well as the breaching of [LastPass](http://www.pcworld.com/article/227268/lastpass_ceo_explains_possible_hack.html) and the recent LinkedIn [break-ins](http://www.forbes.com/sites/reuvencohen/2012/06/06/linkedin-hacked-a-few-apps-suggestions-for-protecting-your-online-passwords/), the last few months has seen a growing interest in the field of cryptography and security in general. 

A few months ago on [Hacker News](http://news.ycombinator.com) I read an article similar to [this one](http://lifehacker.com/5715794/how-to-write-down-and-encrypt-your-passwords-with-an-old+school-tabula-recta) (can't find the original one anymore) that talked about choosing a passphrase based on the website or service you were logging into, and running a local encryption protocol on it and using that as your password. The obvious downside to this is that without the encryption program, you can't really log on to your account, which means that you're basically limited to using your own computer or putting up the encryption system on some web app so that you (*and anyone else....*) can always have access.

I don't do this exactly for that reason, I usually log on to my email account from various computers and so this solution is not the ideal one for me. However it did get me interested in looking into how to roll your own implementation of some of the more well-known cipher algorithms and in cryptology in general. In the end this prompted me to do some research and understand how these algorithms work. My choice of the RC4 protocol was purely chance, it was one of the first ones I came across and it seemed simple enough to implement.

[Wikipedia entry on RC4](http://en.wikipedia.org/wiki/RC4)

### Algorithm

The RC4 algorithm consists of two main parts: the key-scheduling algorithm (**KSA**) and pseudo-random generation algorithm (**PRGA**).

The KSA is a permutation of all possible 256 bytes based on the key or passphrase which itself is from 40-128 bits long.

**Pseudocode:**

<section class="code">
`\(
\large{ \text{for i from } 0 \mbox{ to } 255 \\
{\rm ~~}S[i] := i \\
\text{endfor} \\
j := 0 \\
\text{for i from } 0 \mbox{ to } 255 \\
{\rm ~~}j:= (j + S[i] + \mbox{key}[i {\rm ~mod ~keylength}]) \mbox{ mod } 256 \\
{\rm ~~swap ~values ~of ~} S[i] \mbox{ and } S[j] \\
\text{endfor}}
\)`
</section>

**Lisp Implementation:**
  <section class="code">
  {% highlight cl %}
  (defun switch (a b lst)
  "Switch elements at position a & b with each other"
    (let ((newlst (copy-list lst)))
      (psetf (nth a newlst) (nth b newlst)
             (nth b newlst) (nth a newlst))
      newlst))

  (defun list-init ()
    (loop for i from 0 to 255
      collect i))

  (defun key-init (key)
    (let* ((key-char (coerce key 'list))
           (key-list (mapcar #'char-code key-char)))
      key-list))
  {% endhighlight %}
  </section>

The function `list-init` initializes a list with the elements 0 to 255, this is used to scramble the key. `key-init` takes the passphrase in the form of a string, creates a list from that string, then returns the numeric code representation of each element in the list (normally the result would be the binary representation of each element, in this case it is ASCII code). `switch` is the function responsible for swapping the list values with each other.

  <section class="code">
  {% highlight cl %}
  (defun scramble (lst key-list)
    (let ((j 0))
      (dotimes (n 255)
        (setq j (mod (+ j
                        (nth n lst)
                        (nth (mod n (length key-list))
                             key-list))
                     (length key-list)))
        (setq lst (switch n j lst)))
      lst))
  {% endhighlight %}
  </section>

`scramble`, as the function name suggests, begins the permutation of the passphrase (the output of `key-init`) using the list (or array) initialized by `list-init`. Each iteration has an effect not only on the passphrase but also on the list that is used to determine the permutation.

### A Simple Example
*in this particular implementation the computation for **`j`** lacks one step, mod 256*

#### First Iteration
`\[
\begin{array}{l}
S = 0, 1, 2, 3 \\
K = 1, 7, 1, 7 \\
(i = 0, j = 0, S = 0, 1, 2, 3): \\
j = (j + S[i] + K[i]) = 0 + 0 + 1 = 1 \\
\mbox{Swap } S[i] \mbox{with } S[j] = S = 1, 0, 2, 3 \\
\end{array}
\]`  


#### Second Iteration
`\[
\begin{array}{l}
(i = 1, j = 1, S = 1, 0, 2, 3) \\
j = (j + S[i] + K[i]) = (1 + 0 + 7) = 0 \mbox{ mod } 4 \\
\mbox{Swap } S[i] \mbox{with } S[j] = S = 0, 1, 2, 3 \\
\end{array}
\]`  

#### Third Iteration
`\[
\begin{array}{l}
(i = 2, j = 0, S = 0, 1, 2, 3) \\
j = (j + S[i] + K[i]) = (0 + 2 + 1) = 3 \\
\mbox{Swap } S[i] \mbox{with } S[j] = S = 0, 1, 3, 2 \\
\end{array}
\]`

#### Fourth Iteration
`\[
\begin{array}{l}
(i = 3, j = 3, S = 0, 1, 3, 2) \\
j = (j + S[i] + K[i]) = (3 + 2 + 7) = 0 \mbox { mod } 4 \\
\mbox{Swap } S[i] \mbox{with } S[j] = S = 2, 1, 3, 0 \\
\end{array}
\]`

After the key is scrambled it is then used in the **PRGA** subroutine.

### PRGA

**Pseudocode:**

<section class="code">
`\(
\large{ i := 0\\
j := 0\\
\text{while GeneratingOutput}: \\
{\rm ~~~~} i := (i + 1) \mbox{ mod } 256\\
{\rm ~~~~} j := (j + S[i]) \mbox{ mod } 256\\
{\rm ~~~~} \text{ swap values of }S[i] \mbox{ and } S[j]\\
{\rm ~~~~} K := S[(S[i] + S[j]) \mbox{ mod } 256] \\
{\rm ~~~~} \mbox{output } K\\
\text{endwhile}}
\)`
</section>

After each iteration of the loop, the resulting **`K`** is then **XOR**ed with one byte of the message, this progresses in a linear manner (so every time a new computation for **`K`** is done, the next byte of the message is **XOR**ed).

**Lisp Implementation:**
<section class="code">
  {% highlight cl %}
  (defun output-generator (i j s msg result)
    (setq i (mod (+ i 1) 256))
    (setq j (mod (+ j (nth i s)) 256))
    (setq s (switch (nth i s)
                    (nth j s) s))
    (push (boole boole-xor (mod (+ (nth i s)
                                   (nth j s))
                                256)
                 (car msg)) result)
    (if (> (length msg) 1)
        (output-generator (+ 1 i)
                          (+ 1 j)
                          s
                          (subseq msg 1)
                          result)
        (reverse result)))
  {% endhighlight %}
</section>

### Test Run
Using my Lisp implementation, here are some examples:

*passphrase: 123, text: 456*
<section class="shell">
{% highlight console %}
CL@USER$ (code-decode "123" "456")
6Ë2
{% endhighlight %}
</section>

*passphrase: 123, text: 890*
<section class="shell">
{% highlight console %}
CL@USER$ (code-decode "123" "890")
:Ç4
{% endhighlight %}
</section>

### Wait a minute, what about decoding the stream?
Ciphering would be pretty useless if the decoding method didn't work... Let's see what happens when we apply the **XOR**ed ciphertext with the passphrase.

*passphrase: 123, ciphertext: 6Ë2*
<section class="shell">
{% highlight console %}
CL@USER$ (code-decode "123" "6Ë2")
456
{% endhighlight %}
</section>

*passphrase: 123, ciphertext: :Ç4*
<section class="shell">
{% highlight console %}
CL@USER$ (code-decode "123" ":Ç4")
890
{% endhighlight %}
</section>

### Buggy Implementation, leave this type of stuff to the experts
My particular implementation has some notable sideffects, a lot of the times the ciphertext that is outputted contains non printable characters. I'm not quite sure if this is how it's meant to be or if it's a flaw in the code.

This brings home an important point, encryption algorithms are best left to experienced mathematicians and engineers. Mathematicians because the algorithms can be quite complex; too simple and they are too vulnerable, and engineers because implementing the algorithms isn't quite that simple, the code itself has to be robust and be as immune as possible to attacks.

Full [source code](https://github.com/paradigmshift/salt-n-pepper) of my implementation. *Comments to the code are extremely welcome.*

As a side note, regarding your security online, I recommend *salting* your passwords. [How to Salt your Passwords](http://peebs.org/heres-how-to-salt-your-own-passwords-and-prev) and [Avoiding Password Breaches](http://www.readwriteweb.com/archives/avoiding-password-breaches-101-salt-your-hash.php) are a good read for those who don't know where to start.