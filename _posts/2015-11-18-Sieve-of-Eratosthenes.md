---
layout: post
title: The Sieve of Eratosthenes
---

### Sieve of what??
The Sieve of Eratosthenes is an ancient algorithm for finding all prime numbers up to a given limit. It is one of the most efficient ways of finding small prime numbers, however above a certain threshold other sieves like the [Sieve of Atkin](https://en.wikipedia.org/wiki/Sieve_of_Atkin) or [Sieve of Sundaram](https://en.wikipedia.org/wiki/Sieve_of_Sundaram) should be considered instead.

The sieve itself is quite simple to understand, and translating it to code is very straightforward, however since writing a blog post about current exercises is *highly encouraged* by a course that I'm taking right now, I took the opportunity to start writing again.

### How it works
The Sieve of Eratosthenes works by identifying a prime within the range of numbers you're considering (ex. 1 to 25), then crossing out all ***multiples*** of that prime (since it's a multiple it obviously can't be a prime...).

<div>
  $$
    \left[\begin{matrix}
    1 & 2 & 3 & 4 & 5 \\
    6 & 7 & 8 & 9 & 10 \\ 
    11 & 12 & 13 & 14 & 15 \\ 
    16 & 17 & 18 & 19 & 20 \\ 
    21 & 22 & 23 & 24 & 25 \\ 
    \end{matrix}\right]
  $$
</div>

Starting with 2, the smallest prime, you then eliminate all numbers that are multiples of 2.

<div>
  $$
    \require{cancel}
    \left[\begin{matrix}
    1 & 2 & 3 & \cancel{4} & 5 \\
    \cancel{6} & 7 & \cancel{8} & 9 & \cancel{10} \\ 
    11 & \cancel{12} & 13 & \cancel{14} & 15 \\ 
    \cancel{16} & 17 & \cancel{18} & 19 & \cancel{20} \\ 
    21 & \cancel{22} & 23 & \cancel{24} & 25 \\ 
    \end{matrix}\right]
  $$
</div>

Which leaves us with:

<div>
  $$
    \left[\begin{matrix}
    1 & 2 & 3 &  & 5 \\
     & 7 &  & 9 &  \\ 
    11 &  & 13 &  & 15 \\ 
     & 17 &  & 19 &  \\ 
    21 &  & 23 &  & 25 \\ 
    \end{matrix}\right]
  $$
</div>

Then you take the next number after that prime, in this case 3, and do it all over again.

<div>
  $$
    \left[\begin{matrix}
    1 & 2 & 3 &  & 5 \\
     & 7 &  & \cancel{9} &  \\ 
    11 &  & 13 &  & \cancel{15} \\ 
     & 17 &  & 19 &  \\ 
    \cancel{21} &  & 23 &  & 25 \\ 
    \end{matrix}\right]
  $$
</div>

Leaving us:

<div>
  $$
    \left[\begin{matrix}
    1 & 2 & 3 &  & 5 \\
     & 7 &  &  &  \\ 
    11 &  & 13 &  &  \\ 
     & 17 &  & 19 &  \\ 
     &  & 23 &  & 25 \\ 
    \end{matrix}\right]
  $$
</div>

And again take the next prime, 5, and eliminate all multiples of 5 (25 seems to be the only victim here). Eventually the numbers remaining will be:

<div>
  $$
    \left[ \begin{matrix} 2 & 3 & 5 & 7 & 11 & 13 & 17 & 19 & 23 \end{matrix} \right ]
  $$
</div>

*`\(\ast\)` 1 is not considered a prime number.*

So as you can see, the technique is quite simple and effective, albeit a little time-consuming, especially for large ranges. This is where computers enter the stage.

### The Essence of Programming
...is to make the computer do the repetitive, computational-demanding work. The course I'm taking uses Ruby so I've coded this in Ruby as well.

#### Breaking it down
First, we need to create the sequence of numbers, given a limit as input.

{% highlight ruby %}
def create_range(limit)
    (2..limit).to_a
end
{% endhighlight %}

Then we have the part of the program that removes all the multiples of the given prime. Here we use the `select` method to filter out all numbers that **DO NOT** leave a remainder (hence a multiple) when divided with the prime.

{% highlight ruby %}
def remove_multiple(prime, range)
    range.select { |x| x % prime  != 0 }
end
{% endhighlight %}

Then we just iterate over the range, methodically removing numbers until we are only left with the primes.

{% highlight ruby %}
range = create_range(limit)
self.primes = [2]
limit.times do
  range = remove_multiple(primes.last, range)
  primes.push(range.first) if range.first != nil
end
{% endhighlight %}

### Testing it
Here are a few runs to make sure that it works:

Numbers 1 - 10.

    2.0.0-p647 :561 >   Sieve.new(10).primes
     => [2, 3, 5, 7]

Numbers 1 - 25

    2.0.0-p647 :524 > Sieve.new(25).primes
     => [2, 3, 5, 7, 11, 13, 17, 19, 23] 

Numbers 1-100

    2.0.0-p647 :576 > Sieve.new(100).primes
     => [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97] 

The complete source (I had to use a class since the test-suite provided assumed a class, although for these types of problems I would have just used functions normally):

{% highlight ruby %}
class Sieve
  attr_accessor :primes

  def initialize(limit)
    range = create_range(limit)
    self.primes = [2]
    limit.times do
      range = remove_multiple(primes.last, range)
      primes.push(range.first) if range.first != nil
    end
  end

  def create_range(limit)
    (2..limit).to_a
  end

  def remove_multiple(prime, range)
    range.select { |x| x % prime  != 0 }
  end
end
{% endhighlight %}
