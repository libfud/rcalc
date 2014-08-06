rcalc
=====

rcalc is a programmable polish notation calculator. This means that the operator 
is written before the terms. For example, 
`(+ 2 2)` is equal to 4, and `(+ 2 3 (/ 10 2))` is equal to 10. The regular notation 
for these kinds of expressions is called infix, and these would be respectively
written as `2 + 2` and `2 + 3 + 10 / 2.`

This is being written with the nightly version of rust, so if you're using an
older version, you may not be able to compile it.

## Compiling

In order to build it, ensure that you have downloaded or built the latest nightly
version of rust, the development headers for readline if you use linux, make sure
you have the development headers for readline. Then run

`cargo build`

Ignore the errors for now. Until I can figure out what I'm doing wrong, just use
it to build the library for rust-image (or install it some other way that you
can link against it), and copy or move the library it builds to src/lib.
Then run 

`make`

To run rcalc, just use

`bin/rcalc`


This will compile a binary file called rcalc.

## Goals

As far as I'm concerned, this is 1.0 now. I've got plans in the future for
rcalc, but this is going to go into maintenance mode for a little while.
I'll continue to keep up (or try to) with the latest rust nightlies,
but I'm not going to actively develop this for a while.

## Features 

(List not necessarily complete)

* Graphing
* Arbitrary precision numbers
* Trigonometric functions
* Rational exponentiation
* Matrices: dot product, inversion, determinants, translations. 
  Code present but not integrated yet for minors.
* Boolean values, conditional statements and short circuiting operators
* Comparisons for ordering and equality
* Lists, constructing and destructuring, map, reduce, filter and sort
* User define variables and functions
* Functions are first class, and recursion is possible.
* Anonymous functions can be used as arguments in lieu of named functions
* There is a table function which will print the result of a function applied
to each element in a list. See link below for screenshots of it in action.

## Images of rcalc in action

http://www.librehumanitas.org/images/fib.png


http://www.librehumanitas.org/images/phi.png


http://www.librehumanitas.org/images/truthtables.png


http://www.librehumanitas.org/images/matrix.png