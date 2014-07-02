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

`make`

To run rcalc, just use

`bin/rcalc`


This will compile a binary file called rcalc.

## Goals

rcalc is still immature, and lacks many features. It currently can perform
the arithmatic operations (addition, subtraction, multiplication, and division),
perform exponentiation with (some) rational integer exponents, and perform sine, 
cosine and tangent functions. Additionally, ordering and equality for boolean 
functions are present, as well as AND, OR and NOT. User defined functions
which allow recursion are also present.

rcalc will not be considered complete until it has the following:

* ~~proper rational exponentiation~~
* limits
* matrices and trees
* help message for every feature, builtin function, and an interface for 
documenting user defined functions
* graphing

## Features 

(List not necessarily complete)

* Arbitrary precision numbers
* Trigonometric functions
* Rational exponentiation
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