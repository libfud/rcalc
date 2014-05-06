rcalc is a polish notation calculator. This means that the operator is written
before the terms. For example, (+ 2 2) is equal to 4, and (+ 2 3 (/ 10 2)) is
equal to 10. The regular notation for these kinds of expressions is called infix,
and these would be respectively written as 2 + 2 and 2 + 3 + 10 / 2.

This is being written with the nightly version of rust, so if you're using an
older version, you may not be able to compile it.

In order to build it, ensure that you have downloaded or built the latest nightly
version of rust, and simply run

rustc calculator.rs

This will compile a binary file called rcalc.

rcalc is still immature, and lacks many features. It currently can perform
the arithmatic operations (addition, subtraction, multiplication, and division),
perform exponentiation with ~~integer~~ rational exponents, and perform sine, cosine and
tangent functions.
