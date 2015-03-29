# Computation of the n-th digit of π with low memory

**This is a work in progress and has not been validated yet**

These Racket modules implement an algorithm to compute *single decimals* of the
sequence of π digits, using only a small amount of memory.

This work is based on [Xavier Gourdon's paper]
(http://numbers.computation.free.fr/Constants/Algorithms/nthdecimaldigit.pdf)

## Overall design

The algorithm has two parts, both in `main.rkt`:

- the approximation to the n-th digit of π, which is implemented in the
  function `(pi-digit n n_0)`

- the sum of combinatorial numbers modulo another number, avoiding to compute
  the factorials involved in the combinatorial numbers, which is a part of the
  previous approximation, and is implemented in the function `(pi-digit n n_0)` 

`primes.rkt` holds some helper functions to decompose a number in its prime
factors.

## Implementation notes

Most functions have a contract.  This may obscure the reading, and is possibly
not worth the effort for such a short program, but it is intended to help
porting the modules to [Typed Racket](http://docs.racket-lang.org/ts-guide/)
once the whole program works.

There is a `while` macro, and several functions that use an imperative style,
because those were ported from a previous effort to implement the same
algorithm in C.  Mostly because prefix notation in Lisp can be quite confusing
for complex algebraic expressions; here they are simplified in many cases by
using the [Racket reader infix
notation](http://docs.racket-lang.org/guide/Pairs__Lists__and_Racket_Syntax.html).

Tests and implementations are interleaved in the same files, by repeatedly
using `module+` to define an inner test submodule.  You can run all unit
tests with `raco test *.rkt`.
