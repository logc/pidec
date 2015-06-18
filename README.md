# Computation of the n-th digit of π with low memory

These Racket modules implement an algorithm to compute *single decimals* of the
sequence of π digits, using only a small amount of memory.

This work is based on [Fabrice Bellard's paper](http://bellard.org/pi/pi_n2/pi_n2.html)
and his published code can be found [here](http://bellard.org/pi/pi.c).

The implementation is in the module `bellard.rkt`, while its tests are defined
in `test-bellard.rkt`. The first one is written in Typed Racket, while the
second one is plain Racket. Apparently, Typed Racket does not support defining
tests in submodules of the same file as the implementation. The typed
implementation might be further optimized by constraining the types even more.
 

## Installation

You will need to have Racket > 6.1 installed. Currently, these modules are
**not** listed in Racket's package manager, so you will need to clone this
repository.

## Usage

Compile the code with `raco`, and execute it passing a single number, which is
the starting position for 10 π decimal places.

    $ raco exe bellard.rkt
    $ ./bellard 50

Pass the tests with `raco`

    $ raco test test-bellard.rkt

## License

TBD

## Contributors

Sebastián Ortega (sortega)
