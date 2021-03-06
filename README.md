# Computation of the n-th digit of π with low memory

Pidec implements an algorithm to compute *single decimals* of the sequence of π
digits, using only a small amount of memory.

[![Build Status](https://travis-ci.org/logc/pidec.svg?branch=master)](https://travis-ci.org/logc/pidec)

## References

This work is based on [Fabrice Bellard's paper](http://bellard.org/pi/pi_n2/pi_n2.html)
and his published code can be found [here](http://bellard.org/pi/pi.c).

The implementation is in the module `main.rkt`, while its tests are defined in
`test-main.rkt`. The first one is written in Typed Racket, while the second one
is plain Racket. Apparently, Typed Racket does not support defining tests in
submodules of the same file as the implementation.
 

## Installation

You will need to have Racket > 6.1 installed. Use Racket's built-in package manager:

    $ raco pkg install pidec

## Usage

Compile the code with `raco`, and execute it passing a single number, which is
the starting position for 10 π decimal places.

    $ raco exe bellard.rkt
    $ ./bellard 50

Pass the tests with `raco`

    $ raco test test-bellard.rkt

## License

These module are licensed under the MIT license. See LICENSE for details.

## Contributors

Sebastián Ortega (sortega)
