#lang scribble/manual
@require[@for-label[pidec
                    racket/base]]

@title{pidec}
@author{logc}

@defmodule[pidec]

Pidec implements an algorithm to compute @italic{single decimals} of the
sequence of π digits, using only a small amount of memory.

@section{References}

This work is based on
@hyperlink["http://bellard.org/pi/pi_n2/pi_n2.html"]{Fabrice Bellard's paper}
and his published code can be found
@hyperlink["http://bellard.org/pi/pi.c"]{here}.

The implementation is in the module `main.rkt`, while its tests are defined in
`test-main.rkt`. The first one is written in Typed Racket, while the second one
is plain Racket. Apparently, Typed Racket does not support defining tests in
submodules of the same file as the implementation.

@section{Usage}

@defmodule[pidec/main]

The main exported function is called @tt{main} and it accepts a single
argument, @tt{position}, which is the starting position of the π decimals you
want to compute.

@defproc[
(main [position integer?])
integer
]{
Returns decimals of π at position. Please note that the resulting integer should be padded on the left with a zero if it has less than 9 positions, e.g. for @tt{(main 50)} the procedure answers with @tt{58209749}, while the true answer is @tt{058209749}.
}

