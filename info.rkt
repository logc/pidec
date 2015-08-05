#lang info
(define collection "pidec")
(define deps '("base"
               "rackunit-lib"
               "math-lib"
               "typed-racket-lib"
               "unstable-contract-lib"
               "while-loop"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/pidec.scrbl" ())))
(define pkg-desc "Computation of the n-th digit of π with low memory")
(define version "0.1")
(define pkg-authors '(logc))
