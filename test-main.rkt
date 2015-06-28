#lang racket
(require rackunit)

(require "main.rkt")

(test-case
    "Pi approximation"
    (check-= (main 50) 58209749 1e-12)
    (check-= (main 23) 433832795 1e-12)
    (check-= (main 107) 865132823 1e-12)
    (check-= (main 403) 57270365 1e-12))

(test-case
    "Floating-point modulo"
    (check-= (fmod 2.5 2.0) 0.5 1e-12)
    (check-= (fmod 2.0 1.0) 0.0 1e-12)
    (check-= (fmod 0.9 1.0) 0.9 1e-12))
