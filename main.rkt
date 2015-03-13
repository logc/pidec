#lang racket

(define x 1)

(module+ test
  (require rackunit)
  (check-eq? x 2)
)
