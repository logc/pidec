#lang racket

(define-syntax-rule (while condition body ...)
  (let loop ()
    (when condition
      body ...
      (loop))))

(define/contract (erathostenes limit)
  ((>=/c 2) . -> . (vectorof positive?))
  (when (limit . < . 0)
    #())
  (define A (make-vector limit #t))
  (for ([i (in-range 2 (sqrt limit))])
    (when (vector-ref A i)
      (for ([j (in-range (sqr i) limit i)])
        (vector-set! A j #f))))
  (for/vector ([idx (in-range 2 limit)]
               #:when (vector-ref A idx))
    idx))

(define (factorization n)
  (define primes (erathostenes n))
  (define factors #())
  (for ([trial (in-vector primes)])
    (while (= 0 (modulo n trial))
      (set! factors (vector-append factors (make-vector 1 trial)))
      (set! n (/ n trial))))
  factors)

(module+ test
  (require rackunit)
  (test-case
    "Erathostenes sieve"
    (check-equal? (erathostenes 3) #(2))
    (check-equal? (erathostenes 10) #(2 3 5 7))
    (check-exn exn:fail:contract? (lambda () (erathostenes -1)))
    (check-exn exn:fail:contract? (lambda () (erathostenes 0)))
    (check-exn exn:fail:contract? (lambda () (erathostenes 1))))
  (test-case
    "Prime factorization by trial division"
    (check-equal? (factorization 21) #(3 7))
    (check-equal? (factorization 25) #(5 5))))
