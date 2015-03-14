#lang racket
(provide while factorization limit-factors)


(define-syntax-rule (while condition body ...)
  (let loop ()
    (when condition
      body ...
      (loop))))


(define/contract (erathostenes limit)
  ((>=/c 2) . -> . (vectorof positive?))
  (define A (make-vector limit #t))
  (for ([i (in-range 2 (sqrt limit))])
    (when (vector-ref A i)
      (for ([j (in-range (sqr i) limit i)])
        (vector-set! A j #f))))
  (for/vector ([idx (in-range 2 limit)]
               #:when (vector-ref A idx))
    idx))


(define/contract (factorization n)
  ((>=/c 2) . -> . (vectorof positive?))
  (define primes (erathostenes n))
  (define factors #())
  (for ([trial (in-vector primes)])
    (while (= 0 (modulo n trial))
      (set! factors (vector-append factors (make-vector 1 trial)))
      (set! n (/ n trial))))
  factors)


(define/contract (limit-factors limit factors)
  (natural-number/c (vectorof positive?) . -> . (vectorof positive?))
  (vector-filter (lambda (x) (<= x limit)) factors))


(module+ test
  (require rackunit)

  (test-case
    "Erathostenes sieve"
    (check-equal? (erathostenes 3) #(2))
    (check-equal? (erathostenes 10) #(2 3 5 7))
    (check-equal? (vector-take-right (erathostenes 1000) 4) #(977 983 991 997))
    (check-exn exn:fail:contract? (lambda () (erathostenes -1)))
    (check-exn exn:fail:contract? (lambda () (erathostenes 0)))
    (check-exn exn:fail:contract? (lambda () (erathostenes 1))))

  (test-case
    "Prime factorization by trial division"
    (check-equal? (factorization 21) #(3 7))
    (check-equal? (factorization 25) #(5 5))
    (check-exn exn:fail:contract? (lambda () (factorization -1)))
    (check-exn exn:fail:contract? (lambda () (factorization 0)))
    (check-exn exn:fail:contract? (lambda () (factorization 1))))

  (test-case
    "Limit factors to less or equal to a number"
    (check-equal? (limit-factors 5 (factorization 21)) #(3))
    (check-exn exn:fail:contract? (lambda () (limit-factors 5.0 #(3 7))))
    (check-exn exn:fail:contract? (lambda () (limit-factors 5 '(3 7))))
    (check-exn exn:fail:contract? (lambda () (limit-factors 5 #(-1 1))))))
