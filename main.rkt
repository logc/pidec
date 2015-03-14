#lang racket


(require "primes.rkt")

(module+ test
  (require math)
  (require rackunit))


(define/contract (exact-division prime a)
  (natural-number/c natural-number/c . -> . natural-number/c)
  (define power 1)
  (while (((modulo a power) . = . 0)
          . and .
          (integer? (a . / . (power . * . prime))))
    (set! power (power . * . prime)))
  power)


(module+ test
  (test-case
    "Exact power division"
    (check-equal? (exact-division 5 9) 1)
    (check-equal? (exact-division 5 50) 25)
    (check-equal? (exact-division 3 21) 3)
    (check-equal? (exact-division 2 60) 4)
    (check-equal? (exact-division 3 60) 3)
    (check-equal? (exact-division 5 60) 5)
    (check-exn exn:fail:contract? (lambda () (exact-division -1 21)))
    (check-exn exn:fail:contract? (lambda () (exact-division 3.0 21)))))


(define/contract (decompose-powers a factors)
  (natural-number/c (vectorof positive?) . -> . (vectorof positive?))
  (define exact-divisors
    (for/vector ([factor (in-vector factors)])
        (exact-division factor a)))
  (define remaining
    (a . / . (product exact-divisors)))
  (if (integer? remaining)
    (vector-append (make-vector 1 remaining) exact-divisors)
    (vector-append (make-vector 1 1) exact-divisors)))


(module+ test
  (test-case
    "Decompose powers"
    (let* ([a 60] [k 4] [m 30]
           [raw-factors (factorization m)]
           [factors (limit-factors k raw-factors)])
      (check-equal? raw-factors #(2 3 5))
      (check-equal? factors #(2 3))
      (check-equal? (decompose-powers a factors) #(5 4 3)))
    (let* ([a 60] [k 6] [m 30]
           [raw-factors (factorization m)]
           [factors (limit-factors k raw-factors)])
      (check-equal? raw-factors #(2 3 5))
      (check-equal? factors raw-factors)
      (check-equal? (decompose-powers a factors) #(1 4 3 5)))
    (let* ([a 6] [k 6] [m 30]
           [factors (limit-factors k (factorization m))])
      (check-equal? (decompose-powers a factors) #(1 2 3 1)))
    (check-exn exn:fail:contract?
      (lambda () (decompose-powers -1 (factorization 21))))
    (check-exn exn:fail:contract?
      (lambda () (decompose-powers 60.0 (factorization 21))))
    (check-exn exn:fail:contract?
      (lambda () (decompose-powers 60 #(-2 3 5))))))


(define (product sequence)
  (for/product ([elem sequence]) elem))


(define (_ vec idx)
  (vector-ref vec idx))


(define/contract (sum-binomials-modulo k N m)
  (natural-number/c natural-number/c positive? . -> . natural-number/c)
  (let* ([factors (limit-factors k (factorization m))]
         [l (vector-length factors)]
         [A 1] [B 1] [C 1]
         [R (make-vector (vector-length factors) 1)]
         [a 0] [b 0])
    (for* ([j (in-range 1 k)]
           [i (in-range l)])
      (set! a (+ (- N j) 1)) ; N written 'n' in paper
      (set! b j)
      (define a-powers (decompose-powers a factors))
      (define b-powers (decompose-powers b factors))
      (define a* (vector-ref a-powers 0))
      (define b* (vector-ref b-powers 0))
      (vector-set! R i
        ((R . _ . i) . * .  ((a-powers . _ . i) . / . (b-powers . _ . i))))
      (set! A (modulo (* A a*) m))
      (set! B (modulo (* B b*) m))
      (set! C (+ (* C (vector-ref b-powers 0)) (modulo (* A (product R)) m))))
    (/ C (modulo B m))))


(module+ test
  (test-case
    "Sum of binomials modulo an integer"
    (define epsilon 0.01)
    (define (slow-sum-binomials-modulo k N m)
      (for/sum ([i (in-range (add1 k))]) (modulo (binomial N i) m)))
    (let* ([k 6] [N 6] [m 30]
           [S (slow-sum-binomials-modulo k N m)])
      (check-= (sum-binomials-modulo k N m) S epsilon))
    (let* ([k 6] [N 6] [m 30]
           [S (slow-sum-binomials-modulo k N m)])
      (check-= (sum-binomials-modulo k N m) S epsilon))
    (check-exn exn:fail:contract? (lambda () (sum-binomials-modulo -6 6 30)))
    (check-exn exn:fail:contract? (lambda () (sum-binomials-modulo 6 -6 30)))
    (check-exn exn:fail:contract? (lambda () (sum-binomials-modulo 6 6 -30)))
    (check-exn exn:fail:contract? (lambda () (sum-binomials-modulo 6.0 6 30)))
    (check-exn exn:fail:contract? (lambda () (sum-binomials-modulo 6 6.0 30)))
    (check-exn exn:fail:contract? (lambda () (sum-binomials-modulo 6 6 30.0)))))
