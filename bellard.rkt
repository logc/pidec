#lang racket
(require math/number-theory)
(require racket/cmdline)

(require (planet dyoo/while-loop))

(define (update-while-decreasing term v prime)
  (let do-loop ()
    (set! term (quotient term prime))
    (set! v (sub1 v))
    (when (= (modulo term prime) 0) (do-loop)))
  (values term v))

(define (update-while-increasing term v prime)
  (let do-loop ()
    (set! term (quotient term prime))
    (set! v (add1 v))
    (when (= (modulo term prime) 0) (do-loop)))
  (values term v))

(define (main n)
  (define 𝛆 20)
  (define base 10)
  (define N (exact-floor (* (+ n 𝛆) (/ (log base) (log 2)))))
  (define (primes-between start end)
    (filter prime? (range start end)))
  (define sum
    (for/sum ([prime (primes-between 3 (* 2 N))])
      (define vmax (exact-floor (/ (log (* 2 N)) (log prime))))
      (define max-modulo (expt prime vmax))
      (define s 0)
      (define num 1)
      (define den 1)
      (define v 0)
      (define kq 1)
      (define kq2 1)
      (define term 0)
      (for ([k (in-range 1 (add1 N))])
        (set! term k)
        (when (kq . >= . prime)
          (set!-values (term v) (update-while-decreasing term v prime))
          (set! kq 0))
        (set! kq (add1 kq))
        (set! num (with-modulus max-modulo (mod* num term)))
        (set! term (- (* 2 k) 1))
        (when (kq2 . >= . prime)
          (when (= kq2 prime)
            (set!-values (term v) (update-while-increasing term v prime)))
          (set! kq2 (- kq2 prime)))
        (set! den (with-modulus max-modulo (mod* den term)))
        (set! kq2 (+ kq2 2))
        (when (v . > . 0)
          (set! term (with-modulus max-modulo (mod* (mod/ num den) k)))
          (for ([i (in-range v vmax)])
            (set! term (with-modulus max-modulo (mod* term prime))))
          (set! s (+ s term))
          (when (s . >= . max-modulo)
            (set! s (- s max-modulo)))))
      (set! term (modular-expt 10 (- n 1) max-modulo))
      (set! s (with-modulus max-modulo (mod* s term)))
      (/ s max-modulo)))
  (exact-floor (* (fmod sum 1.0) 1e9)))

(module+ test
  (require rackunit)
  (test-case
    "Pi approximation"
    (check-= (main 50) 58209749 1e-12)
    (check-= (main 23) 433832795 1e-12)
    (check-= (main 107) 865132823 1e-12)
    (check-= (main 403) 57270365 1e-12)))

(define (fmod num mod)
  (- num (* (truncate (/ num mod)) mod)))

(module+ test
  (test-case
    "Floating-point modulo"
    (check-= (fmod 2.5 2) 0.5 1e-12)
    (check-= (fmod 2 1.0) 0.0 1e-12)
    (check-= (fmod 0.9 1.0) 0.9 1e-12)))

(module+ main
  (define position (string->number
                     (vector-ref (current-command-line-arguments) 0)))
  (displayln
    (string-append "Decimal digits of pi at position "
                   (~a position)
                   ": "
                   (~a (main position) #:width 9 #:align 'right #:pad-string "0"))))
