#lang racket
(require math/number-theory)
(require racket/cmdline)

(require (planet dyoo/while-loop))

(define (update-while-decreasing t v prime)
  (let do-loop ()
    (set! t (quotient t prime))
    (set! v (sub1 v))
    (when (= (modulo t prime) 0) (do-loop)))
  (values t v))

(define (update-while-increasing t v prime)
  (let do-loop ()
    (set! t (quotient t prime))
    (set! v (add1 v))
    (when (= (modulo t prime) 0) (do-loop)))
  (values t v))

(define (main n)
  (define N (exact-floor (* (+ n 20) (/ (log 10) (log 2)))))
  (define (primes-between start end)
    (filter prime? (range start end)))
  (define sum
    (for/sum ([prime (primes-between 3 (* 2 N))])
      (define vmax (exact-floor (/ (log (* 2 N)) (log prime)))) 
      (define av (expt prime vmax))
      (define s 0)
      (define num 1)
      (define den 1)
      (define v 0)
      (define kq 1)
      (define kq2 1)
      (define t 0)
      (for ([k (in-range 1 (add1 N))])
        (set! t k)
        (when (kq . >= . prime)
          (set!-values (t v) (update-while-decreasing t v prime))
          (set! kq 0))
        (set! kq (add1 kq))
        (set! num (with-modulus av (mod* num t)))
        (set! t (- (* 2 k) 1))
        (when (kq2 . >= . prime)
          (when (= kq2 prime)
            (set!-values (t v) (update-while-increasing t v prime)))
          (set! kq2 (- kq2 prime)))
        (set! den (with-modulus av (mod* den t)))
        (set! kq2 (+ kq2 2))
        (when (v . > . 0)
          (set! t (with-modulus av (mod* (mod/ num den) k)))
          (for ([i (in-range v vmax)])
            (set! t (with-modulus av (mod* t prime))))
          (set! s (+ s t))
          (when (s . >= . av)
            (set! s (- s av)))))
      (set! t (modular-expt 10 (- n 1) av))
      (set! s (with-modulus av (mod* s t)))
      (/ s av)))
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
