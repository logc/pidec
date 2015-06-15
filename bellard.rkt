#lang racket
(require math/number-theory)
(require racket/cmdline)

(require (planet dyoo/while-loop))

(define (mul_mod a b m)
  (with-modulus m (mod* a b)))

(define (main n)
  (define av 0)
  (define a 0)
  (define vmax 0)
  (define N 0)
  (define num 0)
  (define den 0)
  (define k 0)
  (define kq 0)
  (define kq2 0)
  (define t 0)
  (define v 0)
  (define s 0)
  (define i 0)
  (define sum 0.0)
  (when (<= n 0)
    (error "Bad argument for n"))
  ; the exact-floor tries to mimic the C (int) cast
  (set! N (exact-floor (* (+ n 20) (/ (log 10) (log 2)))))
  (define (primes-between start end)
    (filter prime? (range start end)))
  (for ([a (primes-between 3 (* 2 N))])
    (set! vmax (exact-floor (/ (log (* 2 N)) (log a)))) 
    (set! av 1)
    (for ([i (in-range vmax)])
      (set! av (* av a)))
    (set! s 0)
    (set! num 1)
    (set! den 1)
    (set! v 0)
    (set! kq 1)
    (set! kq2 1)
    (for ([k (in-range 1 (add1 N))])
      (set! t k)
      (when (kq . >= . a)
        (let do-loop ()
          (set! t (quotient t a))
          (set! v (sub1 v))
          (when (eq? (modulo t a) 0) (do-loop)))
        (set! kq 0))
      (set! kq (add1 kq))
      (set! num (mul_mod num t av))
      (set! t (- (* 2 k) 1))
      (when (kq2 . >= . a)
        (when (eq? kq2 a)
          (let do-loop ()
            (set! t (quotient t a))
            (set! v (add1 v))
            (when (eq? (modulo t a) 0) (do-loop))))
        (set! kq2 (- kq2 a)))
      (set! den (mul_mod den t av))
      (set! kq2 (+ kq2 2))
      (when (v . > . 0)
        (set! t (modular-inverse den av))
        (set! t (mul_mod t num av))
        (set! t (mul_mod t k av))
        (for ([i (in-range v vmax)])
          (set! t (mul_mod t a av)))
        (set! s (+ s t))
        (when (s . >= . av)
          (set! s (- s av)))))
    (set! t (modular-expt 10 (- n 1) av))
    (set! s (mul_mod s t av))
    (set! sum (fmod (+ sum (/ s av)) 1.0)))
  (exact-floor (* sum 1e9)))

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
