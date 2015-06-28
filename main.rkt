#lang typed/racket
(require math/number-theory)
(require racket/cmdline)
(require racket/unsafe/ops)

(require (planet dyoo/while-loop))

(provide main fmod)

(define: (update-while-decreasing
           [term : Fixnum] [v : Fixnum] [prime : Fixnum]) : (values Fixnum Fixnum)
  (let do-loop ()
    (set! term (unsafe-fxquotient term prime))
    (set! v (unsafe-fx- v 1))
    (when (= (modulo term prime) 0) (do-loop)))
  (values term v))

(define: (update-while-increasing
           [term : Fixnum] [v : Fixnum] [prime : Fixnum]) : (values Fixnum Fixnum)
  (let do-loop ()
    (set! term (unsafe-fxquotient term prime))
    (set! v (unsafe-fx+ v 1))
    (when (= (modulo term prime) 0) (do-loop)))
  (values term v))

(define: (fmod [num : Float] [mod : Float]) : Float
  (- num (* (truncate (/ num mod)) mod)))

(define: (main [n : Integer]) : Integer
  (define: 𝛆 : Positive-Integer 20)
  (define: base : Positive-Integer 10)
  (define: N : Integer (* (+ n 𝛆) (exact-floor (/ (log (exact->inexact base))
                                                       (log 2.0)))))
  (define: (primes-between
             [start : Integer] [end : Integer]) : (Listof Integer)
    (filter prime? (range start end)))
  (define: sum : Float
    (for/fold: : Float
               ([total : Float 0.0])
               ([prime : Integer (in-list (primes-between 3 (* 2 N)))])
      (define: vmax : Integer (exact-floor
                                (cast (/ (log (* 2 N)) (log prime)) Real)))
      (define: max-modulo : Positive-Integer (cast (expt prime vmax) Positive-Integer))
      (define: s : Integer 0)
      (define: num : Integer 1)
      (define: den : Integer 1)
      (define: v : Fixnum 0)
      (define: kq : Integer 1)
      (define: kq2 : Integer 1)
      (define: term : Fixnum 0)
      (for ([k : Integer (in-range 1 (add1 N))])
        (set! term (cast k Fixnum))
        (when (kq . >= . prime)
          (set!-values (term v) (update-while-decreasing term v (cast prime
                                                                      Fixnum)))
          (set! kq 0))
        (set! kq (add1 kq))
        (set! num (with-modulus max-modulo (mod* num term)))
        (set! term (cast (- (* 2 k) 1) Fixnum))
        (when (kq2 . >= . prime)
          (when (= kq2 prime)
            (set!-values (term v) (update-while-increasing term v (cast prime
                                                                        Fixnum))))
          (set! kq2 (- kq2 prime)))
        (set! den (with-modulus max-modulo (mod* den term)))
        (set! kq2 (+ kq2 2))
        (when (v . > . 0)
          (set! term (cast (with-modulus max-modulo (mod* (mod/ num den) k))
                           Fixnum))
          (for ([i (in-range v vmax)])
            (set! term (cast (with-modulus max-modulo (mod* term prime)) Fixnum)))
          (set! s (+ s term))
          (when (s . >= . max-modulo)
            (set! s (- s max-modulo)))))
      (set! term (cast (modular-expt 10 (- n 1) max-modulo) Fixnum))
      (set! s (with-modulus max-modulo (mod* s term)))
      (+ total (/ (exact->inexact s) (exact->inexact max-modulo)))))
  (exact-floor (* (fmod sum 1.0) 1e9)))


(module+ main

  (define: position : Integer
    (cast (string->number
      (vector-ref (current-command-line-arguments) 0)) Integer))

  (displayln
    (string-append
      "Decimal digits of pi at position "
      (~a position)
      ": "
      (~a (main position) #:width 9 #:align 'right #:pad-string "0"))))
