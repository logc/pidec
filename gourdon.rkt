#lang racket
(require math/base math/number-theory)

(define (choose-n_0 n)
  (exact-floor (/ n 4)))

(define (choose-N n M)
  (let ([n_0 (choose-n_0 n)])
    (exact-ceiling (* (+ n n_0 1) (/ (log 10) (log (* 2 euler.0 M)))))))

(module+ test
  (require rackunit)

  (test-case
    "Choosing N"
    (let ([n 4]
          [M 4])
      (check-= (choose-N n M) 5 1e-12))
    (let ([n 8]
          [M 4])
      (check-= (choose-N n M) 9 1e-12))
    (let ([n 10]
          [M 4]
          )
      (check-= (choose-N n M) 10 1e-12))))

(define/contract (compute-B n M)
  (natural-number/c (and/c (>=/c 4) natural-number/c) . -> . real?)
  (let ([n_0 (choose-n_0 n)]
        [N (choose-N n M)])
    (for/sum ([k (in-range (* (add1 M) (sub1 N)))])
      (* (expt -1 k) (/ (with-modulus (* 2 (+ k 1)) (mod* 4 (modexpt 10 n))) (* 2 (+ k 1)))))))

(module+ test
  (test-case
    "Computing B against values obtained independently"
    (let ([n 4] [M 4])
      (check-= (compute-B n M) 1.4280635085588647 1e-10))
    (let ([n 5] [M 5])
      (check-= (compute-B n M) 2.499908565731691 1e-10))
    (let ([n 6] [M 3])
      (check-exn exn:fail:contract? (lambda () (compute-B n M))))))

(define (compute-C n M)
  (let ([N (choose-N n M)])
    (for/sum ([k (in-range (sub1 N))])
      (* (expt -1 k)
         (/
           (with-modulus (+ (* 2 M N) (* 2 k) 1) 
                         (mod* (modexpt 5 (mod- N 2))
                               (modexpt 10 (mod+ (mod- n  N) 2))
                               (s k M N)))
           (+ (* 2 M N) (* 2 k) 1))))))

(module+ test
  (test-case
    "Computing C against values obtained independently"
    (check-= (compute-C 4 4) 2.06498433118918 1e-10)
    (check-= (compute-C 5 5) 1.0153772080286478 1e-10)
    (check-= (compute-C 5 4) 1.185836400101252 1e-10)))

(define (s k M N)
  (for/sum ([j (in-range k)])
    (with-modulus (+ (* 2 M N) (* 2 k) 1) (binomial N j))))

(module+ test
  (test-case
    "Sum of binomials"
    (check-= (s 4 4 4) 15 1e-10)
    (check-= (s 5 4 5) 31 1e-10)
    (check-= (s 5 5 4) 16 1e-10)))

(define (fractional-part n)
  (let ([value (abs n)])
    (if (value . < . 1)
      value
      (- value (exact-floor value)))))


(module+ test
  (test-case
    "Fractional part of a number"
    (let ([epsilon 1e-12])
      (check-= (fractional-part 3.14) 0.14 epsilon)
      (check-= (fractional-part -1.400) 0.4 epsilon)
      (check-= (fractional-part 0.756) 0.756 epsilon)
      (check-= (fractional-part -0.332) 0.332 epsilon))))

(define (pi-dec n)
  (let* ([M (* 2 (exact-ceiling (/ n (expt (log n) 3))))]
         [N (choose-N n M)]
         [B (compute-B n M)]
         [C (compute-C n M)])
    (printf "M: ~a N: ~a B: ~a C: ~a ~n" M N B C)
    (fractional-part (- B C))))

(module+ test
  (test-case
    "Compute the n-th digit of π"
    (let* ([n 100] [epsilon (expt 10 (* -1 (choose-n_0 n)))])
      (check-= (pi-dec n) 0.82148086513282306647093844609550582231725359408128 ;(fractional-part ((expt 10 n) . * . pi))
               epsilon))
    )
  )
