#lang racket
(define (fst pair)
  (car pair))

(define (snd pair)
  (cdr pair))

(define (add-frac frac1 frac2)
  (cons (+ (* (fst frac1) (snd frac2)) (* (fst frac2) (snd frac1))) (* (snd frac1) (snd frac2))))

(define (substract-frac frac1 frac2)
  (cons (- (* (fst frac1) (snd frac2)) (* (fst frac2) (snd frac1))) (* (snd frac1) (snd frac2))))

(define (mult-frac frac1 frac2)
  (cons (* (fst frac1) (fst frac2)) (* (snd frac1) (snd frac2))))

(define (greatest-common-divisor u v)
  (define (position-gcd u v)
    (if (> u v)
        (bas-gcd u v)
        (bas-gcd v u)))
  (define (bas-gcd u v)
    (if (= v 0)
        u
        (bas-gcd v (remainder u v))))
  (cond
    [(= u 0) (abs v)]
    [(= v 0) (abs u)]
    [else (position-gcd (abs u) (abs v))]))

(define (simplify-frac frac)
  (cons (/ (fst frac) (greatest-common-divisor (fst frac) (snd frac))) (/ (snd frac) (greatest-common-divisor (fst frac) (snd frac)))))
