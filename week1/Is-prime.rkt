#lang racket

(define (prime-recursive? number divisor)
  (cond
    [(= divisor 1) #t]
    [(= (remainder number divisor)0) #f]
    [else (prime-recursive? number (- divisor 1))]))

(define (prime? n)
  (cond
    [(<= n 1) #f]
    [else (prime-recursive? n (- n 1))]))