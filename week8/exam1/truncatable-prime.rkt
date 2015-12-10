#lang racket
(define (is-prime? n)
  (define (iter i)
    (cond
      [(>= i n) #t]
      [(zero? (remainder n i)) #f]
      [else (iter (+ i 1))]))
  (if (= n 1)
      #f
      (iter 2)))
       

(define (truncatable-prime? x)
  (cond
    [(zero? x) #t]
    [(not (is-prime? x)) #f]
    [else (truncatable-prime? (quotient x 10))]))