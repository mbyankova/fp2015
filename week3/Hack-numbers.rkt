#lang racket
(require "Binary.rkt")


(define (next-hack n)
  (define (next-hack-iter i)
    (if
     (and (odd-sum? (string->number(to-binary-string i))) (equal? (to-binary-string i) (string-reverse (to-binary-string i))))
     i
     (next-hack-iter (+ i 1))))
  (next-hack-iter (+ n 1)))

(define (odd-sum? n)
  (define (sum-iter n sum)
    (if (= n 0)
     sum
     (sum-iter (quotient n 10) (+ sum (remainder n 10)))))
  (odd? (sum-iter n 0)))
     
  