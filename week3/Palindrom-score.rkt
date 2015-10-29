#lang racket
(define (p-score n)
  (cond
    [(palindrom? n) 1]
    [else (+ 1 (p-score (+ n (reverse-number n))))]))

(define (reverse-number n)
  (define (reverse-iter n result)
    (cond
      [(= n 0) result]
      [else (reverse-iter (quotient n 10) (+ (remainder n 10) (* result 10)))]))
  (reverse-iter n 0))

(define (palindrom? n)
  (= n (reverse-number n)))