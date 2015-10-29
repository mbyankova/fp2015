#lang racket

(define (cube-sums-recursion? n a b)
  (cond
    [(= n (+ (expt a 3) (expt b 3))) #t]
    [(< n (+ (expt a 3) (expt b 3))) #f]
    [(> n (expt (+ a 1) 3)) (cube-sums-recursion? n (+ a 1) b)]
    [else (cube-sums-recursion? n a (+ b 1))]))

(define (cube-sums? n)
  (cond
    [(< n 2) #f]
    [else (cube-sums-recursion? n 1 1)]))

(define (count-cube-sums from to)
  (define (count-iter i result) 
    (cond
      [(> i to) result]
      [(cube-sums? i) (count-iter (+ i 1) (+ result 1))]
      [else (count-iter (+ i 1)result)]))
  (count-iter from 0))