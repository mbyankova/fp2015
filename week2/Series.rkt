#lang racket

(define (series a b n) 
  (define (series-iter a b i)
    (if (> i n) b
        (series-iter b (+ a b) (+ i 1))))
  (if (= n 1) a
    (series-iter a b 3)))

(define (lucas n)
  (series 2 1 n))

(define (fibonacci n)
  (series 1 1 n))

(define (summed-member n)
  (+ (lucas n) (fibonacci n)))

(define (nth-lucas-sum n)
  (define (lucas-sum-iter i result)
    (if (> i n) result
        (lucas-sum-iter (+ i 1) (+ result (lucas i)))))
  (lucas-sum-iter 1 0))

(define (nth-fibonacci-sum n)
  (define(fibonacci-sum-iter i result)
    (if (> i n) result
        (fibonacci-sum-iter (+ i 1) (+ result (fibonacci i)))))
  (fibonacci-sum-iter 1 0))

(define (lucas-fib-diff n)
  (series (- (lucas 1) (fibonacci 1)) (- (lucas 2) (fibonacci 2)) n))