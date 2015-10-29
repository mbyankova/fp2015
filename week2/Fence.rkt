#lang racket
(define (string-repeat str n)
  (define (repeat-iter result i)
    (if (> i n)
        result
        (repeat-iter (string-append result str) (+ i 1))))
  (repeat-iter "" 1))

(define (fence n)
  (string-append "{" (string-append (string-repeat "-" (round (+ 1 (log n)))) (string-append ">"
  (string-append (number->string n) (string-append "<" (string-append (string-repeat "-" (round (+ 1 (log n)))) "}")))))))