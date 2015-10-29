#lang racket
(define (f p h g)
  (lambda (x)
    (and (p (h x)) (p (g x)))))