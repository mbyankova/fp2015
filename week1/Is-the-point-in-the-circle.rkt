#lang racket

(define (square n)
  (* n n))

(define (circle? circle-x circle-y radius point-x point-y)
  (<= (+ (square (- point-x circle-x)) (square (- point-y circle-y))) (square radius)))