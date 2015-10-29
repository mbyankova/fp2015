#lang racket

(define (triangle? a b c)
  (and
    (> (+ b c) a)
    (> (+ a b) c)
    (> (+ a c) b)))

(define (area a b c)
  (define s (/ (+ a b c) 2))
  (cond
    [(triangle? a b c) (sqrt (* s (- s a) (- s b) (- s c)))]
    [else "These sides do not form triangle"]))
