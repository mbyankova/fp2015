#lang racket
(provide
 group
 take-while
 drop-while)

(define (take-while p items)
  (cond
    [(empty? items) (list)]
    [(p (first items)) (cons (first items) (take-while p (rest items)))]
    [else (list)]))

(define (drop-while p items)
  (cond
    [(empty? items) (list)]
    [(p (first items)) (drop-while p (rest items))]
    [(not (p (first items)))  items]))

(define (group xs)
  (cond
    [(empty? xs) (list)]
    [else (cons (take-while (lambda (x) (equal? (first xs) x)) xs)
                (group (drop-while (lambda (x) (equal? x (first xs))) xs)))]))




  