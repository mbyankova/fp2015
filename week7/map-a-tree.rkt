#lang racket
(define (atom? x)
  (not (or
        (pair? x)
        (null? x)
        (vector? x))))

(define (make-tree node left right)
  (list node left right))

(define (make-leaf node)
  (make-tree node '() '()))

(define (empty-tree? tree)
  (null? tree))

(define (root tree)
  (first tree))

(define (left tree)
  (first (rest tree)))

(define (right tree)
  (first (rest (rest tree))))

(define t
  (make-tree 1
    (make-tree 2
      (make-leaf 5)
      (make-leaf 6))
    (make-leaf 3)))

(define (tree-map f tree)
  (cond
    [(empty-tree? tree) (list)]
    [(atom? (first tree)) (cons (f (first tree)) (tree-map f (rest tree)))]
    [else (cons (tree-map f (first tree)) (tree-map f (rest tree)))]))