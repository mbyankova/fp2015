#lang racket
(define (where list-elements list-predicates)
  (define (iter list-elements list-predicates2 result)
    (cond
      [(empty? list-elements) result]
      [(empty? list-predicates2) (iter (rest list-elements) list-predicates (append result (list (first list-elements))))] 
      [((first list-predicates2) (first list-elements)) (iter list-elements (rest list-predicates2) result)]
      [else (iter (rest list-elements) list-predicates result)]))
  (iter list-elements list-predicates (list)))