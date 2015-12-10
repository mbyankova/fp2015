#lang racket
(define (repeater str)
  (lambda (count glue)
     (define (iter count result)
       (cond
         [(zero? count) result]
         [else (iter (- count 1) (string-append str glue result))]))
    (iter count "")))

            
     
      