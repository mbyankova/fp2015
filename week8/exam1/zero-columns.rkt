#lang racket
(define (atom? x)
  (not (or
        (pair? x)
        (null? x)
        (vector? x))))

(define (find-zeros matrix)
  (define (iter matrix column i)
    (cond
      [(empty? matrix) column]
      [(not (atom? (first matrix))) (append (iter (first matrix) column i) (iter (rest matrix) column i))]
      [(zero? (first matrix)) (iter (rest matrix) (append  (list column)  (list i)) (+ i 1))]
      [else (iter (rest matrix) column (+ i 1))]))
  (iter matrix (list) 1))

(define (zero matrix)
  (define (iter matrix i lst result)
    (cond
      [(empty? matrix) result]
      [(and (atom? (first matrix)) (member i lst)) (iter (rest matrix) (+ i 1) lst (append result (list 0)))]
      [(atom? (first matrix)) (iter (rest matrix) (+ i 1) lst (append  result (list (first matrix))))]
      [ else (cons (iter (first matrix) 1 lst result) (iter (rest matrix) 1 lst result))]))
  (iter matrix 1 (find-zeros matrix) (list)))