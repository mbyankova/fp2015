#lang racket
(define (row index m)
  (list-ref m index))

(define (col index m)
  (map (lambda (row)
         (list-ref row index))m))

(define (zip xs ys)
  (cond
    [(empty? xs) (list)]
    [(empty? ys) (list)]
    [else (cons (cons (first xs) (first ys))
                (zip (rest xs) (rest ys)))]))

(define (enumerate l)
  (zip (range 0 (length l)) l))

(define (matrix-diagonal m)
  (map (lambda (enum-row)
         (list-ref (cdr enum-row) (car enum-row) ))
       (enumerate m)))

(define (const-sum sum i)
  (- sum i))

(define (matrix-second-diagonal m)
  (map (lambda (enum-row)
         (list-ref (cdr enum-row) (const-sum (- (length m) 1) (car enum-row)) ))
       (enumerate m)))

(define m (list (list 23 28 21) (list 22 24 26) (list 27 20 25)))

(define (magic-square? M)
  (define (iter index len)
    (cond
      [(= index len) #t]
      [(= (apply + (row index M)) (apply + (col index M))) (iter (+ index 1) len)]
      [else #f]))
  (cond
    [(and (= (apply + (matrix-diagonal M)) (apply + (matrix-second-diagonal M))) (= (apply + (matrix-diagonal M)) (apply + (row 0 M))) (iter 0 (length M))) #t]
    [else #f]))
  