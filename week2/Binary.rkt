#lang racket
(define (string-reverse str)
  (define (string-iter i result)
    (if (< i 0)
        result
        (string-iter (- i 1) (string-append result (~a (string-ref str i))))))
   (string-iter (- (string-length str) 1) ""))

(define (to-binary-string n)
  (define (to-binary-iter result n)
    (if (= n 0)
        result
        (to-binary-iter (string-append result (~a (remainder n 2))) (quotient n 2))))
    (string-reverse (to-binary-iter "" n)))

(define (from-binary-string binary-str)
  (define (from-binary-iter i result n)
    (if (= n 0)
        result
        (from-binary-iter (+ 1 i) (+ result (* (remainder n 10) (expt 2 i))) (quotient n 10))))
    (from-binary-iter 0 0 (string->number binary-str)))
     