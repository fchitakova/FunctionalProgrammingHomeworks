#lang racket

(define (find-each-prefix initialElems elements)
  (define (helper res)
    (if (null? elements) res
        (let ((currentResult (append initialElems (list (car elements)))))
        (cons currentResult (find-each-prefix currentResult (cdr elements))))))
  (helper '('())))
               
(find-each-prefix '() '(1 2 3 4))