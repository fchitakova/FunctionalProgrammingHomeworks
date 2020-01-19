#lang racket
(require rackunit rackunit/text-ui)
; Искаме да дефинираме следните имена: one, two, three, ..., nine, plus, minus, times, div,
; така че извиквания от типа на (one (plus (three))) (операция с точно две операнди) да връщат легитимни числови стойности (в този случай - 4)
; Още малко примери:
; (three (times (five))) -> 15
; (nine (div (three))) -> 3
; (eight (minus (four))) -> 4

;(define (make-pairs f keys)
 ; (map (lambda (x) (cons x (f x))) keys))

(define (1+ x) (+ x 1))


(define digit-words (list 'one 'two 'three 'four 'five 'six 'seven  'eight 'nine))
(define operations-words (list 'plus 'minus 'times 'div))
(define operations (list + - * /))

(define key-digit-pairs
  (let ((i 0))
    (map (lambda (x) (begin (set! i (1+ i)) (cons x  i))) digit-words)))

(define key-operation-pairs
  (let ((i -1))
        (map (lambda (x) (begin (set! i (1+ i))) (cons x (list-ref operations i))) operations-words)))

(define (get-value key pairList)
  (let ((res (assoc key pairList)))
    (if (equal? res #f) #f
        (cdr res))))

(define (get-digit key) (get-value key key-digit-pairs))
(define (get-op key) (get-value key key-operation-pairs))
       

(define (isOperation? x)
  (not (equal? #f (get-op x))))

(define (isNumber? x)
  (not (equal? #f (get-digit x))))

;Stack Operations

(define (isEmpty stack) (null? stack))
(define (top* stack) (if (isEmpty stack) #f (car stack)))
(define (pop* stack)
  (let ((stack (cdr stack)))
    stack))

(define (push* stack args)
  (let* ((args (reverse args))
        (stack (append args stack)))
    stack))



(define (resolve-exp expression op-stack digit-stack )
  (if (= (length expression) 1)
      (make-arithmetics op-stack (push* digit-stack (list (get-digit (car expression)))))
      (cond ((isNumber? (car expression))
             (resolve-exp (cadr expression) op-stack (push* digit-stack (list (get-digit (car expression))))))
            ((isOperation? (car expression))
             (resolve-exp (cadr expression) (push* op-stack (list (get-op (car expression)))) digit-stack )))))
  
          
(define (make-arithmetics op-stack digit-stack)
  (if (and (> (length digit-stack) 1) (> (length op-stack) 0))
       (let ((newVal ((top* op-stack) (cadr digit-stack) (top* digit-stack)))
             (newOpStack (pop* op-stack))
             (newValues (pop* (pop* digit-stack))))
         (make-arithmetics newOpStack (push* newValues (list newVal))))
       (top* digit-stack)))

;TESTS

(define stack-tests
  (test-suite "Test stack functions"
              (test-case "Test getting top of empty stack"
                         (check-false (top* '())))
              (test-case "Test getting top of non-empty stack"
                         (check-equal? 'd (top* '(d b a c))))
              (test-case "Test pushing into stack"
                         (check-equal? (push* '(a v c d) '(k m)) '(m k a v c d)))))

(define pairs-construction-tests
(test-suite "Test construction of initial data"
            (test-case "Test construction of operations"
                       (check-false (null? (member (cons 'div /) key-operation-pairs))))
            (test-case "Test the size of digit pairs"
                       (check-equal? (length key-digit-pairs) 9))))

(define helper-operations-tests
  (test-suite "Test helper operations"
       (test-case "Get value by key in assosiative list-Positive test"
              (check-equal? (get-value 'one key-digit-pairs) 1))
       (test-case "Get value by key in assosiative list-Negative test"
               (check-not-equal? (get-value 'abc key-digit-pairs) 8))
       (test-case "Get right digit value"
                  (check-equal? (get-digit 'three) 3))
       (test-case "Get right operation value"
                  (check-equal? (get-op 'plus) +))))

(define helper-predicates-tests
  (test-suite "Test predicates"
              (test-case "Check if element is an operation-Positive test"
                  (check-true (isOperation? 'div)))
              (test-case "Check if element is a number-Negative test"
                   (check-false (isNumber? 'div)))
              (test-case "Check if element is a number-Positive test"
                    (check-true (isNumber? 'three)))))

(define arithmetics-tests
  (test-suite "Test arithmetics"
              (test-case "Should return right value-test1"
                  (check-eq? (make-arithmetics (list / *) (list 2 8 3)) 12))
              (test-case "Should return right value-test2"
                  (check-eq? (make-arithmetics (list - * + +) (list 3 5 4 3 1)) 12))
              (test-case "Should parse expression and return right value-test1"
                  (check-eq? (resolve-exp '(one (plus (three (minus (two))))) '() '()) 2))
              (test-case "Should parse expression and return right value-test2"
                   (check-eq? (resolve-exp '(one (plus (three (times (eight (div (four (minus (one))))))))) '() '()) 9))))
                             
                 

(run-tests pairs-construction-tests)
(run-tests helper-operations-tests)
(run-tests helper-predicates-tests)
(run-tests stack-tests)
(run-tests arithmetics-tests)


(define (readAndEvaluate)
  (begin (let ((expression (read))) (display (resolve-exp expression '() '())) (readAndEvaluate))))
  (readAndEvaluate)


       
