;;; simple function
;;; uses trace-lambda to show the nesting
(define sign
   (lambda (x)
      (cond
         [(> x 0) x]
         [(= x 0) 0]
         [else (- x)])))

(define sum_int
   (trace-lambda sum_int (x)
      (if (= x 0)
          0
          (+ x (sum_int (- x 1))))))

;tail-recursive fibonacci 
(define fib-iter 
	(trace-lambda fib-iter (a b n)
		(if (<= n 0)
			b
			(fib-iter b (+ a b) (- n 1)
				))))

(define (fib n)
	(fib-iter '1 '1 n))

;; Same as \map\ procedure 
(define fmap
  (lambda (f ls)
    (if (null? ls)
        '()
        (cons (f (car ls))
              (fmap f (cdr ls))))))

(define (sum a term next b)
  (define (iter it result)
    (if (> it b)
        result
        (iter (next it) (+ (term it) result))))
  (iter a 0))

 (require racket/trace)
 
 (define/match (fact n)
    [(0) 1]
    [(n) (* n (fact (sub1 n)))])
 
 
(trace fact) ; start to trace 
(fact 10)
(untrace fact); stop tract.
(fact 4)



#|
#lang lazy
;; An infinite list:
(define fibs
  (list* 1 1 (map + fibs (cdr fibs))))
|#
