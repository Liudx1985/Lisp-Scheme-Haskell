#lang racket
;  applicative-order Y-combinator.
; Y: The function that takes a function f and returns f (f (f (f (···))))
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))


(define length
  (Y (lambda (length)
     (lambda (l)
       (cond ((null? l) 0)
             (else
              (add1 (length (cdr l)))))))))

(define fib
  (Y (lambda (fib)
       (lambda (n)
         (cond ((eq? 0 n) 1)
               ((eq? 1 n) 1)
               (else (+ (fib (- n 1)) (fib (- n 2)))))))))


