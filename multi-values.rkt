#lang racket
(define (foo1)
  (values 1 2 3))

(let-values (((a b c) (foo1)))
  (display (list a b c))
  (newline))

