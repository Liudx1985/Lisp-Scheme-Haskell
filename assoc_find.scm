#lang racket

(define (find key lst)
  (call/cc (lambda (return)
    (for-each (lambda (x) (when (eq? (car x) key)
                            (return x)))
              lst)
             #f)))

(assoc 5 '((1 2) (3 4) (5 6)))
(assoc 15 '((1 2) (3 4) (5 6)))
(find 5 '((1 2) (3 4) (5 6)))
(find 15 '((1 2) (3 4) (5 6)))