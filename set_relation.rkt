#lang racket

(define  set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member (car lat) (cdr lat)) #f)
      (else  (set? (cdr lat))))))

;Binary heaps are a simple implementation of priority queues.
; (require data/heap) 

;(list->set lst) → set?