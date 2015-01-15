#lang racket
; stack defined
(define make-stack
  (lambda ()
    (let ((st '()))
      (lambda (process arg)
        (case process
          ((push!) (begin
                     (set! st (cons arg st))
                     st))
          ((pop!)  (let ((temp (car st)))
                     (set! st (cdr st))
                     temp))
          ((view)  (display st))
          ((empty?) (null? st))
          (else "error!"))))))

;(define s (make-stack))
;(display (s 'push! 9)) (newline)
;(display (s 'push! 8)) (newline)
;(display (s 'push! 7)) (newline)
;(display (s 'pop! 0)) (newline)
;(s 'view 0) (newline)
(define *queue* '())
 
(define (empty-queue?)
  (null? *queue*))

(define (enqueue x)
  (set! *queue* (append *queue* (list x))))

(define (dequeue)
  (let ((x (car *queue*)))
    (set! *queue* (cdr *queue*))
    x))