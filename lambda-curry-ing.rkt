#lang racket

(define gt-c
  (lambda (a)
    (lambda (x)
      (> x a))))
  
;Nice! Haskell B. Curry 
(findf (gt-c 9)
       '(7 8 9 10 11))


; new & old :any/c
; lst  list of any/c
; seq : lambda (new old list)
(define insert-g
  (lambda (seq)
    (lambda (new old lst)
      (cond
        ((null? lst) '())
        ((eq? (car lst) old)
         (seq new old (cdr lst)))
        (else  (cons (car lst)
                     ((insert-g seq) new old
                                      (cdr lst))))))))



(define seqL
  (lambda (new old lst)
    (cons new (cons old lst))))
(define  insertL (insert-g seqL))

(define  seqR
  (lambda (new old lst)
    (cons old (cons new lst))))

(define  insertR (insert-g seqR))

(define  seqS
  (lambda (new old lst)
    (cons new lst)))

(define subst (insert-g seqS))

; Replace all
(define seqS-multi
  (lambda (new old lst)
    (cons new ((insert-g seqS-multi) new old lst))))

(define subst-multi
  (insert-g seqS-multi))

(define seqrem
  (lambda (new old lst)
    lst))

(define rember
  (lambda (a lst)
    ((insert-g seqrem)  #f a lst)))

(define seqrem-multi
  (lambda (new old lst)
   ((insert-g seqrem-multi) new old lst)))

(define rember-multi
  (lambda (a lst)
    ((insert-g seqrem-multi)  #f a lst)))

; test  code
(insertL 2 1 '(1 3 1))
(insertR 2 1 '(1 3 1))
(subst 2 1 '(1 3 1))
(rember 1 '(1 3 1))

(display "multi edition\n")
;; All editionx
(subst-multi 2 1 '(1 3 1))
(rember-multi 1 '(1 3 1))

(define aa
  (lambda (x y z)
    (list x (list x y (list x y z)))
    ))


; 验证停机问题？
(define eternity
  (lambda (x)
    (eternity x)))

(define last-try
  (lambda (x)
    (and (will-stop? last-try)
         (eternity x))))

(define will-stop?
  (lambda (f)
    (f #f))) ; (f #t) will also never stop the call

(will-stop? last-try)

