#lang racket
(require (only-in  srfi/1
                  reverse!))

(require (only-in r5rs 
                  set-cdr!
                  interaction-environment))
;;;;;;;;;;;;;;
; amb defined pro
(define amb-fail '*)

(define initialize-amb-fail
  (lambda ()
    (set! amb-fail
      (lambda ()
        (error "amb tree exhausted")))))

(initialize-amb-fail)
(define-syntax amb
  (syntax-rules ()
    ((amb alt ...)
     (let ((prev-amb-fail amb-fail))
       (call/cc
        (lambda (sk)

          (call/cc
           (lambda (fk)
             (set! amb-fail
                   (lambda ()
                     (set! amb-fail prev-amb-fail)
                     (fk 'fail)))
             (sk alt))) ...
             
             (prev-amb-fail)))))))


(define number-between
  (lambda (lo hi)
    (let loop ((i lo))
      (if (> i hi) (amb)
          (amb i (loop (+ i 1)))))))

;我们可以用 assert 来插入一个断言。这样可以使程序的表达更加清晰明确。
(define assert
  (lambda (pred)
    (unless pred (amb))))


;当我们需要把 amb 作用于一个从别处返回的列表时,可以用这个宏。
(define-syntax apply-amb
  (syntax-rules ()
    ((apply-amb ls)
     (eval `(amb ,@ls) (interaction-environment)))))



;为了一次性得到所有结果,你 可以用 bag-of.
(define-syntax bag-of
  (syntax-rules ()
    ((bag-of e)
     (let ((prev-amb-fail amb-fail)
           (results '()))
       (when (call/cc
            (lambda (k)                                                
              (set! amb-fail (lambda () (k #f)))                ;<-----+
              (let ((v e))             ;amb-fail will be modified by e |
                (set! results (cons v results))                       ;|
                (k #t))))                                             ;|
           (amb-fail))                 ;so this amb-fail may not be ---+
       (set! amb-fail prev-amb-fail)
       (reverse! results)))))
;;用来判断一个list里的元素是不是没有重复。
(define (distinct? . ls)
  (let loop ((lst (car ls)))
    (let ((first (car lst)) (rest (cdr lst)))
      (cond 
       ((null? rest) #t)
       ((member first rest) #f)
       (else (loop rest))))))

;用来从一个list里删除一个元素。
(define (del n ls)
  (let ((ls (reverse (reverse ls))))
    (cond ((null? ls) ls)
          ((eqv? n (car ls)) (cdr ls))
          (else 
           (let loop ((l (cdr ls)) (last ls))
             (cond ((null? l) ls)
                   ((equal? n (car l))
                    (set-cdr! last (cdr l))
                    ls)
                   (else (loop (cdr l) l))))))))

;; generate prime number
(define (prime? n)
  (call/cc
   (lambda (return)
     (do ((i 2 (+ i 1)))
         ((> i (sqrt n)) #t)
       (when (= (modulo n i) 0) 
           (return #f))))))

(define gen-prime
  (lambda (hi)
    (let ((i (number-between 2 hi)))
      (assert (prime? i))
      i)))

(bag-of (gen-prime 60))

; n-queue
;(define (debug e) #f)
(define debug
  (lambda (e)
    (cond ((list? e)
           (for-each display e))
          ((string? e)
           (display e)))))
;|#

(define (n-queens n)
  (call/cc 
   (lambda (return)
     (let place-queens ((i 0) (rows '())) 
       (when (< i n)                    
         (let ((try-place (number-between 1 n))) ;start to place queen No.i
           (debug `("considering queen " ,i " on row " ,try-place "\n"))
           (do ((placed-idx 0 (+ 1 placed-idx))) ;ensure no two queens conflict
               ((>= placed-idx (length rows)))
             (debug `("checking queen on column " ,placed-idx))
             (let* ((r (list-ref rows placed-idx))
                    (condition (and (not (= r try-place))      
                                    (not (or           
                                          (= (+ placed-idx r) (+ i try-place))
                                          (= (- placed-idx r) (- i try-place)))))))
               (if condition 
                   (debug " ... OK!\n")
                   (debug " ... conflict!\n"))
               (assert condition)))
           (debug `("putting queen " ,i " on row " ,try-place "\n"))
           (debug `("places: " ,(append rows (list try-place)) "\n"))
           (place-queens (+ 1 i) (append rows (list try-place))))
         )
       (return rows)))))

;get all result of 8-queue prob
(n-queens 4)
(bag-of (n-queens 4))

; 24-point game.
(define (get-24 . numbers)
  (let* ((index '(0 1 2 3))
         (ai (apply-amb index))
         (bi (apply-amb index))
         (ci (apply-amb index))
         (di (apply-amb index)))
    (assert (distinct? (list ai bi ci di)))

    (let* ((a (list-ref numbers ai))
           (b (list-ref numbers bi))
           (c (list-ref numbers ci))
           (d (list-ref numbers di)))

      (let* ((ops '('+ '- '* '/))
             (op1s (apply-amb ops))
             (op1 (eval op1s (interaction-environment)))
             (op2s (apply-amb ops))
             (op2 (eval op2s (interaction-environment)))
             (op3s (apply-amb ops))
             (op3 (eval op3s (interaction-environment))))

;         (for-each display `(,a " " ,b " " ,c " " ,d " " 
;                                ,op1s " " ,op2s " " ,op3s "\n"))

        (let ((exp
               (amb 
                (when (not (or (and (eq? op2 /)
                                    (= (op3 c d) 0))
                               (and (eq? op1 /)
                                    (= (op2 b (op3 c d)) 0))
                               (and (memq op3 (list + * /))
                                    (< c d))
                               (and (memq op2 (list + * /))
                                    (< b (op3 c d)))
                               (and (memq op1 (list + * /))
                                    (< a (op2 (op3 c d))))))
                  `(,op1s ,a (,op2s ,b (,op3s ,c ,d))))

                (when (not (or (and (eq? op3 /)
                                    (= 0 b))
                               (and (eq? op2 /)
                                    (= 0 c))
                               (and (eq? op1 /)
                                    (= 0 d))
                               (and (memq op3 (list + * /))
                                    (< a b))
                               (and (memq op2 (list + * /))
                                    (< (op3 a b) c))
                               (and (memq op1 (list + * /))
                                    (< (op2 (op3 a b) c) d))))
                  `(,op1s (,op2s (,op3s ,a ,b) ,c) ,d))

                (when (not (or (and (eq? op3 /)
                                    (= 0 c))
                               (and (eq? op2 /)
                                    (= 0 (op3 b c)))
                               (and (eq? op1 /)
                                    (= 0 (op2 a (op3 b c))))
                               (and (memq op3 (list + * /))
                                    (< b c))
                               (and (memq op2 (list + * /))
                                    (< a (op3 b c)))
                               (and (memq op1 (list + * /))
                                    (< (op2 a (op3 b c)) d))))
                  `(,op1s (,op2s ,a (,op3s ,b ,c)) ,d))

                (when (not (or (and (eq? op3 /)
                                    (= 0 c))
                               (and (eq? op2 /)
                                    (= 0 (op3 b c)))
                               (and (eq? op1 /)
                                    (= 0 (op2 (op3 b c) d)))
                               (and (memq op3 (list + * /))
                                    (< b c))
                               (and (memq op2 (list + * /))
                                    (< (op3 b c) d))
                               (and (memq op1 (list + * /))
                                    (< a (op2 (op3 b c) d)))))
                  `(,op1s ,a (,op2s (,op3s ,b ,c) ,d)))

                (when (not (or (and (eq? op2 /)
                                    (= (op2 a b) 0))
                               (and (eq? op1 /)
                                    (= (op3 c d) 0))
                               (and (memq op3 (list + * /))
                                    (< c d))
                               (and (memq op2 (list + * /))
                                    (< a b))
                               (and (memq op1 (list + * /))
                                    (< (op2 a b) (op3 c d)))))
                  `(,op1s (,op2s ,a ,b) (,op3s ,c ,d))))))

          (assert (eqv? 24 (eval exp (interaction-environment))))
          exp
          )))))