;for-each R5RS implement

(define (for_each f ls)
  (if (not (null? ls))
      (begin
        (f (car ls))
        (for_each f (cdr ls)))))


(define (reverse_ ls)
	(if (null? ls)
		'()
		(append (reverse_ (cdr ls)) (list (car ls)))))

; variable arguments : combine a single element into  list 
(define (f a . ls)
	(cons a ls))

; standard procedure filter
; for-each element in sequence , if (predicate x) is #t ,append to the result.
(define (filter predicate seq)
	(if (null? seq)
		'()
		(let ([x (car seq)])
		(if (predicate x)
			(cons x (filter predicate (cdr seq)))
			(filter predicate (cdr seq))
			)
		)))

; standard procedure fold-left
(define (fold_left op init seq)
	(define iter (trace-lambda iter (result ls)
		(if (null? ls)
			result
			(iter (op result (car ls)) (cdr ls)))))
	(iter init seq))

; standard procedure fold-right
(define fold_right (trace-lambda fold_right (op init seq)
	(if (null? seq)
		init
		(op (car seq) (fold_right op init (cdr seq)))
		)
	))


; unity procedure append reverse, map build on fold-left
(define (reverse ls)
	(fold_left (lambda (i j) (cons j i)) '() ls))


(define (length list)
	(fold-left (lambda (sum element) (+ sum 1)) 0 list))

; append list y after list x:
(define (append list1 list2)
	(fold_right cons list2 list1))

(define (reverse items)
	(fold-right (lambda (x r) (append r (list x))) '() items))

(define (map p lt)
	(fold-right (lambda (x r) (cons (p x) r)) '() lt))

(define (filter f ls)
    (fold-left 
     (lambda (e r) 
       (if (f e) (append r (list e)) r)) 
     '() ls))

; apply usage
(apply + '(1 2 3 4 5))


; Define a loop syntax.
(define (loop-until start done? next f)
  (let loop ([i start])
    (unless (done? i)
      (f i)
      (loop (next i)))))

(loop-until '0 (lambda (x) (> x 10))  (lambda (x) (+ x 2)) display)
