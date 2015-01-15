;Scope.ss
; 因为在 lambda 体内有一个隐式的 begin ，所以可以直接在 lambda 里面使用多条表达式。
(define my-counter
    (let ((count 0))
       (lambda ()
          (set! count (+ count 1))
          count)
       )
    )
)`


(define (withdraw)
	(let ((left 100))
		(lambda (c)
			(set! left (- left c))
		left)
	)
)

; Call with check.
(define (withdraw)
	(let ((left 100))
		(lambda (c)
			(if (> c left)
				'("no enough money")
				(begin (set! left (- left c)) left))
			)))


; declare a object
(define x (withdraw))
; use it!soga
(x 10)
(x 21)
(x -170)