(define remove_
 (lambda (a lat)
  (cond ((null? lat) '())
	((eq? (car lat) a) (remove_ a (cdr lat)))
	(else (cons (car lat) (remove_ a (cdr lat))))
)))


; (append lat a) generate a [lat.a] list not flatten
(define insert-back
	(lambda (lat a)
		(if (list? lat) (append lat (list a))
		'failed))
	)


(define remove_
	(lambda (ls x)
		(filter (lambda (y) (not (eq? y x))) ls)
		))