; sort by <
(define (qsort ls)
	(if (null? ls) '()
		(let ([a (car ls)]
			[left (filter (lambda (x) (< x (car ls))) ls)]
				[right (filter (lambda (x) (> x (car ls))) ls)])
					(append (qsort left) (cons a (qsort right))))))

;; sort : list-of-numbers  ->  list-of-numbers (sorted)
;; to create a list of numbers with the same numbers as
;; alon sorted in descending order
(define (insert_sort alon cmp)
  (cond
    [(null? alon) '()]
    [(list? alon) (insert (car alon) (insert_sort (cdr alon) cmp) cmp)]))

;; insert : number list-of-numbers (sorted)  ->  list-of-numbers (sorted)
;; to create a list of numbers from n and the numbers on
;; alon that is sorted in descending order; alon is sorted
(define (insert n alon cmp)
	(define insert_iter
		(trace-lambda insert_iter (n alon cmp)
		  (cond
		    [(null? alon) (cons n '())]
		    [(cmp n (car alon)) (cons n alon)]
		    [else (cons (car alon) (insert_iter n (cdr alon) cmp))])
		  )
		)
	(insert_iter n alon cmp)
	)


; generate random list of length.

(define (gen_random_list n len)
	(define gen-iter
		(trace-lambda gen-iter (cnt result)
			(if (= 0 cnt)
				result
				(cons (random n) (gen-iter (- cnt 1) result))
				)
			)
		)
	(gen-iter len '())
	)

; test the clock of a functin in scheme: 
(current-time) ; display current time [(current-seconds) in PLT scheme]
(time (gen_random_list 100 100))
(time (insert_sort (gen_random_list 100 100) <))

; Merge Sort, @a @b must be a sorted list.
(define (merge_sorted_list a b)
	(cond [(null? a) b]
		[(null? b) a]
		[(< (car a) (car b)) (cons (car a) (merge_sorted_list (cdr a) b))]
		[else (cons (car b) (merge_sorted_list a (cdr b)))]
	))

