#lang racket
(define substring1
  (case-lambda
    [(s) (substring1 s 0 (string-length s))]
    [(s start) (substring1 s start (string-length s))]
    [(s start end) (substring s start end)]))

;;; Calculate sqrt(x * x + y * y)
(define g 
	(lambda (x y) 
		(let ([a (* x x)] [b (* y y)])
		 (sqrt (+ a b)))))

;;;a technique called memoization caching values.
(define memoize
  (lambda (proc)
    (let ([cache '()])
      (lambda (x)
        (cond
          [(assq x cache) => cdr]
          [else
           (let ([ans (proc x)])
             (set! cache (cons (cons x ans) cache))
             ans)]))))) 

(define fibonacci
  (memoize
    (lambda (n)
      (if (< n 2)
          1
          (+ (fibonacci (- n 1)) (fibonacci (- n 2))))))) 

; (sqrt (+ (* x x) (* y y)))
(define hyper
  (let ([g (lambda (x) (* x x))]
        [f (lambda (x y) (+ x y))])
    (lambda (x y) (sqrt (f (g x) (g y))))))

