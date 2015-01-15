; delay call from  http://www.ibm.com/developerworks/cn/linux/l-lazyprog.html
(define f
	(delay
	(begin (display "#first enter#\n")
		 (* 5 6 7 8 9))
	))

(current-time)
(display (force f)) ; this  call will print a 'yes'
(newline)
(display (force f)) ; just return the result of (* 5 6 7 8 9)

(define (square x) (* x x))
(define (calculate-statistics the-series)
   (let* (
          (size (delay (length the-series)))
          (mean (delay (/ (apply + the-series) (force size))))
          ;variance is the sum of  (x[i] - mean)^2
          (variance 
            (delay 
              (let* (
                     (variance-list (map (lambda (x) (square (- x (force mean)))) 
                      the-series)))
                (/ (apply + variance-list) (force size)))))
          (standard-deviation (delay (sqrt (force variance)))))
     (vector mean variance standard-deviation)))

;Run the program
(define stats (calculate-statistics '(2 6 4 3 7 4 3 6 7 8 43 4 3 2 36 75 3)))
(define mean (force (vector-ref stats 0)))
(define variance (force (vector-ref stats 1)))
(define stddev (force (vector-ref stats 2)))
(display (list mean variance stddev))
(newline)
