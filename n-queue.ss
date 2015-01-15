; algorithm to solve N-queue promble
; May not work under some Complier, you can use amb.rkt to calc n-queen
(set! null '())

(define (enumerate-interval i j)
 (if (> i j)
  null 
  (cons i (enumerate-interval (+ i 1) j))))

(define (filter f seq) 
  (if (null? seq) 
    null 
    (if (f (car seq))
     (cons (car seq) (filter f (cdr seq))) 
     (filter f (cdr seq)))))

(define (flatmap op seq)
  (fold-right append null (map op seq)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list '())
        (filter
          (lambda (position) (safe? position))
          (flatmap
            (lambda (rest-of-queens)
              (map (lambda (new-row)
                      (cons new-row rest-of-queens))
                   (enumerate-interval 1 board-size)))
            (queen-cols (- k 1))))))

  (define (safe? position)
    (safe-iter? (car position) 1 (cdr position)))
  
  (define (safe-iter? fst n rest-position)
    (cond ((null? rest-position) #t)
          ((= fst (car rest-position)) #f)
          ((= (abs (- fst (car rest-position))) n) #f)
          (else (safe-iter? fst (+ n 1) (cdr rest-position)))))
   (queen-cols board-size))