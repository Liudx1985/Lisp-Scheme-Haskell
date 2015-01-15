;stream.ss
;(define (delay exp)
;  (lambda () exp))
;  ;(memo-proc (lambda ()
;  ; exp)))
;
;(define (force delayed-object)
;  (delayed-object))
;
;(define (memo-proc proc)
;  (let ((already-run? false) (result false))
;    (lambda ()
;      (if (not already-run?)
;          (begin (set! result (proc))
;                 (set! already-run? true)
;                 result)
;          result))))
 
(define (stream-car stream) (car stream))
 
(define (stream-cdr stream) (force (cdr stream)))
 
; this won't work as a simple function
;(define (cons-stream a b)
;  (cons a (delay b)))
 
; This is scheme syntax for macro
; http://stackoverflow.com/questions/5610480/scheme-sicp-r5rs-why-is-delay-not-a-special-form
(define-syntax cons-stream
  (syntax-rules ()
    [(cons-stream x y) (cons x (delay y))]))
 
(define the-empty-stream '())
 
(define (stream-null? stream)
  (null? stream))
 
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))
 
(define (stream-ref s n)
  (if (stream-null? s) the-empty-stream
      (if (= n 0)
          (stream-car s)
          (stream-ref (stream-cdr s) (- n 1)))))
 
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream (apply proc (map stream-car argstreams))
                   (apply stream-map 
                          (cons proc (map stream-cdr argstreams))))))
 
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))
 
; Neil, 2012-05-10
(define (stream-subseq stream a b)
  (cond ((stream-null? stream) the-empty-stream)
        ((= a b) the-empty-stream)
        ((> a b) the-empty-stream)
        (else (cons-stream (stream-ref stream a)
              (stream-subseq stream (+ a 1) b)))))
 
 ; Fetch the Nth elements of a stream.
(define (stream-head stream n)
   (if (or (stream-null? stream) (= 0 n)) the-empty-stream
    (cons-stream (stream-car stream) (stream-head (stream-cdr stream) (- n 1)))
    ))

(define (display-line x)
  (newline)
  (display x))
 
(define (display-stream s)
  (stream-for-each display-line s))

 (define (stream->list stream)
  (if (stream-null? stream)
    '()
    (cons (stream-car stream)
      (stream->list (stream-cdr stream)))))
 
; examples
;(let ((x (delay (+ 1 2))))
;  (for ([i (in-range 1 10)])
;            (display (force x))))
;
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
 
(define integers
  (integers-starting-from 1))
 
;(display-line (stream-ref integers 0))
(display "\ndisplay the integer numbers from 10000th to 100010th.");
(let ((x (stream-subseq integers 10000 10010)))
  (display-stream x))
 
(define odd-numbers 
  (stream-filter odd? integers))

(display "\ndisplay the odd numbers from 50th to 60th.");
(display-stream (stream-subseq odd-numbers 50 60))

;(let ((x (cons-stream 1 (cons-stream 2 '(3)))))
;  (display-stream x))
 
(define (stream-add s n)
  (stream-map (lambda (x)
                (+ x n)) s))
 
(define (add-streams s1 s2)
  (stream-map + s1 s2))
 
(define fib
  (cons-stream 1
    (cons-stream 1
      (add-streams fib
        (stream-cdr fib)))))

(display "\ndisplay the fibonacci numbers from 100th to 110th.");
(display-stream (stream-subseq fib 100 110))
 
 
(define (divisible? x y)
  (= (remainder x y) 0))
 
(divisible? 10 2)
 
 ; 筛分
(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))
 
(define primes
  (sieve (integers-starting-from 2)))
(display "\ndisplay the primes numbers from 1000th to 1010th.");
(display-stream (stream-subseq primes 1000 1010))


; Newton calc sqrt(x)
(define (average a b) (/ (+ a b) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
      (cons-stream 1.0
        (stream-map (lambda (guess)
          (sqrt-improve guess x))
        guesses)))
  guesses)

; Calc the first 10 iteration of sqrt(10)
(display-stream (stream-subseq (sqrt-stream 10) 1 10))

; sequence Xi+1 = f(Xi)
(define (sequence-stream i-stream f x0)
  (define sequence
    (cons-stream x0
      (stream-map (lambda (x) (f x)) sequence)
      ))
  sequence)

; Display a sequence Xi+1 = 3*Xi + 1, base X0 = 0
(display-stream (stream-subseq (sequence-stream integers (lambda (x) (+ x 3)) 1) 0 10))

; Display a sequence Xi+1 = Xi * 2, base X0 = 1
(display-stream (stream-subseq (sequence-stream integers (lambda (y) (* y 2)) 1) 0 10))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

; calc sum of stream Yi = Sigma(Xi)
(define (partial-sums stream)
  (define sum
    (cons-stream
     (stream-car stream)
     (add-streams sum (stream-cdr stream))))
  sum)

(define (stream-take-while pred? stream)
    (if (stream-null? stream)
        '()
        (if (pred? (stream-car stream))
            (cons-stream (stream-car stream)
                         (stream-take-while pred? (stream-cdr stream)))
            '())))

; stream->list
(stream->list
  (stream-take-while (lambda (x)
    (< x 10))
  integers))

#|;; racket stream
(require racket/stream)

(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))

(define (stream-subseq stream a b)
  (cond ((stream-empty? stream) empty-stream)
        ((= a b) empty-stream)
        ((> a b) empty-stream)
        (else (stream-cons (stream-ref stream a)
              (stream-subseq stream (+ a 1) b)))))

(define (stream-take-while pred? stream)
    (if (stream-empty? stream)
        '()
        (if (pred? (stream-first stream))
            (stream-cons (stream-first stream)
                         (stream-take-while pred? (stream-rest stream)))
            '())))


(stream->list
 (stream-take-while (lambda (x) (< x 8)) (integers-starting-from 0)))

(define x(integers-starting-from 0)) 
(stream->list
 (stream-subseq x 0 10))

(stream->list
 (stream-append (stream-subseq x 0 5) (stream-subseq x 5 10)))
|#