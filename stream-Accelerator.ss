(load "stream.ss")

(define (square x)
	(* x x))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))


(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

; example on accelerated-sequence.
(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))


(display-stream (stream-subseq (euler-transform
                                      pi-stream) 0 15))

(display-stream (stream-subseq (accelerated-sequence euler-transform
                                      pi-stream) 0 15))
;calc ln(2) using formular
(define (ln2-stream n)
 (cons-stream (/ 1.0 n)
 	(stream-map - (ln2-stream (+ n 1)))))

(define ln2 (partial-sums (ln2-stream 1)))

; accerated-sequence on ln(2) = 0.693147180559945309417232121458176568075500134360255254120680009493393..
(display-stream (stream-head (accelerated-sequence euler-transform ln2) 9))

