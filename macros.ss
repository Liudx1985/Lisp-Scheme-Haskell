
(define-syntax sum
  (syntax-rules ()
    ((_ exp1 exp2 ...)
     (+ exp1 exp2 ...))))

(display (sum 1 2 3 4 5)) (newline)

(define-syntax ourlst
  (syntax-rules ()
    ((_ exp)
     (cons exp '()))
    ((_ exp1 exp2 ...)
     (cons exp1 (ourlst exp2 ...)))))
(display (ourlst 1 2 3 4 5)) (newline)

#|
(define (xor a b)
  (or (and a (not b))
      (and b (not a))))
|#
(define-syntax xor
	(syntax-rules ()
	((_ exp1 exp2)
	 (or (and exp1 (not exp2)) (and exp2 (not exp1)) ) 
	 )))

; notice the symbol should be (quote x)
; basical type of scheme :
; symbol, pair(generated list), string, number
; usefull tools symbol? string->symbol/symbol->string.
(define-syntax symbol->number
  (syntax-rules ()
    ((_ exp1)
      (string->number (symbol->string exp1))
  )))


; Define SWAP to be a macro
(define-syntax SWAP
   ;;We are using the syntax-rules method of macro-building
   (syntax-rules ()
      ;;Rule Group
      (
         ;;This is the pattern we are matching
         (SWAP a b)
         ;;This is what we want it to transform into
         (let (
               (c b))
            (set! b a)
            (set! a c)))))

; defined a (my-if condition then yes-result else no-result)
(define-syntax my-if
  (lambda (x)
    ;;establish that "then" and "else" are keywords
    (syntax-case x (then else)
      (
        ;;pattern to match
        (my-if condition then yes-result else no-result)
        ;;transformer
        (syntax (if condition yes-result no-result))
       )
)))
; use my-if
(my-if (> x y) then '"x > y" else '"x <= then y")
