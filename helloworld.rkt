#lang racket
; cd $racket
; raco exe helloworld.rkt
(define (Hello you)
	(display (string-append "hello world! " you))
	)

(Hello (symbol->string (read)))

(define typeof
  (lambda (x)
    (cond 
      [(null? x) "Null list"]
      [(list? x) "list"]
      [(pair? x) "Pair"]      
      [(vector? x) "Vector"]
      [(number? x) "Number"]
      [(string? x) "String"]
      [(char? x) "Char"]
      [(boolean? x) "Boolean"]
      [(symbol? x) "Symbol"]
      [(procedure? x) "Procedure"]
      [else 'Unknown]
      )))