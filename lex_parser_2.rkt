#lang scheme
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))
         ;parser-tools/yacc)

(provide (all-defined-out))

(define calc+-lexer
  (lexer
   [(:+ (:or (char-range #\a #\z) (char-range #\A #\Z)))
    ; =>
    (cons `(ID ,(string->symbol lexeme))
          (calc+-lexer input-port))]
   
   
   [(:: #\/ #\*)
    ; =>
    (comment-lexer input-port)]
   
   [#\( 
    ; =>
    (cons '(LPAR)
          (calc+-lexer input-port))]
   
   [#\)
    ; =>
    (cons '(RPAR) 
          (calc+-lexer input-port))]
   
   [(:: (:? #\-) (:+ (char-range #\0 #\9)))
    ; =>
    (cons `(INT ,(string->number lexeme))
          (calc+-lexer input-port))]
   
   [(:or #\+ #\*)
    ; =>
    (cons `(OP ,(string->symbol lexeme))
          (calc+-lexer input-port))]
   
   [whitespace 
    ; =>
    (calc+-lexer input-port)]
   
   [(eof)
    '()]))

(define comment-lexer 
  (lexer
   [(:: #\* #\/)
    (calc+-lexer input-port)]
   
   [any-char
    (comment-lexer input-port)]))

(calc+-lexer (open-input-string "-3 * (foo/*a variable*/ + 12)"))