#lang scheme
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(provide (all-defined-out))
; brace {}
; bracket []
; Parentheses ()
(define-tokens value-tokens (NUM VAR STRING CHAR ASSIGN EXPLST DEFLST))
; Ignored Gramma.
(define-empty-tokens op-tokens (newline = OP CP OB CB
                                        QUOT SIG_QUOT ;[" ']
                                        COMMA SEMICOLON COLON COLON2;; [, ; :]
                                        + - * / ^ < >
                                        EOF NEG COMMENTC COMMENT INCLUDE
                                        ENUM STRUCT CLASS))

#|
alphabetic
  lower-case
  upper-case
  title-case
  numeric
  symbolic
  punctuation
  graphic
  whitespace
  blank
  iso-control
|#
(define-lex-abbrevs
  (lower-letter (:/ "a" "z"))
  (upper-letter (:/ #\A #\Z))
  ;; (:/ 0 9) would not work because the lexer does not understand numbers.  (:/ #\0 #\9) is ok too.
  (digit (:/ "0" "9"))
  (alpha (:/ "A" "z"))
  (variable (:: (:or "_" alpha) (:* (:or "_" alpha digit))) )
  (space (:? #\space))
  (assign (:: space variable space "=" space (:or variable digit) space))
  (def (:: space variable space variable space ";" space))
  )
 

(define cxx-lexer
  (lexer
   [(eof) 'EOF]  
   ["{" 'OB]
   ["}" 'CB]
   ["(" 'OP]
   [")" 'CP]
   ["\"" 'QUOT]
   ["'" 'SIG_QUOT]
   ["," 'COMMA]
   [";" 'SEMICOLON]
   [":" 'COLON]
   ["::" 'COLON2]
   
   ["struct" 'STRUCT]
   ["class" 'CLASS]
   ["#include"  'INCLUDE]
   ["enum"  'ENUM]
   ["struct" 'STRUCT]
   [#\newline 'newline] ; (token-newline) returns 'newline   
   ;; recursively call the lexer on the remaining input after a tab or space.  Returning the
   ;; result of that operation.  This effectively skips all whitespace.
   [(:or #\tab #\space) (cxx-lexer input-port)]
   [(:: "//" (complement (:: any-string "\n" any-string))) 'COMMENT]  ; C++ stype /* COMMENTs */
   [(:: "/*" (complement (:: any-string "*/" any-string)) "*/") 'COMMENTC]  ; C stype /* COMMENTs */
   [(:or "=" "+" "-" "*" "/" "^" "<" ">") (string->symbol lexeme)]
   [(:+ digit) (token-NUM (string->number lexeme))]
   [(:: variable) (token-VAR (string->symbol lexeme))]   
   [(:: "'" alpha "'") (token-CHAR lexeme)]   ; any char
   [(:: "\"" (complement (:: any-string "\"" any-string)) "\"") (token-STRING lexeme)]   ; any string
   [(:: variable (:+ #\space) assign ";") (token-ASSIGN lexeme)]
   [(:: (:or assign variable) (:? (:: "," (:or assign variable))) (:? ",")) (token-EXPLST lexeme)]
   [(:: (:+ def)) (token-DEFLST lexeme)]
   ))

(define cxx-parser
  (parser
   (start start)
   (end newline EOF)
   (error void)
   (tokens value-tokens op-tokens)
   (precs (right =)
          (left - +)
          (left * /)
          (left NEG)
          (right ^)
          )
   (yacc-output "d:/cxx.y")
   (grammar
     (start [() #f]
           ;; If there is an error, ignore everything before the error
           ;; and try to start over right after the error
           [(error start) $2]
           [(exp) $1])
    (exp
     [(COMMENT) ":COMMENT"]
     [(COMMENTC) ":COMMENT-C"]
     [(NUM) (list ":numberic" $1)]
     [(CHAR) (list ":char" $1)]
     [(STRING) (list ":string" $1)]
     [(VAR) (list ":variable" $1)]
     [(EXPLST) (list ":explst" $1)]
     [(DEFLST) (list ":deflst" $1)]
     [(ASSIGN) (list ":=" $1)]
     [(VAR VAR = STRING SEMICOLON) (list $1 $2 "=s" $4)]
     [(VAR VAR = CHAR SEMICOLON) (list $1 $2 "=c" $4)]
     [(INCLUDE < exp >) (list ":include<" $3 ">")]
     [(INCLUDE STRING)  (list ":include" $2)]
     [(ENUM VAR OB EXPLST CB SEMICOLON) (enum_proc $2 $4)]
     [(STRUCT VAR OB CB SEMICOLON) (struct_proc $2 "")]
     [(STRUCT VAR OB DEFLST CB SEMICOLON) (struct_proc $2 $4)]
     
     ))
   ))

;; run the calculator on the given input-port       
(define (CxxParse ip)
  (port-count-lines! ip)
  (letrec ((one-line
	    (lambda ()
	      (let ((result (cxx-parser (lambda () (cxx-lexer ip)))))
		(when result
                  (printf "~a\n" result)
                  (one-line))))))
    (one-line)))


(define str "520 
 'k'
 \"I LOVE YOU\"
 /*@param a:any param default 0;\n*/
 #include \"iostream\"
 #include <vector>
 int a =1;
 int a =c;
 // Fuck;
 int a =\"Son of **!\";
 int a ='A';
 enum color {red = 1,green = 4}; 
 struct rgba{color clr;int alpha;};")


(define enum_proc
  (lambda (name lst)
    (list name (string-split lst #px","))))

(define struct_proc
  (lambda (name lst)
    (list name (string-split lst #px";"))))

(CxxParse (open-input-string str))

