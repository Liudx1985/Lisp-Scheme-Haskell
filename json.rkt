#lang racket
; json is a dict same result of  make_hasheq {eq？}
(define ht (make-hash))
(hash-set! ht 'name_0 'value)
(hash-set! ht "banana" '(yellow long))
(hash-ref ht 'name_0)
(displayln ht)
(when (hash-has-key? ht "banana")
  (hash-remove! ht "banana")
  (displayln "removed banana")
  )
(hash-ref ht "banana" "banana not there")

; read json & parse.
(require json
         xml)
; (call-with-input-file path lambda (f) (...))
(let* ([fo (open-input-file "./test.json")]
      [js (read-json fo)]) 
  (jsexpr? js)
  (displayln (hash-ref js 'name_0 "not exist"))  
  (displayln (jsexpr->string js)); same as (write js (current-output-port))
  (close-input-port fo))

