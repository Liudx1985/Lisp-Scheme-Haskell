#lang racket
; Remove comments of a cxx file.
(require racket/match)
(match '(1 2 3)
   [(list (not 4) ...) 'yes]
   [_ 'no])

(match "/*God, fuck you!*/"
 [(regexp #rx"//~s") 'yes]
 [(regexp #rx"/*~s*/") 'yes]
 [_ 'no])

; ,@ is  quasiquote
`(1 ```,,@,,@(list (+ 1 2)) 4)
