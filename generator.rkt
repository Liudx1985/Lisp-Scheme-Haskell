#lang racket
(require racket/generator)
; define a generator
(define rand
    (infinite-generator
      (yield (random 100))))


(require r5rs)
;(null-environment 5) return a r5rs env
(syntax? #'(+ 1 2))
;(eval syntax? env)
(eval #'(+ 1 2) (null-environment 5))

(define ns (make-base-namespace))
(apply (eval '+ ns) '(1 2 3))
