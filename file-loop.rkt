#lang racket
; set! add1 
(define-syntax add
  (syntax-rules ()
    ((_ var)
     (set! var (add1 var)))
    ((_ var n)
     (set! var (+ var n)))
    ))

(define ht (make-hash))
(define (gather-strID-file path)
  (let ([line-no 0])
  (call-with-input-file path #:mode 'text
    (lambda (in)      
      (for ([l (in-lines in)])
        (begin (add line-no)
        (when (and (> (string-length l) 8) (string=? (substring l 0 3) "760"))
         (hash-set! ht (string->number (substring l 0 8) 16) line-no)))
        )
      )))
  )

; 获得字符串文件中可用的ID 集合
(gather-strID-file "D:/OTNM/ui/bin/MbmpGuiStr.txt")

(define a (sort (hash-keys ht) <))

;(for-each (lambda (x) (display (format "~x\t" x))) a)

(define id_range (range #X76000001 #X76001400))

(for-each (lambda (x) 
            (when (not (hash-has-key? ht x))
             (display (format "~x\t" x))
             )
            )
          id_range)

