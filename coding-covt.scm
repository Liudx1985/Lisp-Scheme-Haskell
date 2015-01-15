#lang racket
#|
(require rnrs/io/ports-6)
 (define convert
   (lambda (source_string from_code to_code)
     (let ([codes (string->bytevector source_string
                                      (make-transcoder (from_code)))])
        (bytevector->string codes (make-transcoder (to_code)))
       )))
 ;(reencode-output-port (current-output-port) "utf-8")
 (convert "草泥马" utf-8-codec latin-1-codec)
 (convert "ÎäooË3μÀ1⁄2ÖÀ©Õ1640" latin-1-codec utf-8-codec)
 |#

 ; ISO-8859-1 a.k.a latin-1
 ; GBK : GB2312
 ; UTF-8
 ; UTF-16
(define a (string->bytes/latin-1 "ÎäººË³µÀ½ÖÀ©Õ¹640"))
; Convert byte-vector(from encoding) to other byte-vector(to-encoding).
(define code-convert
  (lambda (source from to)
    (let* ([cvt (bytes-open-converter from to)]
           [len (bytes-length source)]
           [dest (make-bytes (* 2 len))])
      (bytes-convert cvt source 0 len dest)
      (bytes-close-converter cvt)
      dest)))

(display (code-convert a "GBK" "UTF-8"))
