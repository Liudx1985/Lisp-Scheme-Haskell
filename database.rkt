#lang racket
(require db)

(define con
  (mysql-connect #:user "root"	 	 	 	 
                 #:database "alldb"	 	 	 	 
                 #:server "10.78.222.55"	 	 	 	 
                 #:port 3306	 	 	 	 
                 #:password "vislecaina"))

(define results
  (query-rows con
              "SELECT p.partno,n.neno,n.nename
	FROM ne n,part p
	WHERE n.partid=p.partid")
  )


; Convert byte-vector(from encoding) to other byte-vector(to-encoding).
 ; ISO-8859-1: latin-1
 ; GBK : GB2312
 ; UTF-8
 ; UTF-16
(define code-convert
  (lambda (source from to)
    (let* ([cvt (bytes-open-converter from to)]
           [len (bytes-length source)]
           [dest (make-bytes (* 2 len))])
      (bytes-convert cvt source 0 len dest)
      (bytes-close-converter cvt)
      dest)))

(display (length results))
; open-file-input-port & svae
(define fd (open-output-file "d:/ne.txt" #:exists 'replace))
(for-each (lambda (x) 
   (begin (display (code-convert 
                    (string->bytes/latin-1 (vector-ref x 2))
                    "GBK" "UTF-8") fd)
          (display "\r\n" fd))) 
          results)

(close-output-port fd)


