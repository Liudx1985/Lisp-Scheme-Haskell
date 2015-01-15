#lang racket

(with-output-to-file "d:/a.txt" 
  (lambda () (begin (printf "a=1;")(newline)
                    (printf "b=[xx];")(newline)
                    (printf "c=a^b+a*b;")(newline)))
  	#:mode 'text 
        #:exists 'truncate/replace)

(define (display-file path)
  (call-with-input-file path #:mode 'text
    (lambda (in)
      (display (format "[~a,~a byte]\n" path (file-size path)))
      (for ([l (in-lines in)])
        (display l)
        (newline)))
      ))

(if (file-exists? "d:/a.txt")
    (display-file "d:/a.txt")
    '())


#| search? #binary# file
(define (change-ini-file-value file-path key newvalue)
   (let-values (((fi fo) (open-input-output-file file-path #:exists 'update)))
  (define pos (caadr (regexp-match-peek-positions (format "~s=(.+);" key) fi)))  ; get the position
    (display (format "pos=~a" pos))
    (file-position fo pos)
    (write-string (format "~s;\r" newvalue) fo)
    (write-char #\newline fo)
    ; Close file port.
  (close-input-port fi)
  (close-output-port fo)
  )
  )
(change-ini-file-value "D:/a.txt" 'IS_TEST '50)
|#

;
; rule lambda(line,out-port): return the change line
; You can using string port APIs(open-output-string | get-output-string out-string-port): 
; instead of temp file for the small file.
; 
(define (handle-file-lines in-file fn_change)
  (let* ([in (open-input-file in-file)]
        [tmpf (make-temporary-file "rkttmp~a" #f "D:/")]
        [out (open-output-file tmpf #:exists 'update)]
        [line_no 0])
    
    (for ([l (in-lines in)])      
      (display (fn_change l line_no) out) ; copy you string
      (write-string "\r\n" out)
      (set! line_no (add1 line_no)))    
    
    (close-input-port in)
    (close-output-port out)
    (rename-file-or-directory tmpf in-file #t)
    ))

;Rules '(["e" "o"] ["o" "oo"])
(define (regex-replace-file-words file-path rules)
  (handle-file-lines file-path 
                     (lambda (line lineno) (regexp-replaces line rules)))
  )

(regex-replace-file-words "d:/a.txt"
                          '(["a=.*;" "a=0x1;"]
                            ["b=.*;" "b=xyz;"]
                            ["c=.*;" "c=3.1415;"]))
(display-file "d:/a.txt")


