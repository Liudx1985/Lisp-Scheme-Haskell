; call with current continuation = call/cc
; call/cc全称是call with current continuation，在Scheme中是一个函数call-with-current-continuation，
; 用全名和call/cc均可以调用。call/cc可以看作一个控制流的操作符，用来实现程序执行的跳转。
; 所谓一个continuation是指程序运行中的某一个snapshot，告诉我们程序未来将要进行的所有操作。 
; 例如在(sqr (+ 6 7))中，(+ 6 7)所处的continuation便是，得到(+ 6 7)这个值，然后将其平方。

; 在Scheme中，函数call/cc接收另一个函数f作为参数，并将f作用在当前continuation上， 
; 因此call/cc常用来抽取当前的continuation。这个continuation保存了当前程序执行的上下文， 它（指当前continuation）
; 唯一支持的操作是函数调用。 调用一个continuation时，现有的执行流被挂起， 被调用的continuation所对应的
;执行流被恢复。 调用这个continuation时传入的参数则作为创建它的call/cc的返回值（
; 执行流回到call/cc创建这个continuation的地方）。 这样通过call/cc创建出来的continuation可以反复在其他地方调用， 
; 而不仅仅限制于包围call/cc的作用域。 像这种将程序的隐含状态提取出来的过程称为reification（具体化）。

; 传给call/cc的函数f，将会以当前continuation为参数被调用。 在其中，若这个作为参数的continuation被调用， 
; 则控制流返回其对应的call/cc处，并将调用continuation时传入的参数作为这个call/cc的返回值。 否则f正常退出，执行继续。
#lang racket
(define cont null)
;cont = '()
(set! cont (call/cc (lambda (c) c)))
(cont 10)
; cont = 10

(define (find_if lst f)
	(call/cc (lambda (return)
         (for-each (lambda (x) (when (f x) (return x)))
                lst)
        #f))
	)
; Find first if x > 3
(find_if (list 1 2 3 4 5) (lambda (x) (> x 3)))
;4


;; Generator
(define (make-generator lst)
  (define (gateway outer-space)
    (for-each (lambda (element)
    	(set! outer-space
    		(call/cc (lambda (cc)
    		(set! gateway cc) ; 非本地退出
    		(outer-space element)))
                ))
    lst)
    (outer-space 'end-of-list))

  (define (get-next)
    (call/cc gateway))

get-next)

(define aGenerator (make-generator (list 1 2)))

(aGenerator)
(aGenerator)
(aGenerator)
(aGenerator)

;;
;  continuation Coroutine协程
;;
(define *queue* '())
 
(define (empty-queue?)
  (null? *queue*))

(define (enqueue x)
  (set! *queue* (append *queue* (list x))))

(define (dequeue)
  (let ((x (car *queue*)))
    (set! *queue* (cdr *queue*))
    x))
   
(define (fork proc)
  (call/cc
   (lambda (k)
     (enqueue k)
     (proc))))

(define (yield)
  (call/cc
   (lambda (k)
     (enqueue k)
     ((dequeue)))))

(define (thread-exit)
  (if (empty-queue?)
      (exit)
      ((dequeue))))

(define (do-stuff-n-print str)
  (lambda ()
    (let loop ((n 0))
      (when (< n 10) (fprintf (current-output-port) "~A ~A\n" str n)
      (yield)
      (loop (+ 1 n))))))

(fork (do-stuff-n-print "This is AAA"))
(fork (do-stuff-n-print "Hello from BBB"))
(thread-exit)
; CPS = Continuation-passing style
(define (cps-prim f)
  (lambda args
    (let ((r (reverse args)))
      ((car r) (apply f (reverse (cdr r)))))))
; Example 
(define +& (cps-prim +))
(define *& (cps-prim *))
(define sqrt& (cps-prim sqrt))

(define (pyth& x y k)
  (*& x x (lambda (x2)
  	(*& y y (lambda (y2)
  		(+& x2 y2 (lambda (sq)
  			(sqrt& sq k))))))))

(pyth& 3 4 display)