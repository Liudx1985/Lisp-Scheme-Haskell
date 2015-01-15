#lang racket
;;; 以下三个定义 env0, ent-env, lookup 是对环境(environment)的基本操作:

;; 空环境
(define env0 '())

;; 扩展。对环境 env 进行扩展，把 x 映射到 v，得到一个新的环境
(define ext-env
  (lambda (x v env)
    (cons `(,x . ,v) env)))

;; 查找。在环境中 env 中查找 x 的值
(define lookup
  (lambda (x env)
    (let ([p (assq x env)])
      (cond
       [(not p) x]
       [else (cdr p)]))))

#|
由于函数体内也许会含有外层函数的参数,比如 (lambda (y) (lambda (x) (* y 2)))
 里面的 y 是外层函数的参数,却出现在内层函数定义中。如果内层函数被作为值返回,
那么 (* y 2) 就会跑到 y 的作用域以外。所以我们必须把函数做成“闭包”(closure)。
闭包是一种特殊的数据结构,它由两个元素组成:函数的定义和当前的环境
;; 闭包的数据结构定义，包含一个函数定义 f 和它定义时所在的环境
|#
(struct Closure (f env))

;; 解释器的递归定义（接受两个参数，表达式 exp 和环境 env）
;; 共 5 种情况（变量，函数，调用，数字，算术表达式）
(define interp1
  (lambda (exp env)
    (match exp                                          ; 模式匹配 exp 的以下情况（分支）
      [(? symbol? x) (lookup x env)]                    ; 变量
      [(? number? x) x]                                 ; 数字
      [`(lambda (,x) ,e)                                ; 函数
       (Closure exp env)]
      [`(,e1 ,e2)                                       ; 调用
       (let ([v1 (interp1 e1 env)]
             [v2 (interp1 e2 env)])
         (match v1
           [(Closure `(lambda (,x) ,e) env1)            ; 用模式匹配的方式取出闭包里的各个子结构
            (interp1 e (ext-env x v2 env1))]))]         ; 在闭包的环境中把 x 绑定到 v2,解释函数体
      [`(,op ,e1 ,e2)                                   ; 算术表达式
       (let ([v1 (interp1 e1 env)]
             [v2 (interp1 e2 env)])
         (match op
           ['+ (+ v1 v2)]
           ['- (- v1 v2)]
           ['* (* v1 v2)]
           ['/ (/ v1 v2)]))])))

;; 解释器的“用户界面”函数。它把 interp1 包装起来，掩盖第二个参数，初始值为 env0
(define interp
  (lambda (exp)
    (interp1 exp env0)))
; Try to debug it!
(interp '((lambda (x)
            (+ 
             ((lambda (y) (* y x)) 2)
             x)) 5))

(interp '((lambda (y) (((lambda (y) (lambda (x) (* y 2))) 3) 0)) 4))
