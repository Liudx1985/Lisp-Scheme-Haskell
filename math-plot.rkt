#lang racket
(require plot)
#|
(plot (polar (λ (θ) θ) 0 (* 16 pi)))
(define (f1 θ) (+ 1/2 (* 1/6 (cos (* 5 θ)))))
(define (f2 θ) (+ 1 (* 1/4 (cos (* 10 θ)))))
(plot (list (polar-axes #:number 10)
              (polar-interval f1 f2 #:label "[f1,f2]")))

 (plot (list (function sqr 0 2)
              (point-label (vector 1 1))))

(plot3d (polar3d (λ (θ ρ) 1)) #:altitude 25)
|#

(require math)

(define M (matrix [[1 2] [3 4]]))
(matrix-inverse M) ;计算逆矩阵

; QR 分解矩阵
(define MA
    (matrix [[12 -51   4]
             [ 6 167 -68]
             [-4  24 -41]]))
(define-values (Q R) (matrix-qr M))
(values Q R)
; LU 矩阵分解
(define-values (L U) (matrix-lu M))
(values L U)