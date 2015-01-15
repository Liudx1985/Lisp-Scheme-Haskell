#lang racket
(for ([path (in-directory "d:/workspace/dxh")]
      #:when (regexp-match? #rx"[.](cpp)|h$" path))
  (printf "source file: ~a,~a byte\n" path (file-size path)))

