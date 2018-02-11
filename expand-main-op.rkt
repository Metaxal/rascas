#lang racket/base

(provide expand-main-op)

(require racket/match
         "arithmetic.rkt"
         "misc.rkt"
         "expand-product.rkt"
         "expand-power.rkt")

(define (expand-main-op u)
  (match u
    ( `(* ,a . ,rest)
      (expand-product a
                      (expand-main-op (apply * rest))) )
    ( `(^ ,a ,b)
      (expand-power a b) )
    ( else u )))
