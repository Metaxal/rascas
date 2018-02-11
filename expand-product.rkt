#lang racket/base

(provide expand-product)

(require "misc.rkt"
         "arithmetic.rkt")

(define (expand-product r s)
  (cond ( (sum? r)
          (let ((f (list-ref r 1)))
            (+ (expand-product f s)
               (expand-product (- r f) s))) )
        ( (sum? s) (expand-product s r) )
        ( else (* r s) )))
