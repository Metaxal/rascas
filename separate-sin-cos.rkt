#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(provide separate-sin-cos)

(require "misc.rkt"
         "arithmetic.rkt")

(define (sin-or-cos? u)
  (or (sin? u)
      (cos? u)
      (and (power? u)
           (or (sin? (base u))
               (cos? (base u))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (separate-sin-cos u)
  (cond ( (product? u)
          (let loop ((r 1)
                     (s 1)
                     (operands (cdr u)))
            (if (null? operands)
                (list r s)
                
                (let ((operand (car operands)))
                  (if (sin-or-cos? operand)
                      (loop r (* s operand) (cdr operands))
                      (loop (* r operand) s (cdr operands)))))) )
        ( (sin-or-cos? u) (list 1 u) )
        ( else            (list u 1) )))
