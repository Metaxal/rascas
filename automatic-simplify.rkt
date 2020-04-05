#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(provide automatic-simplify)

(require "misc.rkt"
         "arithmetic.rkt"
         "sin.rkt"
         "cos.rkt"
         "tan.rkt")

(define (kind v)
  (and (pair? v)
       (car v)))

(define (automatic-simplify u)
  (if (list? u)
      (let ((v (map automatic-simplify u)))
        ;; TODO: All operators should follow the logic
        ;; (apply op (cdr v))
        ;; instead of relying on simplify-* functions
        ;; (that is, the operators should be forced to
        ;; do the simplifications)
        (cond ((power?      v) (simplify-power      v))
              ((product?    v) (simplify-product    v))
              ((sum?        v) (simplify-sum        v))
              ((quotient?   v) (simplify-quotient   v))
              ((difference? v) (simplify-difference v))
              ((factorial?  v) (simplify-factorial  v))

              ( (eq? (kind v) 'exp) (apply exp (cdr v)) )
              ( (eq? (kind v) 'log) (apply log (cdr v)) )
              ( (eq? (kind v) 'sin) (apply sin (cdr v)) )
              ( (eq? (kind v) 'cos) (apply cos (cdr v)) )
              ( (eq? (kind v) 'tan) (apply tan (cdr v)) )

              ( (eq? (kind v) 'sqrt) (apply sqrt (cdr v)) )
              
              (else                                 v)))
      u))
