#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(provide automatic-simplify
         ->inexact)

(require "misc.rkt"
         "arithmetic.rkt"
         "sin.rkt"
         "cos.rkt"
         "tan.rkt"
         (prefix-in rkt: (only-in math/base euler.0))
         (prefix-in rkt: (only-in racket/math pi)))

(module+ test
  (require rackunit))

(define (automatic-simplify u [inexact? #f])
  (let loop ([u u])
    (if (list? u)
      (let ([v (map loop u)])
        ;; TODO: All operators should follow the logic
        ;; (apply op (cdr v))
        ;; instead of relying on simplify-* functions
        ;; (that is, the operators should be forced to
        ;; do the simplifications)
        ;; Provide a 'plugin' mechanism to add user-custom ops.
        (case (car v)
          [(^) (simplify-power      v)]
          [(*) (simplify-product    v)]
          [(+) (simplify-sum        v)]
          [(/) (simplify-quotient   v)] ; should not appear
          [(-) (simplify-difference v)] ; should not appear
          [(!) (simplify-factorial  v)]
          [(sqr)  (apply sqrt (cdr v))]
          [(sqrt) (apply sqrt (cdr v))]
          [(exp)  (apply exp (cdr v))]
          [(log)  (apply log (cdr v))]
          [(sin)  (apply sin (cdr v))]
          [(cos)  (apply cos (cdr v))]
          [(tan)  (apply tan (cdr v))]
          [else v]
          ))
      (if inexact?
        (cond
          [(number? u) (exact->inexact u)]
          [(symbol? u)
           (case u
             [(π pi) rkt:pi]
             [else u])]
          [else u])
        u))))

;; Replaces every exact value with an inexact value.
;; If the tree is an expression and is free of free variables,
;; it should reduce to a single number.
;; (The tree may also be a list of expressions, or may contain unknown
;; operators.)
;; TODO: bigfloats and other precisions?
(define (->inexact u)
  (automatic-simplify u #t))

(module+ test
  (require (prefix-in rkt: (only-in racket/base * log)))
  (check-equal? (->inexact 'x) 'x)
  (check-equal? (->inexact 2) 2.)
  (check-equal? (->inexact (log 2)) (rkt:log 2))
  (check-equal? (automatic-simplify '(* 3 pi)) '(* 3 pi))
  (check-equal? (* 3 'pi) '(* 3 pi)) ; check this still works
  (check-equal? (->inexact '(* 3 pi)) (rkt:* 3 rkt:pi))
  (check-equal? (->inexact (/ 3 (* 2 3))) .5)
  )
