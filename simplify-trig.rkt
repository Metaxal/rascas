#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(provide simplify-trig)

(require "arithmetic.rkt"
         "numerator.rkt"
         "denominator.rkt"
         "rationalize-expression.rkt"
         "expand-trig.rkt"
         "contract-trig.rkt"
         "trig-substitute.rkt"
         "algebraic-expand.rkt")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Original version from book

;; (define (simplify-trig u)
;;   (let ((w (rationalize-expression (trig-substitute u))))
;;     (/ (contract-trig (expand-trig (numerator   w)))
;;        (contract-trig (expand-trig (denominator w))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 2009/10/01

;; This version calls 'algebraic-expand' between 'contract-trig' and
;; 'expand-trig'. This enables 'simplify-trig' to work on EA Example 7.16.

(define (simplify-trig u)
  (let ((w (rationalize-expression (trig-substitute u))))
    (/ (contract-trig (algebraic-expand (expand-trig (numerator   w))))
       (contract-trig (algebraic-expand (expand-trig (denominator w)))))))