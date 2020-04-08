#lang racket/base

(require "misc.rkt"
         "arithmetic.rkt"
         (prefix-in rkt: math/special-functions))

(provide gamma)

;; TODO: a mechanism to add matchers for different functions,
;; for example (+ (^ (cos x) 2) (^ (sin x) 2) reduce to 1.
;; or (log (gamma x)) is calculated with rkt:log-gamma.
;; The matcher would be on the main operator, here '+.
;; * Same for derivatives.

;; https://en.wikipedia.org/wiki/Gamma_function
(define-simple-function gamma       rkt:gamma)
(define-simple-function psi0        rkt:psi0)
(define-simple-function erf         rkt:erf)
(define-simple-function erfc        rkt:erfc)
(define-simple-function lambert     rkt:lambert)
(define-simple-function lambert-    rkt:lambert-)
(define-simple-function zeta        rkt:zeta)
(define-simple-function eta         rkt:eta)
(define-simple-function Fresnel-S   rkt:Fresnel-S)
(define-simple-function Fresnel-C   rkt:Fresnel-C)
(define-simple-function Fresnel-RS  rkt:Fresnel-RS)
(define-simple-function Fresnel-RC  rkt:Fresnel-RC)


(module+ test
  (require rackunit
           "automatic-simplify.rkt")
  (check-false (number? (log (gamma 3/2))))
  (check-pred number? (->inexact (log (gamma 3/2))))
  (check-false (number? (psi0 3)))
  (check-pred number? (->inexact (psi0 3))))