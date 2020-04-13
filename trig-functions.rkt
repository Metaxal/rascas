#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(provide sin cos tan
         asin acos atan
         sinh cosh tanh
         asinh acosh atanh)

(require "misc.rkt"
         "arithmetic.rkt"
         "sin.rkt"
         "cos.rkt"
         (prefix-in rkt: (only-in racket/base tan asin acos atan))
         (prefix-in rkt: (only-in racket/math sinh cosh tanh))
         (prefix-in rkt: (only-in math/base asinh acosh atanh)))

;; TODO: provide inverse functions so as to automatically reduce f-1(f(x)) to x.
;;   watchout: definition domains! (sqrt (sqr x)) = |x|, not x.
;;   hence f-1(f(x)) reduces to g(x), 
(define-simple-function tan  rkt:tan)
; Special cases to be dealt with later
(define-simple-function asin rkt:asin)
(define-simple-function acos rkt:acos)
(define-simple-function atan rkt:atan)
(define-simple-function sinh rkt:sinh)
(define-simple-function cosh rkt:cosh)
(define-simple-function tanh rkt:tanh)
(define-simple-function asinh rkt:asinh)
(define-simple-function acosh rkt:acosh)
(define-simple-function atanh rkt:atanh)

;;; https://en.wikipedia.org/wiki/Differentiation_rules
(register-derivative 'sin cos)
(register-derivative 'cos (λ (x) (- (sin x))))
(register-derivative 'tan (λ (x) (^ (cos x) -2)))
(register-derivative 'asin (λ (x) (/ (sqrt (- 1 (sqr x))))))
(register-derivative 'acos (λ (x) (- (/ (sqrt (- 1 (sqr x)))))))
(register-derivative 'atan (λ (x) (/ (+ 1 (sqr x)))))
(register-derivative 'sinh cosh)
(register-derivative 'cosh sinh)
(register-derivative 'tanh (λ (x) (^ (cosh x) -2)))
(register-derivative 'asinh (λ (x) (/ (sqrt (+ 1 (sqr x))))))
(register-derivative 'acosh (λ (x) (/ (sqrt (- 1 (sqr x))))))
(register-derivative 'atanh (λ (x) (/ (- 1 (sqr x)))))
