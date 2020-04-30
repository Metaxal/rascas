#lang racket/base

(require "misc.rkt"
         "arithmetic.rkt"
         "letstar.rkt")
(provide (all-from-out "letstar.rkt")
         _list
         >0 >=0 _> _< _if _max _min)

(module+ test (require "rackunit.rkt"))

;; Just a normal list.
;; Not really 'arithmetic', but too simple to put in a separate file.
(define (_list . args)
  (cons 'list args))
(register-function 'list _list)

(module+ test
  (check-equal? (_list 'a 'b 'c)
                '(list a b c)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; if and tests
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A few primitives

(define (>0 x)
  (if (number? x)
    (if (> x 0) 1 0)
    `(>0 ,x)))
(register-function '>0 >0)
(register-derivative '>0 dirac)

(define (>=0 x)
  (if (number? x)
    (if (>= x 0) 1 0)
    `(>=0 ,x)))
(register-function '>=0 >=0)
(register-derivative '>=0 dirac)

;;; The following are not primitives and so don't need derivatives.

;; This is not really 'algorithmic' as it's not real flow-control.
;; TODO: avoid doing `test` twice. Use a _let*?
(define (_if test y n)
  (slet* ([test test])
     (+ (* test y)
        (* (- 1 test) n))))
(register-function 'if _if)

(define (_> a b)
  (>0 (- a b)))
(register-function '> _>) ; only for reduction

(define (_< a b)
  (>0 (- b a)))
(register-function '< _<)

(define (_max a b)
  (slet* ([a a]
          [b b])
    (_if (_> a b) a b)))
(register-function 'max _max)


(define (_min a b)
  (slet* ([a a]
          [b b])
    (_if (_< a b) a b)))
(register-function 'min _min)
