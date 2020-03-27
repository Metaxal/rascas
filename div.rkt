#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(require racket/match
         "sum-product-power.rkt") ; + * ^

(provide / simplify-quotient)

(define (simplify-quotient u)
  (match u
    [`(/ ,x ,y)
     (* x (^ y -1))]))

(define /
  (case-lambda
    [(u v) (simplify-quotient `(/ ,u ,v))]
    [(u) (simplify-quotient `(/ 1 ,u))]))

(module+ test
  (require rackunit)
  (check-equal? (/ 4) 1/4)
  (check-equal? (/ 'a) '(^ a -1))
  (check-equal? (/ 3 4) 3/4)
  (check-equal? (/ 3 'a) '(* 3 (^ a -1))))

  