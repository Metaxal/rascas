#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(require racket/match
         "sum-product-power.rkt") ; + * ^

(provide / simplify-quotient)

(define (simplify-quotient u)
  (match u
    [`(/ ,x . ,ys)
     (* x (^ (apply * ys) -1))]))

(define /
  (case-lambda
    [(u) (simplify-quotient `(/ 1 ,u))]
    [(u . vs) (simplify-quotient `(/ ,u . ,vs))]))

(module+ test
  (require rackunit)
  #;(displayln "tests...")
  (check-equal? (/ 4) 1/4)
  (check-equal? (/ 'a) '(^ a -1))
  (check-equal? (/ 3 4) 3/4)
  (check-equal? (/ 3 'a) '(* 3 (^ a -1)))
  (check-equal? (/ 3 1 2 4) 3/8)
  (check-equal? (/ 'a 2 (* 2 'a)) 1/4)
  (check-equal? (/ 'a 2 'b) '(* 1/2 a (^ b -1))))

  