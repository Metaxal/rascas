#lang racket/base

(require "misc.rkt"
         "arithmetic.rkt")

(provide solve-quadratic)

;; Returns the two solutions of ax^2 + bx + c = 0
(define (solve-quadratic a b c)
  (define Δ (- (sqr b) (* 4 a c)))
  (list (* 1/2 (/ a) (+ (- b) (sqrt Δ)))
        (* 1/2 (/ a) (- (+ b (sqrt Δ))))))


(module+ test
  (require rackunit
           "coefficient-gpe.rkt")

  ; Both don't reduce nicely for now :(

  (define quad (expand-product 'a (expand-product (- 'x 'r1) (- 'x 'r2))))
  (define a (coefficient-gpe quad 'x 2))
  (define b (coefficient-gpe quad 'x 1))
  (define c (coefficient-gpe quad 'x 0))
  ;; Will not see through the squares...
  #;(distribute (solve-quadratic a b c) '* '+)
  
  #;(check-equal?
   (apply expand-product
          (map (λ (r) (- 'x r))
               (solve-quadratic 'u 'v 'w)))
   (+ (* 'u (^ 'x 2))
      (* 'v 'x)
      'w)))
