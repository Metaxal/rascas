#lang racket/base

;;;; This file has been changed for its original dharmatech/mpl version.

(provide derivative)

(require "misc.rkt"
         "arithmetic.rkt"
         "sin.rkt"
         "cos.rkt"
         "tan.rkt"
         "log.rkt"
         "contains.rkt")
        
(define (derivative u x)
  (cond ( (equal? u x) 1 )
        ( (power? u)
          (let ((v (base     u))
                (w (exponent u)))
            (+ (* w
                  (^ v (- w 1))
                  (derivative v x))
               (* (derivative w x)
                  (^ v w)
                  (log v)))) )
        ( (sum? u)
          (let ( (v (list-ref u 1)) )
            (let ( (w (- u v)) )
              (+ (derivative v x) (derivative w x)))) )
        ( (product? u)
          (let ( (v (list-ref u 1)) )
            (let ( (w (/ u v)) )
              (+ (* (derivative v x) w)
                 (* v (derivative w x))))) )
        ( (sin? u)
          (let ( (v (list-ref u 1)) )
            (* (cos v) (derivative v x))) )
        ( (cos? u)
          (let ( (v (list-ref u 1)) )
            (* (- (sin v)) (derivative v x))) )
        ( (tan? u)
          (let ( (v (list-ref u 1)) )
            (* (^ `(sec ,v) 2) (derivative v x))) )
        ( (free? u x) 0 )
        ( else `(derivative ,u ,x) )))
