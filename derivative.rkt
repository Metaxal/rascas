#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(provide derivative)

(require "misc.rkt"
         "arithmetic.rkt"
         "sin.rkt"
         "cos.rkt"
         "tan.rkt"
         "exp.rkt"
         "log.rkt"
         "contains.rkt"
         racket/list)
        
(define (derivative u x)
  (cond [(equal? u x) 1]
        [(exp? u)
         (* (derivative (second u) x)
            u)]
        [(log? u)
         (let ([v (second u)])
           (/ (derivative v x)
              v))]
        [(power? u)
          (let ([v (base     u)]
                [w (exponent u)])
            (+ (* w
                  (^ v (- w 1))
                  (derivative v x))
               (* (derivative w x)
                  (^ v w)
                  (log v))))]
        [(sum? u)
          (let* ([v (second u)]
                 [w (- u v)])
            (+ (derivative v x)
               (derivative w x)))]
        [(product? u)
          (let* ([v (second u)]
                 [w (/ u v)])
            (+ (* (derivative v x) w)
               (* v (derivative w x))))]
        [(sin? u)
          (let ([v (second u)])
            (* (cos v) (derivative v x)))]
        [(cos? u)
          (let ([v (second u)])
            (* (- (sin v)) (derivative v x)))]
        [(tan? u)
          (let ([v (second u)])
            (* (^ `(sec ,v) 2) (derivative v x)))]
        [(free? u x) 0]
        [else `(derivative ,u ,x)]))
