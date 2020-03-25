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
         racket/list
         racket/match)
        
(define (derivative u x)
  (cond
    [(equal? u x) 1]
    [else
     (match u
       [`(exp ,v)
        (* (derivative (second u) x)
            u)]
       [`(log ,v)
        (/ (derivative v x)
           v)]
       [`(^ ,v ,w)
        (+ (* w
              (^ v (- w 1))
              (derivative v x))
           (* (derivative w x)
              (^ v w)
              (log v)))]
       [`(+ . ,vs)
        (apply + (map (λ (v) (derivative v x))
                      vs))]
       [`(* ,v . ,ws)
        (define *ws (apply * ws))
        (+ (* v (derivative *ws x))
           (* *ws (derivative v x)))]
       [`(sin ,v)
        (* (cos v) (derivative v x))]
       [`(cos ,v)
        (* (- (sin v)) (derivative v x))]
       [`(tan ,v)
        (* (^ `(sec ,v) 2) (derivative v x))]
       [(? (free-of? x)) 0]
       [else `(derivative ,u ,x)])]))
