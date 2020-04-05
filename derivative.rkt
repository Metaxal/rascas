#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(provide derivative)

(require "misc.rkt"
         "arithmetic.rkt"
         "contains.rkt"
         "exp.rkt"
         "log.rkt"
         racket/list
         racket/match)
        
(define (derivative u x)
  (cond
    [(equal? u x) 1]
    [else
     (match u
       [`(sgn ,_) 0] ; actually undefined for x=0(!)
       [`(abs ,v) (sgn v)] ; should be undefined for v=0
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
