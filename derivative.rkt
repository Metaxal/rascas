#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(provide derivative)

(require "misc.rkt"
         "arithmetic.rkt"
         "contains.rkt"
         "sin.rkt"
         "cos.rkt"
         "tan.rkt"
         racket/list
         racket/match)
        
(define (derivative u x)
  (cond
    [(equal? u x) 1]
    [(free? u x) 0] ; check early to simplify early
    [else
     (match u
       [`(sgn ,_) 0] ; actually undefined for x=0(!)
       [`(abs ,v) (sgn v)] ; should be undefined for v=0
       [`(exp ,v)
        (* (derivative v x)
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
       ;; Trigonometry
       [`(sin ,v)
        (* (cos v) (derivative v x))]
       [`(cos ,v)
        (* (- (sin v)) (derivative v x))]
       [`(tan ,v)
        (* (^ `(sec ,v) 2) (derivative v x))]
       ; unknown function symbols
       [else `(derivative ,u ,x)])]))

(module+ test
  (require rackunit)
  (check-equal? (derivative (+ (* 3 (^ 'x 2)) (* 4 'x)) 'x)
                (+ (* 6 'x) 4))
  (check-equal? (derivative (/ 3 'x) 'x)
                (/ -3 (sqr 'x)))
  (check-equal? (derivative (exp (* 3 'x)) 'x)
                '(* 3 (exp (* 3 x))))

  (check-equal? (derivative (log (* 3 'x)) 'x)
                '(^ x -1))

  (check-equal? (derivative (sin (* 3 'x)) 'x)
                '(* 3 (cos (* 3 x))))
  (check-equal? (derivative (cos (* 3 'x)) 'x)
                '(* -3 (sin (* 3 x))))
  (check-equal? (derivative (tan (* 3 'x)) 'x)
                '(* 3 (^ (sec (* 3 x)) 2)))
  )
