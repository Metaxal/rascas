#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(provide derivative)

(require "misc.rkt"
         "arithmetic.rkt"
         "contains.rkt"
         racket/list
         racket/match)


(define (derivative u x)
  (cond
    [(equal? u x) 1]
    [(free? u x) 0] ; check early to simplify early, but can be costly in total!
    [else
     (match u
       ;; Due to their variadic nature, + and * are treated specially.
       [`(+ . ,vs)
        (apply + (map (λ (v) (derivative v x))
                      vs))]
       #;[`(* . ,vs)
          ; Based on the trick: df(x)/dx = f(x) dlog(f(x))/dx
          ; This takes only linear time but gives inverse values which is an issue
          ; with zeros.
          ; See the drracket submodule below for an example where it takes way fewer
          ; steps than the default method.
          ; It's also a little more complicated to simplify.
          (* u (apply + (map (λ (v) (/ (derivative v x)
                                       v))
                             vs)))]
       [`(* ,v . ,ws)
        ; This can take quadratic time with the number of arguments but doesn't
        ; produce inverses like the variant above.
        (define *ws `(* . ,ws))
        (+ (* v (derivative *ws x))
           (* *ws (derivative v x)))]

       ;; General case.
       [`(,op . ,args)
        ; The number of derivs must be equal to the number of args
        (define dfs (function-derivatives op))
        (if dfs
          (begin
            (unless (= (length args) (length dfs))
              (error "The number of derivatives does not match the number of arguments"
                     dfs args))
            (apply + (map (λ (df arg)
                          (* (apply df args)
                             (derivative arg x)))
                        dfs
                        args)))
          `(derivative ,u ,x))]
       ; Unknown case (reachable?).
       [else `(derivative ,u ,x)])]))

;; So that an expression that could not be derived earlier can now be.
;; See tests for an example.
;; This can also be used to take the derivative after a substitution rather than before.
(register-function 'derivative derivative)

(module+ test
  (require rackunit
           "automatic-simplify.rkt"
           "substitute.rkt"
           "trig-functions.rkt"
           "special-functions.rkt")
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
                '(* 3 (^ (cos (* 3 x)) -2)))

  ;; TODO: Check registering a derivative of 3 args
  

  ; Check that 'derivative is registered as a function
  (check-equal? (automatic-simplify '(derivative (sqr x) x))
                (* 2 'x))

  ;; Register a function /after/ it has been used for derivation.
  ;; Simplify the expression to obtain the correct derivative.
  (let ()
    (define bad-deriv (derivative '(__unknown-deriv (* x 2)) 'x))
    (check-equal? bad-deriv '(derivative (__unknown-deriv (* x 2)) x))
    (register-function '__unknown-deriv (λ (x) (sqr x)))
    (check-equal? (automatic-simplify bad-deriv)
                  (* 8 'x)))
  

  (define (make-df f [h 0.001])
    (λ (x)
      (->inexact
       (/ (- (gamma (+ x h)) (gamma (- x h)))
          (* 2 h)))))

  (let ([f gamma])
    (define dfnum (make-df f))
    (define dfsym (derivative (f 'x) 'x))
    (for ([i (in-range 10)])
      (define x (* 10 (random)))
      (check-= (->inexact
                (/ (dfnum x)
                   (substitute dfsym 'x x)))
               1.
               0.01
               )))
  )

;; This is just a curiosity, although it works quite well.
;; Minimalistic derivative based on df(x)/dx = f(x)dlog(f(x))/dx,
; and heavily relies a lot on automatic reduction, in particular of the log.
(module+ drracket
  
  (define (deriv* u x)
    #;(displayln u)
    #;(read-line)
    (cond
      [(not (contains? u x)) 0]
      [(equal? u x)
       1]
      [else
       (match u
         [`(+ . ,vs) (apply + (map (λ (v) (deriv* v x)) vs))]
         [`(log ,v) (/ (deriv* v x)
                       v)]
         [else (* u (deriv* (log u) x))])]))

  ;; Can lead to much shorter formula (but not always)
  (let ([f (apply * (build-list 1000 (λ (x) (+ 'x (random 1000)))))])
    (list (tree-size (deriv* f 'x))
          (newline)
          (tree-size (derivative f 'x))))
  )


