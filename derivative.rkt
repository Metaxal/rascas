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
  #;(displayln u)
  (cond
    [(equal? u x) 1]
    [(free? u x) 0] ; check early to simplify early, but can be costly in total!
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
       #;[`(* . ,vs)
          ; Based on the trick: df(x)/dx = f(x) dlog(f(x))/dx
          ; This takes only linear time but gives inverse values which is an issue
          ; with zeros.
          ; It's also a little more complicated to simplify.
          (* u (apply + (map (λ (v) (/ (derivative v x)
                                       v))
                             vs)))]
       [`(* ,v . ,ws)
        ; This can take quadratic time with the number of arguments but doesn't
        ; produce inverses like the variant above.
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

       #;[`(gamma ,v)]
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

;; This is just a curiosity, although it works quite well.
;; Minimalistic derivative based on df(x)/dx = f(x)dlog(f(x))/dx,
; and heavily relies a lot on automatic reduction, in particular of the log.
(module+ drracket
  (define (tree-size tree)
    (cond [(pair? tree)
           (+ (tree-size (car tree))
              (tree-size (cdr tree)))]
          [else 1]))
  
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
  (let ([f
         (apply * (build-list 1000 
              (λ (x) (+ 'x (random 1000)))))])
    (list (tree-size (deriv* f 'x))
          (newline)
          (tree-size (derivative f 'x))))
  )


