#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(provide derivative)

(require "misc.rkt"
         "algorithmic.rkt"
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
       ;;; let*
       [`(let* () ,body)
        (derivative body x)]
       [`(let* ,(list (list id sub) bindings ...) ,body)
        ;; https://en.wikipedia.org/wiki/Chain_rule#Multivariable_case
        (when (equal? id x)
                          (error "let*: Cannot differenciate for a bound id:" id))
        (define dsub (derivative sub x))
        (define sublet `(let* ,bindings ,body))
        (if (zero-number? dsub)
          (derivative sublet x)
          ; Chain rule
          (_let* `([,id ,sub])
                 (+ (derivative sublet x)
                    (* dsub (derivative sublet id)))))]
       #;[`(let* ,(list bindings ...) ,body)
        ;; https://en.wikipedia.org/wiki/Chain_rule#Multivariable_case
        (_let* bindings
               (apply +
                      (derivative body x)
                      (for/list ([b (in-list bindings)])
                        (define id (car b))
                        (when (equal? id x)
                          (error "let*: Cannot differenciate for a bound id:" id))
                        (define sub (cadr b))
                        (define dsub (derivative sub x))
                        (if (zero-number? dsub)
                          0
                          ; Chain rule.
                          (* dsub (derivative body id))))))]
       [`(list . ,args)
        ;; WARNING: list can cause problems when differentiating, e.g.,
        ;; (derivative '(list a b c) 'x) = 0, but should be '(list 0 0 0) instead?
        (apply _list (map (λ (arg) (derivative arg x)) args))]
       ;; General case.
       [`(,op . ,args)
        ; The number of derivs must be equal to the number of args
        (define dfs (function-derivatives op))
        (if dfs
          (begin
            (unless (= (length args) (length dfs))
              (error "The number of derivatives does not match the number of arguments."
                     'function: op 'n-derivs: (length dfs) 'n-args: (length args)))
            #;(displayln args)
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

;; TODO: extend this to multiple arguments and export
#;
(define (derivative->function f sym)
  (tree->function (derivative f sym) sym))

(module+ test
  (require rackunit
           "automatic-simplify.rkt"
           "distribute.rkt"
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

  ;;; list

  (check-equal? (derivative (_list (* 'x 2) (^ 'x 'a) (log 'x)) 'x)
                '(list 2 (* a (^ x (+ -1 a))) (^ x -1)))

  (check-equal? (derivative (_list (* 'x 2) (^ 'x 'a) (log 'x)) 'a)
                '(list 0 (* (log x) (^ x a)) 0))

  ;;; let*
  (check-equal? (derivative (_let* `([a ,(+ 'x 2)])
                                   (* (+ 1 'a) (+ 2 'a))) 'x)
                '(+ 3 (* 2 (+ 2 x))))
  (check-equal? (derivative (expand-let* (_let* `([a ,(+ 'x 2)])
                                                (* (+ 1 'a) (+ 2 'a)))) 'x)
                '(+ 7 (* 2 x)))
  (check-exn exn:fail?
             (λ () (derivative (_let* `([a ,(+ 'x 2)]
                                        [b ,(* 'a 2)])
                                      (* (+ 1 'b) (+ 2 'a)))
                               'a)))
  (check-equal? (derivative (_let* `([a (+ x 2)])
                                   (* (+ 1 'a) (+ 2 'a))) 'y)
                0)

  (check-equal? (distribute-product
                 (expand-let*
                  (derivative (_let* `([b (* (+ 1 a) (+ 2 a))] [c (* (+ b 1) (+ b 2))] [d c])
                                     (* (+ 'd 1) (+ 'd 2)))
                              'a)))
                (distribute-product
                 (derivative (expand-let*
                              (_let* `([b (* (+ 1 a) (+ 2 a))] [c (* (+ b 1) (+ b 2))] [d c])
                                     (* (+ 'd 1) (+ 'd 2))))
                             'a)))
  
  )

;; This is just a curiosity, although it works quite well.
;; Minimalistic derivative based on df(x)/dx = f(x)dlog(f(x))/dx,
; and heavily relies a lot on automatic reduction, in particular of the log.
#;
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


