#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(provide derivative
         derivative/proc)

(require "misc.rkt"
         "algorithmic.rkt"
         "arithmetic.rkt"
         "contains.rkt"
         "substitute.rkt"
         racket/list
         racket/match)

;;; In rascas we consider symmetric derivatives lim_{h->0} (f(x+h)-f(x-h))/(2h):
;;; https://en.wikipedia.org/wiki/Symmetric_derivative
;;; which are defined also for non-continuous functions
;;; such as |x| or sgn(x), and have better numerical stability.

;;; TODO: 'AutoDiff', backprop:
;;;  take as input a substitution dictionary,
;;;  and substitute while differentiating with the
;;;  chain rule to reduce all expressions to numbers
;;;  and be computationally as efficient as calculating
;;;  the forward pass.

;;; TODO: Differentiate on several variables at the same time,
;;; which may allow to share computation and generated code.

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
       [`(* ,v . ,ws)
        ; This can take quadratic time with the number of arguments but doesn't
        ; produce inverses like the variant above.
        (define *ws `(* . ,ws))
        (+ (* v (derivative *ws x))
           (* *ws (derivative v x)))]
       [`(let* (,orig-bindings ...) ,body)
        (define new-body
          ;; Recursively apply the chain rule for each id, unless d.id / d.x = 0.
          (let bind-loop ([bindings orig-bindings] [x x])
            (cond
              [(null? bindings)
               (derivative body x)]
              [else
               (define-values (bid btree) (apply values (first bindings)))
               (when (equal? bid x)
                 ;; TODO: Instead, we could just consider this shadowing and
                 ;; stop the diff for the old-id, and continue with the 'new' id
                 ;; if old-id appears in the binding like [id (+ id 3)].
                 (error "let*: Cannot differentiate for a bound id:" bid))
               (define dbtree (derivative btree x))
               (if (zero-number? dbtree)
                 (bind-loop (rest bindings) x)
                 (+ (bind-loop (rest bindings) x)
                    (* dbtree
                       (bind-loop (rest bindings) bid))))])))
        ; Reconstruct the let (and maybe do some simplifications)
        ; TODO: Here would be the right place to do a contract-let* actually?
        (_let* orig-bindings new-body)]
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

;; Returns a racket procedure of 1 argument which is the derivative of the tree
;; (f sym) at sym.
;; f must be a racket procedure of 1 argument that produces a tree when
;; applied to a tree.
;; If inexact? is not #f, all numbers are turned into inexact numbers.
(define (derivative/proc f #:inexact? [inexact? #f])
  (unless (procedure-arity-includes? f 1 #t)
    (raise-argument-error 'derivative/proc "A function of 1 argument" f))
  (define sym (gensym))
  (tree->procedure (derivative (f sym) sym) sym #:inexact? inexact?))

;; Useful to check equality
(define (numeric-derivative f [ε 0.000001])
  (λ (x)
    (/ (- (f (+ x ε)) (f (- x ε)))
       (* 2 ε))))

(module+ test
  (require rackunit
           "automatic-simplify.rkt"
           "distribute.rkt"
           "substitute.rkt"
           "trig-functions.rkt"
           "special-functions.rkt")

  ;; Tool to test the symbolic derivative value against the numeric one
  ;; f must be a procedure of 1 argument
  (define (check-derivative f [xs (build-list 10 (λ (i) (/ (- (random) .5)
                                                           (random))))])
    (define dfnum (numeric-derivative f))
    (define dfsym (derivative/proc f))
    (for ([v (in-list xs)])
      (check-= (->inexact
                (sqr (- (dfnum v)
                        (dfsym v))))
               0.
               0.001)))
  
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

  (check-derivative gamma (build-list 10 (λ (i) (* 10 (random)))))


  ;;; list

  (check-equal? (derivative (_list 'x 'y 'x 'z) 'x)
                '(list 1 0 1 0))

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
  ; error: Cannot differentiate for a bound id (a)
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

  (check-not-exn
   (λ () (derivative
          '(let* ((_u0 (+ (* _w_0_0_0 a) 1))
                  (_w_out_1_0 (* 0.5 _u0 (+ 1 (sgn _u0))))
                  (_u1 4)
                  (_w_out_1_1 (* 0.5 _u1 (+ 1 (sgn _u1))))
                  (_w_out_2_0
                   (+ 1.0 (exp (* -1 (+ (* 3 _w_out_1_0) (* 2 _w_out_1_1))))))
                  (_w_out_2_1
                   (+ 1.0 (exp (* -1 (+ (* 4 _w_out_1_0) (* 5 _w_out_1_1))))))
                  (_u2 (+ (* 3 _w_out_2_0) (* 2 _w_out_2_1))))
             (+ (* 0.5 _u2 _w_3_0_0 (+ 1 (sgn _u2))) (* 0.5 _u2 _w_3_0_1 (+ 1 (sgn _u2)))))
          '_w_0_0_0)))

  (check-equal?
   (contract-let*
    #:prefix '_g
    (derivative
     '(let* ((a (exp x))
             (b (+ 1.0 a))
             (c (+ -1.0 a))
             (d (^ b -2)))
        (* a (+ (^ b -1) (* -1 d c) (* -1 d a) (* (+ (* -1 d) (* 2 (^ b -3) c)) a))))
     'x))
   '(let* ((_g4 (exp x))
           (b (+ 1.0 _g4))
           (c (+ -1.0 _g4))
           (_g0 (^ b -3))
           (_g3 (^ b -2))
           (_g1 (* 2 _g0 c))
           (_g2 (* -1 _g3)))
      (*
       _g4
       (+
        (* (+ _g1 _g2) _g4)
        (* -1 _g3 _g4)
        (* (+ _g1 (* -2 _g3)) _g4)
        (* _g4 (+ _g2 (* 2 _g0 _g4)))
        (^ b -1)
        (* -1 _g3 c)
        (* -2 _g0 _g4 (+ (* -2 _g4) (* -1 c)))
        (* _g4 (+ (* -1 _g3) (* -6 _g4 (^ b -4) c)))))))

  (check-derivative
   (λ (x)
     `(let* ((a (exp ,x))
             (b (+ 1.0 a))
             (c (+ -1.0 a))
             (d (^ b -3))
             (e (^ b -2))
             (f (* 2 d c))
             (g (* -1 e)))
        (*
         a
         (+
          (^ b -1)
          (* -1 e c)
          (* -1 e a)
          (* (+ (* -2 e) f) a)
          (* (+ (* -1 e) f) a)
          (* -2 d a (+ (* -1 c) (* -2 a)))
          (* a (+ g (* 2 d a)))
          (* a (+ g (* -6 (^ b -4) c a))))))))


  (check-equal? ((derivative/proc sqr) 'a)
                (* 'a 2))
  (check-equal? ((derivative/proc sqr) 2)
                4)
  (check-equal? ((derivative/proc log) 1/2)
                2)
  (check-equal? ((derivative/proc log #:inexact? #t) (sqrt 2))
                (->inexact (/ (sqrt 2))))
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


