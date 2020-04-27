#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(provide derivative
         derivative/proc
         jacobian)

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

(define (derivative u x)
  (cond
    [(equal? u x) 1]
    ; Check early to simplify early, but can be costly in total if the tree is a comb.
    [(free? u x) 0] 
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
            ;; TODO: Don't recompute the args, use a let* to compress.
            ;; unless only one arg
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

;; Symmetric derivative.
;; Useful to check equality.
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
   (expand-let*
    (contract-let*
     (derivative
      '(let* ((a (exp x))
              (b (+ 1.0 a))
              (c (+ -1.0 a))
              (d (^ b -2)))
         (* a (+ (^ b -1) (* -1 d c) (* -1 d a) (* (+ (* -1 d) (* 2 (^ b -3) c)) a))))
      'x)))
   (expand-let*
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
         (* _g4 (+ (* -1 _g3) (* -6 _g4 (^ b -4) c))))))))

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

;; --------------------------------------------------------------------------------

;; Reverse mode.
;; Go through the bindings in *reverse* order,
;; collect the derivative along each branch,
;; and when hitting an input symbol x, collect in a hash.
;; Finally, build the let* with a list 
(define (jacobian tree xs)
  (define rev-bindings (make-hash)) ; expr -> id
  (define bindings (make-hasheq)) ; id -> expr
  ;; First, create a computation graph
  ;; WARNING: Does not work with let*
  (define last-id ; might be a number!
    (let loop ([tree tree])
      ; Start with the bottom.
      (define res
        (match tree
          [`(let* ,(list subbindings ...) ,body)
           (for ([b (in-list subbindings)])
             (define-values (id expr) (apply values b))
             (when (hash-has-key? bindings id)
               (error "ID already exists in hash:" id expr))
             (define new-id (loop expr))
             (hash-set! bindings id new-id)
             (hash-set! rev-bindings new-id id)) ; not useful?
           ;; TODO: What if the binding already exists?
           (loop body)]
          [`(,op . ,args)
           (cons op (map loop (cdr tree)))]
          [else tree]))
      (cond
        [(number? res) res]
        [(symbol? res) res]
        [(hash-ref rev-bindings res #f)] ; reuse binding if possible
        [else
         (define id (default-make-id))
         (hash-set! bindings id res)
         (hash-set! rev-bindings res id)
         id])))
  (define jach (make-hasheq))
  ;; TODO: Use struct nodes instead of hashes?
  (for ([x (in-list xs)])
    (hash-set! jach x 0))
  (let loop ([id last-id] [diff 1])
    (cond [(hash-has-key? jach id)
           (hash-update! jach id (λ (old) (+ diff old)))]
          [(hash-ref bindings id #f)
           =>
           (λ (expr)
             ; expr cannot be a let* as they all have been deconstructed.
             (cond
               [(number? expr) (void)]
               [(symbol? expr) (loop expr diff)]
               [(list? expr)
                ; for each subid, calculate derivative
                (for ([subid (in-list (rest expr))])
                  (define diffid (default-make-id))
                  (hash-set! bindings diffid (* diff (derivative expr subid)))
                  (loop subid diffid))]))]
          [else (void)])) ; number or free id 

  (define body (apply _list (for/list ([x (in-list xs)])
                              (hash-ref jach x))))
  #;(debug bindings body)
  ;; Now we can reduce the let*
  ;; The simplify-top is in the likely case where some ids are used 0 times,
  ;; but increase the count of ids in their bound expr. 
  (simplify-top (_let* (hash-map bindings list) body)))

(module+ test
  (check-equal?
   (jacobian (+ (* 'c 'a) (* 'd (^ 'b 2)))
             '(a b))
   '(list c
          (* 2 b d)))

  (check-equal?
   (rebind-all-let*
    (jacobian (exp (+ (* 'c 'a) (* 'd (^ 'b 2))))
              '(a b))
    '_s)
   '(let* ((_s0 (exp (+ (* a c) (* (^ b 2) d)))))
      (list (* _s0 c)
            (* 2 _s0 b d))))
  
  (check-equal?
   (rebind-all-let*
    (jacobian (sin (exp (+ (* 'c 'a) (* 'd (^ 'b 2)))))
              '(a b))
    '_s)
   '(let* ((_s0 (+ (* a c) (* (^ b 2) d)))
           (_s1 (* (cos (exp _s0)) (exp _s0))))
      (list (* _s1 c) (* 2 _s1 b d))))

  ;; let*
  (check-equal?
   (rebind-all-let*
    (jacobian
     `(let* ([a (+ 3 x)]
             [b (+ 2 x a)])
        (+ (* a b)
           (* (log a) (log b))))
     '(a b))
    '_s)
   '(let* ((_s0 (+ 3 x))
           (_s1 (+ 2 _s0 x)))
      (list (+ _s1 (* (^ _s0 -1) (log _s1)))
            (+ _s0 (* (^ _s1 -1) (log _s0)))))))

;; Calculates and returns the derivative of each xs and applies the substitutions subst
;; at the same time.
;; If the substitution is complete and inexact? is not #f, numeric values are returned.
;; The returned tree should produce a subst list
#;(define (diff+subst f xs subst #:inexact? [inexact? #f]))


