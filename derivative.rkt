#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(provide derivative
         derivative/proc
         numeric-derivative ; export only for test submod?
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
       #;[`(* . ,ws)
        ;; Can lead to nan for zeros of u (instead of 0), but otherwise expands linearly
        ;; instead of quadratically.
        (* u (derivative (log u) x))]
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
                            (define d-arg-x (derivative arg x))
                            (if (zero? d-arg-x)
                              d-arg-x
                              (* (apply df args)
                                 d-arg-x)))
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




;; Returns the jacobian of tree on xs, in the form of a _list surrounded
;; by a _let*, using 'reverse' mode.
;; Takes advantage of the large amount of shared code between the different derivatives,
;; which usually leads to a much smaller jacobian tree than length(xs) × size(tree).
;;
;; Go through the bindings in *reverse* order,
;; collect the derivative along each branch,
;; and when hitting an input symbol x, collect in a hash.
;; Finally, build the _let* with a _list.
;;
;; If `log-product?` is not #f, then products of more than 8 elements
;; use the rule dprod/dx = prod × dlog(prod)/dx which can lead to a more space-efficient
;; representation.
;; This has the drawback that zeros of the product may lead to +nan.0 instead of 0 in the derivative,
;; but for some cases this is ok (like gradient descent).
;; (8 because n(n-1)/2 > 2n when n ≥ 6, but we add a small penalty for the inconvenience.)
;; It can be a good idea to apply factor-product afterwards (not done automatically due to
;; potentially large cost).
;;
;; If `+tree?` is not #f, then the first argument of the _list of the returned tree is
;; the tree itself. This can be useful to share code and computation time as a lot of the tree
;; is reused in the jacobian.
(define (jacobian tree xs
                  #:+tree? [+tree? #f]
                  #:log-product? [log-product? #f])
  (define rev-bindings (make-hash)) ; expr -> id
  (define hbindings (make-hasheq)) ; id -> expr
  ;; Step 1, create a computation graph (bindings are nodes)
  (define last-id ; might be a number?
    (let loop ([tree (rebind-all-let* tree)]) ; Should we rebind by default?
      ; Start with the bottom.
      (define res
        (match tree
          [`(let* ,(list subbindings ...) ,body)
           (for ([b (in-list subbindings)])
             (define-values (id expr) (apply values b))
             (when (hash-has-key? hbindings id)
               (error "ID already exists in hash:" id expr))
             (define new-id (loop expr))
             (hash-set! hbindings id new-id)
             (hash-set! rev-bindings new-id id)) ; not useful?
           ;; TODO: What if the binding already exists?
           (loop body)]
          ;; If log-product is #f, deconstruct products into binary products as they may lead
          ;; to quadritically too many terms otherwise.
          [`(* ,v)
           (loop v)]
          [`(* ,v . ,ws)
           #:when (not log-product?)
           (list '* (loop v) (loop `(* . ,ws)))]
          ;; All other operators (including +)
          [`(,op . ,args)
           (cons op (map loop (cdr tree)))]
          [else tree]))
      (cond
        [(number? res) res]
        [(symbol? res) res]
        [(hash-ref rev-bindings res #f)] ; reuse binding if possible (this may be a little costly)
        [else
         (define id (default-make-id))
         (hash-set! hbindings id res)
         (hash-set! rev-bindings res id)
         id])))
  ;; Step 2: backpropagation
  (define bindings (sort-bindings (hash-map hbindings list)))
  #;(debug bindings)
  (define ids (map first bindings))
  ;; The derivative for each x is updated in the jach hash.
  ;; TODO: Use struct nodes instead of hashes?
  (define jach (make-hasheq))
  (for ([x (in-list xs)])
    (hash-set! jach x 0))
  (for ([id (in-list ids)])
    (hash-set! jach id 0))
  (hash-set! jach last-id 1) ; WARNING: WHAT IF NUMBER?
  (define new-bindings
    (for/fold ([new-bindings '()]) ; are returned in the correct order
              ([bind (in-list (reverse bindings))])
      (define-values (id expr) (apply values bind))
      (define diffid (default-make-id))
      (cond [(and (symbol? expr)
                  (hash-has-key? jach expr))
             ; Chain rule: d.f/d.expr = d.f/d.id × d.id/d.expr + A = d.f/d.id + A
             ; where A is d.f/d.other-ids.
             ; diffid will be bound to its derivative d.f/d.id in the resulting let*.
             (hash-update! jach expr (λ (A) (+ A diffid)))]
            [(list? expr) ;; TODO: check operator is known? (derivative should do that though)
             ; For each subid (subexpr), calculate derivative.
             (define subids (filter (λ (subid) (and (symbol? subid)
                                                    (hash-has-key? jach subid)))
                                    (rest expr)))
             (if (and log-product?
                      (eq? '* (operator-kind expr))
                      (length>= expr 8))
               (for ([subid (in-list subids)])
                 ;; log-product rule. Leads to linearly many terms compared to quadratically many
                 ;; for the default rule, but may lead to nan instead of 0 sometimes.
                 (hash-update! jach subid (λ (A) (+ A (* diffid id (derivative (log expr) subid))))))
               (for ([subid (in-list subids)])
                 ; Chain rule: d.f/d.subid = d.f/d.id × d.id/d.subid + A  (where A is d.f/d.other-ids)
                 ; diffid will be bound to its derivative d.f/d.id in the resulting let*.
                 (hash-update! jach subid (λ (A) (+ A (* diffid (derivative expr subid)))))))])
      ; Once we reach id, all its parents have been processed already,
      ; so its derivative is definitive and we can propagate to its children.
      (list* bind (list diffid (hash-ref jach id)) new-bindings)))
  (define diffs (for/list ([x (in-list xs)])
                  (hash-ref jach x)))
  (define new-body
    (apply _list
           (if +tree?
             (cons last-id diffs)
             diffs)))
  #;(debug bindings new-bindings new-body)
  ;; Now we can reduce the let*
  ;; The simplify-top is in the likely case where some ids are used 0 times,
  ;; but increase the count of ids in their bound expr. 
  (simplify-top (_let* new-bindings new-body)))



;; Calculates and returns the derivative of each xs and applies the substitutions subst
;; at the same time.
;; If the substitution is complete and inexact? is not #f, numeric values are returned.
;; The returned tree should produce a subst list
#;(define (diff+subst f xs subst #:inexact? [inexact? #f]))
