#lang racket/base
(require "misc.rkt"
         "substitute.rkt"
         "automatic-simplify.rkt"
         "order-relation.rkt"
         racket/list
         racket/match
         )

(provide _let*
         contract-let*
         contract-let*/ascovars
         expand-let*)

(module+ test (require "rackunit.rkt"))

;; The purpose of this 'function' is to remove the binding pairs when the
;; value is an atom (and not an expression), since it would take no more room
;; (in terms of tree size). Also, this ensures that the body can be reduced
;; if possible.
;; Naming it `let*` would badly interact with Racket, because `_let*` doesn't
;; behave exactly like `let*`. The list of bindings are not ids, but expressions
;; so one must write like (_let* `([a ,(+ 2 'b)]) ...).
;; NOTICE: _let* triggers `automatic-simplify` (through concurrent-substitute),
;;   which recurse through the body.
;;   ALSO automatic-simplify triggers _let*.
(define _let*
  (match-lambda*
    [`( ,(list bindings ...) ,body)
     ;; Recursively reduce the bindings if possible (possibly after a substitution).
     (define-values (subst new-bindings)
       (for/fold ([subst '()] ; substitution list (not an assoc)
                  [new-bindings '()]) ; new list of binding to keep in let*
                 ([bind (in-list bindings)])
         (define new-val (concurrent-substitute (cadr bind) subst))
         (define new-bind (list (car bind) new-val))
         (if (pair? new-val)
           (values subst (cons new-bind new-bindings))
           (values (cons new-bind subst) new-bindings))))
     (define new-body (concurrent-substitute body subst))
     
     #;(debug bindings body subst new-bindings)

     ;; Now check if a binding is used only once, in which case it should be
     ;; substituted back.
     (define counts (ids-occurrences (map car new-bindings)
                                     (list new-bindings new-body)))
     (define-values (subst2 new-bindings2)
       (for/fold ([subst '()]
                  [new-bindings '()])
                 ([bind (in-list new-bindings)])
         (define id (car bind))
         (define c (hash-ref counts id))
         (cond [(= 1 c) ; id is never used (only defined), remove it
                (values subst new-bindings)]
               [(= 2 c) ; id is used only once, this is wasteful
                (values (cons bind subst) new-bindings)]
               [else (values subst (cons bind new-bindings))])))
     (define new-body2 (concurrent-substitute new-body subst2))
     ;; Also apply the substitution to the new bindings.
     ;; (bindings are in reverse order, new-bindings2 in correct order,
     ;; and so is new-bindings3).
     (define new-bindings3
       (for/list ([bind (in-list new-bindings2)])
         (list (car bind)
               (concurrent-substitute (cadr bind) subst2))))

     #;(debug subst2 new-bindings2 new-bindings3)

     ;; Reconstruct the let* form, or remove it if not needed anymore.
     (if (null? new-bindings3)
       new-body2
       `(let* ,new-bindings3 ,new-body2))]
    
    [other (error "_let* bad form" other)]))
(register-function 'let* _let*)


;; New bindings are placed after the existing top-level bindings (if any).
;; TODO: Check name collisions + error?
(define (try-merge-let*-bindings bindings body)
  (match body
    ;; If body is a let*, merge the new bindings with the old ones.
    [`(let* ,(list sub-bindings ...) ,sub-body)
     `(let* ,(append sub-bindings bindings)
        ,sub-body)]
    ;; Otherwise just create a new let* body.
    [else
     `(let* ,bindings
        ,body)]))

;; WARNING: The order of the bindings in the let* is syntactically not important because
;; `concurrent-substitute` substitutes also into substitutions, but is computationally
;; important because reduction can be made either before or after substitution.
;; Compare:
#;(let* ([b 3]
         [c 2]
         [a (* (+ b c 1) (+  b c 2) (+ b c 3))])
    (+ a b c))
#;(let* ([a (* (+ b c 1) (+  b c 2) (+ b c 3))]
         [b 3]
         [c 2])
    (+ a b c))
;; In the first one, b and c are substituted in a which thus reduces to a value
;; which makes the evaluation of the body simple.
;; In the second one, b and c are substituted in a only after a has been substituted
;; in the body, making the evaluation more demanding.


;; Recursively replaces the most used sub-expression (syntactic check) in tree
;; with a let* binding and returns the result.
;; Returns the original expression if no sub-expression occurs at least twice.
;; This is probably not the most efficient implementation.
(define (contract-let* tree
                       #:prefix [prefix (gensym)]
                       #:idx [first-idx 0])
  (let bind-loop ([tree tree] [idx first-idx])
    (define h (make-hash))
    ;; Count the number of occurrences of each list (really, subtree).
    (let loop ([tree tree])
      (when (list? tree)
        (hash-update! h tree add1 0)
        (map loop tree)))

    ;; Find the largest sub-tree that appears the most times.
    ;; 'index' = the expression tree
    (define-values (best-count _best-k best-subtree)
      (hash-best+key+index h (λ (n.s1 n.s2)
                               (or (> (car n.s1) (car n.s2))
                                   (and (= (car n.s1) (car n.s2))
                                        (> (cdr n.s1) (cdr n.s2)))))
                           ; Use a key to calculate the tree size only once
                           #:key (λ (t n-occs) (cons n-occs (tree-size t)))))

    #;(debug best-count _best-k best-subtree)
  
    ;; Replace the most used list with a binding.
    (cond
      [(or (not best-count) ; empty hash
           (= 1 best-count)) ; Can't reduce anything.
       (automatic-simplify tree)]
      [else
       (define id (symbol-format "~a~a" prefix idx))

       ;; Recurse.
       ;; Do *not* trigger an automatic-simplify at this stage as the list of bindings
       ;; is incomplete (so some could get removed).
       (bind-loop (try-merge-let*-bindings
                   (list (list id best-subtree))
                   (substitute/no-simplify tree best-subtree id))
                  (+ idx 1))])))

;; For associative+commutative variadic operators like + and *.
;; We care only about non-lists, since lists are dealt with contract-let*.
;; op : symbol?
(define (contract-let*/ascovar tree op #:prefix [prefix (gensym)])

  (let bind-loop ([tree tree] [idx 0])
    ;; Co-occurrence table.
    (define counts (make-hash))
    ;; Count each co-occurrence of non-lists in an op list.
    (let loop ([tree tree])
      (when (list? tree)
        (for-each loop tree)
        (when (eq? op (operator-kind tree))
          (define args (rest tree))
          (define filt (filter-not pair? args))
          (for* ([(x r) (in-list+rest filt)]
                 [y (in-list r)])
            ;; Lists are assumed sorted according to order-relation, so
            ;; co-occurrences should always appear in the same order x then y.
            (hash-update! counts (cons x y) add1 0)))))

    ;; Find the best co-occurrence pair and its number of occs.
    (define-values (best-n _best-n best-p)
      (hash-best+key+index counts >))
    
    (cond
      [(or (not best-n)
           (<= best-n 1))
       ; Nothing more to do.
       (automatic-simplify tree)]
      [else
       (define x (car best-p))
       (define y (cdr best-p))
       ;; The id to introduce.
       (define id (symbol-format "~a~a" prefix idx))
       ;; Replace every co-occurrence of x and y with the new id
       ;; Do *not* trigger an automatic-simplify at this stage as the list of bindings
       ;; is incomplete (so some could get removed).
       (define new-body
         (variadic-replace op (list x y) id tree))

       #;(debug id op x y new-body)
       #;(read-line)
       
       ;; First recurse for a new binding pair, then construct the let* form.
       ;; Can we do several substitutions in parallel? If independent?
       ;; TODO: After compression, if an id appears only once, substitute it back
       ;; (and simplify). Maybe leave this to _let*?       
       (bind-loop (try-merge-let*-bindings `([,id (,op ,x ,y)])
                                           new-body)
                  (+ 1 idx))])))

;; Recursively replaces in tree every co-occurrence of the from-subtrees in an list of
;; operator-kind op with new-subtree and returns the result.
(define (variadic-replace op from-subtrees new-subtree tree)
  ; Make sure they are in the correct order.
  #;(set! froms (sort order-relation froms))
  (let loop ([tree tree])
    (cond
      [(eq? op (operator-kind tree))
       (define args (map loop (rest tree)))
       (define-values (new-args _rm not-rm)
         (remove-all+ from-subtrees args))
       ; Both x and y have been removed.
       (if (null? not-rm)
         ; Don't apply automatic-reduction here as the let* is yet incomplete,
         ; and some bindings may be unrightfully removed.
         (cons op (sort (cons new-subtree new-args) order-relation))
         (cons op (sort args order-relation)))]
      [(list? tree) (map loop tree)]
      [else tree])))

;; Another operator could be `list-no-order` (or `set`)
(define (contract-let*/ascovars tree [ops '(+ *)]
                                #:prefix [prefix (gensym)])
  (for/fold ([tree tree])
            ([op (in-list ops)])
    (contract-let*/ascovar tree op #:prefix (format "~a~a" prefix op))))

;; Expands only the top-level let* if any.
(define (expand-let* u)
  (match u
    [`(let* (,bindings ...) ,body) ; ps are pairs
     ;; Recursively replace the ids with their values
     (define subst
       (for/fold ([subst '()]) ; substitution list (not an assoc)
                 ([bind (in-list bindings)])
         (define new-val (concurrent-substitute (cadr bind) subst))
         (cons (list (car bind) new-val) subst)))
     (concurrent-substitute body subst)]
    [else u]))

(module+ test
  (require "arithmetic.rkt"
           "trig-functions.rkt")

  (check-equal?
   (variadic-replace '* '(_xx12 _xx14) 'ID
                    '(* 0.5 _xx12 _xx14 (+ (* _xx13 _xx9) _xx12 _xx14 (* 2 _xx12 _xx14 _xx9))))
   '(* 0.5 ID (+ (* _xx13 _xx9) _xx12 _xx14 (* 2 ID _xx9))))
  
  (check-equal?
   (contract-let* '(op3 (op1 (op2 a b)
                             (op2 a b))
                        (op1 (op2 a b)
                             (op2 a b)))
                  #:prefix '_w)
   '(let* ((_w0 (op2 a b))
           (_w1 (op1 _w0 _w0)))
      (op3 _w1 _w1)))
  
  (check-equal? (_let* '() (+ 'a 2)) (+ 'a 2))
  (check-equal? (_let* '([a 5]) (+ 'a 2)) 7)
  (check-equal? (_let* '([a 5] [b a] [c (+ b a)]) (+ 'a 'b 'c))
                20)
  (check-equal? (_let* '([a b]) (+ 'a 2)) (+ 'b 2))
  (check-equal? (_let* '([a (+ b 5)]) (+ 'a 2))
                (+ 'b 7))
  (check-equal? (contract-let* (+ (log (+ 'x 3))
                                  (exp (+ 'x 3))
                                  (^ (+ 'x 3) 'a))
                               #:prefix '_y)
                `(let* ([_y0 ,(+ 'x 3)])
                   ,(+ (log '_y0) (exp '_y0) (^ '_y0 'a))))
  (check-equal? (expand-let*
                 (contract-let* (+ (log (+ 'x 3))
                                   (exp (+ 'x 3))
                                   (^ (+ 'x 3) 'a))
                                #:prefix '_y))
                '(+ (exp (+ 3 x)) (log (+ 3 x)) (^ (+ 3 x) a)))
  (check-equal?
   (contract-let* '(*
                    2
                    (^ a 2)
                    (cos (exp (cos (* (^ a 2) (^ (+ b x) 2)))))
                    (exp (cos (* (^ a 2) (^ (+ b x) 2))))
                    (^ (sin (exp (cos (* (^ a 2) (^ (+ b x) 2))))) -1)
                    (sin (log (sin (exp (cos (* (^ a 2) (^ (+ b x) 2)))))))
                    (sin (* (^ a 2) (^ (+ b x) 2)))
                    (+ b x))
                  #:prefix '_y)
   '(let* ((_y0 (^ a 2))
           (_y1 (+ b x))
           (_y2 (* _y0 (^ _y1 2)))
           (_y3 (exp (cos _y2)))
           (_y4 (sin _y3)))
      (* 2 _y0 _y1 _y3 (^ _y4 -1) (cos _y3) (sin _y2) (sin (log _y4))))
   #;`(let* ((_y0 ,(* (^ 'a 2) (^ (+ 'b 'x) 2)))
           (_y1 ,(exp (cos '_y0)))
           (_y2 ,(sin '_y1)))
      (*
       2
       _y1
       (^ _y2 -1)
       (^ a 2)
       (cos _y1)
       (sin _y0)
       (sin (log _y2))
       (+ b x))))
  (check-equal?
   (expand-let*
    `(let* ((_y0 ,(* (^ 'a 2) (^ (+ 'b 'x) 2)))
            (_y1 ,(exp (cos '_y0)))
            (_y2 ,(sin '_y1)))
       (*
        2
        _y1
        (^ _y2 -1)
        (^ a 2)
        (cos _y1)
        (sin _y0)
        (sin (log _y2))
        (+ b x))))
   '(*
     2
     (^ a 2)
     (cos (exp (cos (* (^ a 2) (^ (+ b x) 2)))))
     (exp (cos (* (^ a 2) (^ (+ b x) 2))))
     (^ (sin (exp (cos (* (^ a 2) (^ (+ b x) 2))))) -1)
     (sin (log (sin (exp (cos (* (^ a 2) (^ (+ b x) 2)))))))
     (sin (* (^ a 2) (^ (+ b x) 2)))
     (+ b x)))

  (check-equal?
   (expand-let*
    (contract-let*/ascovars
     '(list (* a b c d e t) (* a b c d e x) (* a b c d e y) (* a b c d e z)
            (+ a b c d e t) (+ a b c d e x) (+ a b c d e y) (+ a b c d e z))
     #:prefix '_w))
   '(list
     (* a b c d e t)
     (* a b c d e x)
     (* a b c d e y)
     (* a b c d e z)
     (+ a b c d e t)
     (+ a b c d e x)
     (+ a b c d e y)
     (+ a b c d e z)))

  ;; Nothing to simplify.
  (check-equal?
   (contract-let*/ascovars '(* (* a b) (* a b))
                           #:prefix '_w)
   '(* (^ a 2) (^ b 2)))

  (check-equal?
   (contract-let*/ascovars
    '(let* ([a (* 2 aa)])
       (op1 (* a b c d e t) (* a b c d e x)))
    #:prefix '_w)
   '(let* ((_w*3 (* 2 aa b c d e)))
      (op1 (* _w*3 t) (* _w*3 x))))
  )


;; TODO: derivative through let*
;;  either expand, derivate, contract, or
;;  derivate sub-expressions with the binding.
;; d/dx (let* ([a <a(x)>] [b <b(x)>] ...) <f(a, b, ... x)>)
;; is syntactically df/dx + da/dx df/da + db/dx df/db...
;; https://en.wikipedia.org/wiki/Chain_rule#Multivariable_case
;; TODO: autodiff(?)
;;  (Can we do batch optimization with autodiff?)
;; https://stats.stackexchange.com/questions/224140/step-by-step-example-of-reverse-mode-automatic-differentiation

(module+ drracket
  (require "derivative.rkt")
  (define f '(cos (exp (sin (log (+ (^ x 2) (* 3 y) z))))))

  (define xp1
    (list (derivative f 'x)
          (derivative f 'y)
          (derivative f 'z)))

  (define lifted-xp1 (contract-let* xp1 #:prefix 'X_))

  lifted-xp1

  (list (tree-size xp1)
        '---reduced-to--->
        (tree-size lifted-xp1))

  )

(module+ drracket
  (require "derivative.rkt"
           "arithmetic.rkt"
           "trig-functions.rkt")
  (newline)
  (derivative (cos (log (sin (exp (cos (sqr (* (+ 'x 'b) 'a)))))))
              'x)
  #;'(*
      2
      (^ a 2)
      (cos (exp (cos (* (^ a 2) (^ (+ b x) 2)))))
      (exp (cos (* (^ a 2) (^ (+ b x) 2))))
      (^ (sin (exp (cos (* (^ a 2) (^ (+ b x) 2))))) -1)
      (sin
       (log (sin (exp (cos (* (^ a 2) (^ (+ b x) 2)))))))
      (sin (* (^ a 2) (^ (+ b x) 2)))
      (+ b x))
  (newline)
  (contract-let*
   (derivative (cos (log (sin (exp (cos (sqr (* (+ 'x 'b) 'a)))))))
               'x)
   #:prefix '_y)
  #;  
  '(let* ((_y0 (* (^ a 2) (^ (+ b x) 2)))
          (_y1 (exp (cos _y0)))
          (_y2 (sin _y1)))
     (*
      2
      _y1
      (^ _y2 -1)
      (^ a 2)
      (cos _y1)
      (sin _y0)
      (sin (log _y2))
      (+ b x)))

  ; let* doesn't look into products and sums for now:
  (newline)
  (contract-let*
   '(+ (* a b c d e t) (* a b c d e x) (* a b c d e y) (* a b c d e z)))

  ; but this one should compress:
  (contract-let*/ascovars
     '(list (* a b c d e t) (* a b c d e x) (* a b c d e y) (* a b c d e z)
            (+ a b c d e t) (+ a b c d e x) (+ a b c d e y) (+ a b c d e z))
     #:prefix '_w)

  )
