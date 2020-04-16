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
     ;; Recursively reduce the bindings to atomic expressions if possible,
     ;; and substitute back in if reduced.
     (define-values (subst new-bindings)
       (for/fold ([subst '()] ; substitution list (not an assoc)
                  [new-bindings '()]) ; new list of binding to keep in let*
                 ([bind (in-list bindings)])
         (define new-val (concurrent-substitute (cadr bind) subst))
         (define new-bind (list (car bind) new-val))
         (if (pair? new-val)
           ; Still a pair, not reduced enough, we keep the old binding.
           (values subst (cons new-bind new-bindings))
           ; Not a pair, that's reduced enough, remove the binding and
           ; add a substitution.
           (values (cons new-bind subst) new-bindings))))
     (define new-body
       (concurrent-substitute body subst))
     
     #;(debug bindings body subst new-bindings)

     ;; Now check if a binding is used only once, in which case it should be
     ;; substituted back in, and remove bindings that are not used anymore.
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
     (define new-body2 (recurrent-substitute new-body subst2))
     ;; Also apply the substitution to the new bindings.
     ;; (bindings is in reverse order, new-bindings2 is in correct order,
     ;; and so is new-bindings3).
     (define new-bindings3
       (for/list ([bind (in-list new-bindings2)])
         (list (car bind)
               (recurrent-substitute (cadr bind) subst2))))

     #;(debug subst2 new-bindings2 new-bindings3)

     ;; Reconstruct the let* form, or remove it if not needed anymore.
     ;; Make sure the bindings are in the best order.
     (let ([bindings (sort-bindings new-bindings3)]
           [body new-body2])
       (if (null? bindings)
       body
       `(let* ,bindings ,body)))]
    
    [other (error "_let* bad form" other)]))
(register-function 'let* _let*)

;; Sort bindings in dependency order.
;; Not only useful for computational reasons, but derivative may fail
;; if not sorted in chain-rule order.
(define (sort-bindings bindings)
  (define bind-hash (make-hasheq))
  (for ([b (in-list bindings)])
    (hash-set! bind-hash (car b) (cadr b)))
  (define indices (make-hasheq))
  (define (get-idx id)
    (hash-ref!
     indices id
     ;; First time query, get the max idx of the ids in expression tree and add 1.
     (λ ()
       (define tree (hash-ref bind-hash id))
       (+ 1
          ; Find the max idx of the ids in the tree.
          (let loop ([tree tree] [idmax 0])
            (cond [(pair? tree)
                   (loop (cdr tree)
                         (loop (car tree) idmax))]
                  [(symbol? tree)
                   (if (hash-has-key? bind-hash tree)
                     (max idmax (get-idx tree))
                     idmax)]
                  [else idmax]))))))
  ; ids with the same index can be safely swapped.
  (sort bindings < #:key (λ (b) (get-idx (car b)))))

;; Sort failure:
#;(sort '(10) (λ (a b) (when (= a b) (error "a=b" b)) (< a b))
        #:key (λ (x) x)
        #:cache-keys? #t)

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


(define let*-default-id-prefix
  (let ([idx 0])
    (λ ()
      (set! idx (+ 1 idx))
      (string-append "__s" (number->string idx) "_"))))


;; Recursively replaces the most used sub-expression (syntactic check) in tree
;; with a let* binding and returns the result.
;; Returns the original expression if no sub-expression occurs at least twice.
;; This is probably not the most efficient implementation.
(define (contract-let* tree
                       #:prefix [prefix (let*-default-id-prefix)]
                       #:idx [first-idx 0])
  (let bind-loop ([tree tree] [idx first-idx])
    (define h (make-hash))
    ;; Count the number of occurrences of each list (really, subtree).
    (let loop ([tree tree])
      (match tree
        [`(let* ([,ids ,vals] ...) ,body)
         ; We can't compress bindings, only expressions
         (loop body)
         (for-each loop vals)]
        [(? list?)
         (hash-update! h tree add1 0)
         (for-each loop tree)]
        [else (void)]))

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
       ;; Trigger automatc-simplify only if any change has been made.
       (if (= idx first-idx)
         tree
         (automatic-simplify tree))]
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
(define (contract-let*/ascovar tree op #:prefix [prefix (let*-default-id-prefix)])

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
       ; automatic-simplify is triggerred only if any change has been made.
       #;(debug tree)
       (if (= idx 0)
         tree
         (automatic-simplify tree))]
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
       (if (null? not-rm)
         ; Both x and y have been removed.
         ; Don't apply automatic-reduction here as the let* is yet incomplete,
         ; and some bindings may be unrightfully removed.
         (cons op (sort (cons new-subtree new-args) order-relation))
         ; We need to sort in case any change has been made below.
         ; TODO: sort only if any change occurred.
         (cons op (sort args order-relation)))]
      [(list? tree) (map loop tree)]
      [else tree])))

;; Another operator could be `list-no-order` (or `set`)
(define (contract-let*/ascovars tree [ops '(+ *)]
                                #:prefix [prefix (let*-default-id-prefix)])
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
         (define new-val (recurrent-substitute (cadr bind) subst))
         (cons (list (car bind) new-val) subst)))
     (recurrent-substitute body subst)]
    [else u]))

(module+ test
  (require "arithmetic.rkt"
           "trig-functions.rkt")

  ;; TODO: Make sure than any id has an index that is at least as high
  ;; as any of its dependencies +1.
  (check-equal?
   (remove '(e 5)
           (sort-bindings
            '([a (+ b c)]
              [d (* b c)]
              [e 5] ; causes problems with `sort` as it doesn't compare with b
              [b (* c (exp c))])))
   ; Make sure that b is placed before a and d.
   '((b (* c (exp c))) (a (+ b c)) (d (* b c))))

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

  (check-equal? (_let* `([a x] [b y] [c (* z z)])
                       '(+ a b c))
                '(+ x y (^ z 2)))
  (check-equal? (_let* `([a x] [b y] [c ,(* 'z 'z)])
                       (* 'c (+ 'a 'b 'c)))
                '(let* ((c (^ z 2))) (* c (+ c x y))))
  (check-equal? (_let* '() (+ 'a 2)) (+ 'a 2))
  (check-equal? (_let* '([a 5]) (+ 'a 2)) 7)
  (check-equal? (_let* '([a 5] [b a] [c (+ b a)]) (+ 'a 'b 'c))
                20)
  (check-equal? (_let* '([a b]) (+ 'a 2)) (+ 'b 2))
  (check-equal? (_let* '([a (+ b 5)]) (+ 'a 2))
                (+ 'b 7))

  ; nested let
  (check-equal? (_let* `([a 5])
                       (+ 3
                          (_let* `([b 6])
                                 (* 'a 'b))
                          (_let* `([c 10])
                                 (+ 'a 'c))))
                48)


  
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

  ;; When extracting from the bindings,
  ;; the new binding must be above, not after.
  (check-equal?
   (contract-let*
    #:prefix '_y
    (_let* `([c ,(* (+ 'a 'b) (exp (+ 'a 'b)))])
           (* 'c (log 'c))))
   '(let* ([_y0 (+ a b)]
           [c (* _y0 (exp _y0))])
      (* c (log c))))

  ;; The new binding must be in the middle of the list(!)
  (check-equal?
   (contract-let*
    #:prefix '_y
    (_let* `([a ,(* 'b (exp 'b))]
             [c ,(* (^ 'a (+ 'a 'b)) (exp (^ 'a (+ 'a 'b))))])
           (* 'c (log 'c))))
   `(let* ([a ,(* 'b (exp 'b))]
           [_y0 (^ a (+ a b))]
           [c (* _y0 (exp _y0))])
      (* c (log c))))

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


  (check-not-exn
   (λ ()
     (contract-let*
      #:prefix '_cc
      '(list
        (let* ((_w_out_1_0
                (* 0.5 (+ (* _w_0_0_0 a) (* _w_0_0_1 b) (* _w_0_0_2 c) (* _w_0_0_3 d) (* _w_0_0_4 e)) (+ 1 (sgn (+ (* _w_0_0_0 a) (* _w_0_0_1 b) (* _w_0_0_2 c) (* _w_0_0_3 d) (* _w_0_0_4 e)))))))
          (*
           0.5
           (list
            (+
             (* 2.0 _w_1_0_0 _w_2_0_0 'a 'b)
             (* 2.0 _w_1_1_0 _w_2_0_1 'c 'd)))
           (+
            (* 2 b (dirac (+ (* _w_0_0_0 a) (* _w_0_0_1 b) (* _w_0_0_2 c) (* _w_0_0_3 d) (* _w_0_0_4 e))) (+ (* _w_0_0_0 a) (* _w_0_0_1 b) (* _w_0_0_2 c) (* _w_0_0_3 d) (* _w_0_0_4 e)))
            (* b (+ 1 (sgn (+ (* _w_0_0_0 a) (* _w_0_0_1 b) (* _w_0_0_2 c) (* _w_0_0_3 d) (* _w_0_0_4 e))))))))
        (let* ((_w_out_1_0
                (* 0.5 (+ (* _w_0_0_0 a) (* _w_0_0_1 b) (* _w_0_0_2 c) (* _w_0_0_3 d) (* _w_0_0_4 e)) (+ 1 (sgn (+ (* _w_0_0_0 a) (* _w_0_0_1 b) (* _w_0_0_2 c) (* _w_0_0_3 d) (* _w_0_0_4 e)))))))
          (*
           0.5
           (list
            (+
             (* 2.0 _w_1_0_0 _w_2_0_0 'a 'b)
             (* 2.0 _w_1_1_0 _w_2_0_1 'c 'd)))
           (+
            (* 2 c (dirac (+ (* _w_0_0_0 a) (* _w_0_0_1 b) (* _w_0_0_2 c) (* _w_0_0_3 d) (* _w_0_0_4 e))) (+ (* _w_0_0_0 a) (* _w_0_0_1 b) (* _w_0_0_2 c) (* _w_0_0_3 d) (* _w_0_0_4 e)))
            (* c (+ 1 (sgn (+ (* _w_0_0_0 a) (* _w_0_0_1 b) (* _w_0_0_2 c) (* _w_0_0_3 d) (* _w_0_0_4 e)))))))))
      )))

  ;; The problem here is that contract-let* will find ([c (+ a b)]) as
  ;; something to compress, which is not right.
  ;; we should look only inside expressions
  (check-not-exn
   (λ () (contract-let*
          #:prefix '_y
          (+
           (_let* `([c ,(+ 'a 'b)])
                  (* 'c (log 'c)))
           (_let* `([c ,(+ 'a 'b)])
                  (* 'c 'd (log 'c)))))))
  )
