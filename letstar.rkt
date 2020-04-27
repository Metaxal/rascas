#lang racket/base
(require "misc.rkt"
         "substitute.rkt"
         "automatic-simplify.rkt"
         "order-relation.rkt"
         racket/list
         racket/match
         racket/string
         )

(provide _let*
         contract-let*
         contract-let*/ascovars
         expand-let*
         sort-bindings
         make-make-id
         default-make-id
         rebind-let*
         rebind-all-let*
         variadic-replace
         unbound-ids)

;;; There are many differences with Scheme/Racket's let*.
;;; - ids have a more global scope (for now).
;;; - ids can't shadow previous ids (or it may lead to inconsistencies)
;;; - there can't be twice the same id in the same let* (which is also shadowing).
;;; - ids are sorted automatically in dependency order.
;;; - ...

;;; The make-id mechanism allows to keep consistent naming by making functions
;;; forward the make-id to other functions.

;; Returns a `make-id` procedure that returns a new id every time it is called.
(define (make-make-id prefix)
  (define idx -1) ; first id is 0
  (λ ()
    (set! idx (+ 1 idx))
    (string->symbol (string-append prefix (number->string idx)))))

;; TODO: Rename to make-id?
(define default-make-id (make-make-id "__s"))


;; The purpose of this 'function' is to remove the binding pairs when the
;; value is an atom (and not an expression), since it would take no more room
;; (in terms of tree size). Also, this ensures that the body can be reduced
;; if possible.
;; Naming it `let*` would badly interact with Racket, because `_let*` doesn't
;; behave exactly like `let*`. The list of bindings are not ids, but expressions
;; so one must write like (_let* `([a ,(+ 2 'b)]) ...).
;; NOTICE: _let* reorders the bindings in topological order.
;; NOTICE: _let* triggers `automatic-simplify` (through substitute/dict),
;;   which recurse through the body.
;;   ALSO automatic-simplify triggers sub _let* if any.
(define _let*
  (match-lambda*
    [`( ,(list bindings ...) ,body)
     (let ([bindings (sort-bindings bindings)])
       ;; Recursively reduce the bindings to atomic expressions if possible,
       ;; and substitute back in if reduced.
       (define subst (make-hasheq))
       (define new-bindings
         (for/fold ([new-bindings '()] ; new list of binding to keep in let*
                    ; We must keep the correct order of the bindings.
                    #:result (reverse new-bindings))
                   ([bind (in-list bindings)])
           (define id (car bind))
           (define expr (second bind))
           (define new-val (substitute/dict expr subst))
           (if (pair? new-val)
             ; Still a pair, not reduced enough, we keep the old binding.
             (cons (list id new-val) new-bindings)
             ; Not a pair, that's reduced enough, remove the binding and
             ; add a substitution.
             (begin
               (hash-set! subst id new-val)
               new-bindings))))
       (define new-body (substitute/dict body subst))
     
       #;(debug bindings body subst new-bindings)

       ;; Now check if a binding is used only once, in which case it should be
       ;; substituted back in, and remove bindings that are not used anymore.
       ;; Also through away bindings that are not used at all.
       ;; Important: the bindings must be correcly ordered already.
       (define counts (ids-occurrences (map car new-bindings)
                                       (list new-bindings new-body)))
       (define subst2 (make-hasheq))
       (define new-bindings2
         (for/fold ([new-bindings '()]
                    #:result (reverse new-bindings))
                   ([bind (in-list new-bindings)])
           (define id (car bind))
           (define expr (cadr bind))
           (define c (hash-ref counts id))
           (cond [(= 1 c) ; id is never used (only defined), remove it
                  ;; TODO: If id references other ids, we need to decrease their counts
                  new-bindings]
                 [(= 2 c) ; id is used only once, this is wasteful
                  ; Substituting here avoids a recurrent substitute for body.
                  (hash-set! subst2 id (substitute/dict expr subst2))
                  new-bindings]
                 [else
                  ; Also apply the substitutions to existing bindings.
                  (cons (list id (substitute/dict expr subst2))
                        new-bindings)])))
       (define new-body2 (substitute/dict new-body subst2))

       #;(debug subst2 new-bindings2)

       ;; Reconstruct the let* form, or remove it if not needed anymore.
       ;; Make sure the bindings are in the best order.
       (if (null? new-bindings2)
         new-body2
         `(let* ,new-bindings2 ,new-body2)))]
    
    [other (error "_let* bad form" other)]))
(register-function 'let* _let*)

;; Sort bindings in dependency order (topological order).
;; Not only useful for computational reasons, but derivative may fail
;; if not sorted in chain-rule order.
;; TODO: Check that the same binding doesn't appear twice?
(define (sort-bindings bindings)
  (define bind-hash (make-hasheq))
  (for ([b (in-list bindings)])
    (define id (car b))
    (when (hash-has-key? bind-hash id)
      ; This does *not* check all cases.
      (error "Duplicate identifier in let*:" id))
    (hash-set! bind-hash id (cadr b)))
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
;; Do NOT trigger a _let* or an automatic-simplify here as sorting all bindings again (and more)
;; after introducing a single new one is inefficient (do it in batches instead).
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

;; Replaces the ids with fresh ids in bindings and body to produce a new let* form.
;; TODO: Check that the newly introduced ids don't already appear!
(define (rebind-let* alet* #:make-id [make-id default-make-id])
  (match alet*
    [`(let* ,(list bindings ...) ,body)
     (define n-bindings (length bindings))
     (define subst (make-hasheq))
     (for ([b (in-list bindings)])
       (hash-set! subst (first b) (make-id)))
     ;; No need to apply let*?
     `(let* ,(substitute/dict bindings subst #:simplify? #f)
        ,(substitute/dict body subst #:simplify? #f))]))

;; Rebinds all let*s in the tree to ensure all bindings have different names,
;; and returns the new tree.
;; if `prefix` is not #f, then the `make-id` argument is discarded
;; and a new make-id is created using the prefix,
;; otherwise `make-id` is used to generate the ids.
(define (rebind-all-let* tree [prefix #f] #:make-id [make-id default-make-id])
  (when prefix
    (set! make-id (make-make-id (format "~a" prefix))))
  (define rebind-idx 0)
  (let loop ([tree tree])
    (match tree
      [`(let* ,(list bindings ...) ,body)
       ;; First we must rebinding all bindings below before
       ;; rebinding the current let*.
       (define new-bindings
         (for/list ([b (in-list bindings)])
           (list (car b) (loop (cadr b)))))
       (define new-body (loop body))
       (set! rebind-idx (+ rebind-idx 1))
       (rebind-let* `(let* ,new-bindings ,new-body) #:make-id make-id)]
      [(? list?)
       (map loop tree)]
      [else tree])))

;; Returns a tree with only a single top-level let* and no sublet*.
;; All bindings are renamed for safety.
(define (lift-all-let* tree)
  (define bindings '())
  (define new-body
    ; Returns a new body free of let*.
    (let loop ([tree (rebind-all-let* tree)])
      (match tree
        [`(let* ,(list bds ...) ,body)
         (set! bindings
               (append
                ; Also iterate through the bindings' values and remove the let*s.
                (for/list ([b (in-list bds)])
                  (list (car b) (loop (cadr b))))
                bindings))
         (loop body)]
        [(? list?)
         (map loop tree)]
        [else tree])))
  (_let* bindings new-body))



#;(define (contract-let* tree)
  (let batch-loop ([orig-tree tree] [lift-all? #t])
    ;; First lift all sublet* to the top level (and rename all ids).
    ;; This avoids extracting ids out of their context.
    (let bind-loop ([tree (if lift-all? (lift-all-let* orig-tree) orig-tree)]
                    [iter 0])
      ;; Count the number of occurrences of each list (really, subtree).
      ;; Doing this at each iteration is probably not the most efficient strategy.
      (define h (make-hash))
      (let loop ([tree tree])
        (match tree
          [`(let* ([,ids ,vals] ...) ,body)
           ; We can't compress ids, only expressions
           (loop body)
           (for-each loop vals)]
          [(? list?)
           (hash-update! h tree add1 0)
           (for-each loop tree)]
          [else (void)]))

      ;; Find the largest sub-tree that appears the most times.
      ;; 'index' = the expression tree
      (define-values (best-count best-subtree _best-k)
        (hash-best+key+value h (λ (n.s1 n.s2)
                                 (or (> (car n.s1) (car n.s2))
                                     (and (= (car n.s1) (car n.s2))
                                          (> (cdr n.s1) (cdr n.s2)))))
                             ; Use a key to calculate the tree size only once
                             #:value (λ (t n-occs) (cons n-occs (tree-size t)))))

      #;(debug best-count _best-k best-subtree)
  
      ;; Replace the most used list with a binding.
      (cond
        [(or (not best-count) ; empty hash
             (= 1 best-count)) ; Can't reduce anything.
         #;#;(newline)(debug tree)
         ;; Trigger automatc-simplify only if any change has been made.
         (if (= iter 0)
           orig-tree ; don't change anything (no renaming)
           ; At least one new binding has been made, simplify everything
           ; and try again from the start (without rebinding again).
           ; At the next iteration, if no new binding is created we exit.
           (batch-loop (automatic-simplify tree) #f))]
        [else
         (define id (default-make-id))
         ;; Recurse.
         ;; Do *not* trigger an automatic-simplify at this stage as the list of bindings
         ;; is incomplete (so some could get removed).
         (bind-loop (try-merge-let*-bindings
                     (list (list id best-subtree))
                     (substitute/no-simplify tree best-subtree id))
                    (+ iter 1))]))))
;; Recursively replaces the most used sub-expression (syntactic check) in tree
;; with a let* binding and returns the result.
;; Returns the original expression if no sub-expression occurs at least twice.
;; This is probably not the most efficient implementation.
;; TODO: There is no need to lift all sublet* to the top level. We only need to
;; start by contracting all the sublet* before doing the upper levels.
(define (contract-let* tree)
  ;; First lift all sublet* to the top level (and rename all ids).
  ;; This avoids extracting ids out of their context.
  (let batch-loop ([orig-tree (lift-all-let* tree)])
    (let ([tree orig-tree])
      ;; Count the number of occurrences of each list (really, subtree).
      ;; Doing this at each iteration is probably not the most efficient strategy.
      (define h (make-hash)) ; subtree -> count
      (let loop ([tree tree])
        (match tree
          [`(let* ([,ids ,vals] ...) ,body)
           ; We can't compress ids, only expressions
           (loop body)
           (for-each loop vals)]
          [(? list?)
           (hash-update! h tree add1 0)
           (for-each loop tree)]
          [else (void)]))

      ;; Turn the hash into a list, sorted first by #occurrences, then by tree-size
      ;; (this order is important).
      (define counts
        (sort (hash->list h)
              (λ (n.s1 n.s2)
                (or (> (car n.s1) (car n.s2))
                    (and (= (car n.s1) (car n.s2))
                         (> (cdr n.s1) (cdr n.s2)))))
              ; Use a key to calculate the tree size only once
              #:key (λ (t.n-occs) (cons (cdr t.n-occs) (tree-size (car t.n-occs))))
              #:cache-keys? #t))

      ;; subst : (listof (list/c tree? id))
      ;; This loop creates a list of bindings based on the number of occurrences
      ;; of each subtree, starting with the most occurring one.
      (let loop ([tree tree] [counts counts] [subst '()])
        (cond
          [(or (empty? counts)
               (= 1 (cdr (first counts))))
           (if (empty? subst)
             orig-tree ; return the original tree (eq?) if no change
             ; At least one subtree has been factored-out, so we restart the counting from
             ; scratch in case new factorable subtrees have appeared.
             (batch-loop
              (automatic-simplify
               ; we need to merge with the top level bindings otherwise we may lose some,
               ; if some r.h.s. have been factored out.
               (try-merge-let*-bindings
                (reverse (map reverse subst))
                tree))))]
          [else
           ;; The next subtree that occurs the most.
           ;; First, 
           (define best-subtree
             ; (reverse subst) because we need to make sure that the best subtree is first.
             (sequential-substitute (car (first counts)) (reverse subst) #:simplify? #f))
           (define id (default-make-id))
         
           ;; Observe that for every sub-tree t1 of best-subtree,
           ;; then n-occs(t1) ≥ n-occs(best-subtree), and furthermore 
           ;; if n-occs(t1) > n-occs(best-subtree), then t1 has necessarily been selected
           ;; before best-subtree.
           ;; if n-occs(t1) = n-occs(best-subtree), then t1 appears only within best-subtree,
           ;; and so cannot be selected before (and will not be selected later).
           (loop (substitute/no-simplify tree best-subtree id) ; replace best-subtree with id in tree
                 (rest counts)
                 (cons (list best-subtree id) subst))])))))

;; For associative+commutative variadic operators like + and *.
;; We care only about non-lists, since lists are dealt with contract-let*.
;; op : symbol?
(define (contract-let*/ascovar tree op)

  ;; First rebind all-let* for safety, in case of name collision between
  ;; sub-let*.
  (let bind-loop ([tree (rebind-all-let* tree)] [idx 0])
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
    (define-values (best-n best-p _best-n)
      (hash-best+key+value counts >))
    
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
       (define id (default-make-id))
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
(define (contract-let*/ascovars tree [ops '(+ *)])
  (for/fold ([tree tree])
            ([op (in-list ops)])
    (contract-let*/ascovar tree op)))

;; Expands only the top-level let* if any.
(define (expand-let* u)
  (match u
    [`(let* (,bindings ...) ,body) ; ps are pairs
     ;; Recursively replace the ids with their values
     (define subst (make-hasheq))
     (for ([bind (in-list bindings)])
       (define id (first bind))
       (define expr (second bind))
       (hash-set! subst id (substitute/dict expr subst)))
     (substitute/dict body subst)]
    [else u]))

;; Returns the list of ids that are not bound in a let* and are not functions.
(define (unbound-ids tree)
  (define prefix "_NO_")
  (define h (make-hasheq))
  (let loop ([tree (rebind-all-let* tree #:make-id (make-make-id prefix))])
    (cond [(and (symbol? tree)
                (not (string-prefix? (symbol->string tree) prefix))
                (not (symbol->function tree)))
           (hash-set! h tree #t)]
          [(list? tree) (for-each loop tree)]))
  (hash-keys h))
