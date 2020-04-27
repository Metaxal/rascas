#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(provide substitute
         substitute/no-simplify
         substitute-this
         substitute-in
         sequential-substitute
         concurrent-substitute
         recurrent-substitute
         apply// ; wrong name?!
         substitute/dict
         tree->procedure)

(require racket/match
         racket/dict
         "automatic-simplify.rkt")

(module+ test
  (require rackunit))

;;; TODO: Substitutions should stop when encountering a bound id with the same name (shadowing).
;;; see bottom for tentative algorithm.


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Returns u where t has been substituted with r, and apply automatic-simplification only
;; if any change has occurred.
;; BETTER: simplify along the branch of the change only.
(define (substitute u t r)
  (define-values (new-u changed?)
    (substitute/no-simplify+? u t r))
  (if changed?
    (automatic-simplify new-u)
    u))

(define (substitute/no-simplify u t r)
  (let loop ([u u])
    (cond [(equal? u t) r]
          [(list? u)
           (map loop u)] ; warning: not aware of let*
          [else u])))

;; Returns u where t has been substituted with r, and whether any substitution has been done.
(define (substitute/no-simplify+? u t r)
  (define changed? #f)
  (define new-u
    (let loop ([u u])
      (cond [(equal? u t)
             (set! changed? #t)
             r]
            [(list? u)
             (map loop u)]
            [else u])))
  (values new-u changed?))

(define (substitute-this t r)
  (lambda (u)
    (substitute u t r)))

(define (substitute-in u)
  (lambda (t r)
    (substitute u t r)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Sequentially substitutes all of L into u.
;; L : (listof (list/c from to))
;; automatic-simplify is triggerred only if any change has been made.
;; WARNING: This function can be slow if S is a large assoc list.
(define (sequential-substitute u L #:simplify? [simplify? #t])
  (cond
    [(null? L) u]
    [else
     (define changed? #f)
     (define new-u
       (let loop ([u u] [L L])
         (match L
           ['() u]
           [`((,t ,r) . ,rem)
            (define-values (new-u ch?)
              (substitute/no-simplify+? u t r))
            (when ch? (set! changed? #t))
            (loop new-u rem)])))
     (if changed?
       (if simplify?
         (automatic-simplify new-u)
         new-u)
       u)]))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; automatic-simplify is triggerred only if any change has been made and simplify? is #t.
;; WARNING: This function can be slow if S is a large assoc list.
;; TODO: to speed up, specialized variant that just substitutes
;; TODO: also allow for a hash table.
;; symbols with eq?, and sort the symbols(not sure it's worth it)
;; u : tree
;; S : 'assoc' list, but instead of cons pairs, it takes lists '(id val)
(define (concurrent-substitute u S [=? equal?]
                               #:simplify? [simplify? #t])
  (cond
    [(null? S) u]
    [else
     (define changed? #f)
     (define new-u
       (let loop ([u u])
         (cond [(assoc u S =?) => (λ (p)
                                    (set! changed? #t)
                                    (cadr p))]
               [(list? u)         (map loop u)]
               [else              u])))
     (if changed?
       (if simplify?
         (automatic-simplify new-u)
         new-u)
       u)]))

;; like concurrent-substitute, but also recurses inside the newly substituted
;; value in case more substitutions apply.
;; Alternatively, we could first apply all substitutions inside S recursively,
;; and then just do concurrent-substitute. Not sure which is best.
;; Reduction is applied only if a change has been made.
;; WARNING: This function can be slow if S is a large assoc list.
(define (recurrent-substitute u S [=? equal?])
  (cond
    [(null? S) u]
    [else
     (define changed? #f)
     (define new-u
       (let loop ([u u])
         (cond [(assoc u S =?)
                =>
                (λ (p)
                  (set! changed? #t)
                  ; Substitute also inside the newly substituted value.
                  ; TODO: we could cache this value in case it comes up again?
                  (loop (cadr p)))]
               [(list? u) (map loop u)]
               [else      u])))
    (if changed?
      (automatic-simplify new-u)
      u)]))

;; Like concurrent-apply, but the substitutions are given as a dictionary.
;; Substitutions apply only to symbols, and substitutions are not done
;; in substitutes.
(define apply//
  (case-lambda
    ;; Substitutions are provided as 2 lists.
    ;; Make a dictionary (hasheq) and 
    [(u syms vals)
     (unless (= (length syms) (length vals))
       (raise-arguments-error 'apply// "syms and vals must have the same lengths"))
     (substitute/dict u (make-hasheq (map cons syms vals)))] ; immutable?
    ))

(define no-key (gensym))

;; Replaces in u every occurrence of an symbol id of subst with the corresponding value.
;; If simplify is #f, no automatic-simplify is applied. Otherwise, automatic-simplify is applied
;; only if a change is detected.
;; If recur? is #t, the substitution is recursively applied to any newly substituted value.
;; u : tree?
;; subst : (dictof symbol? tree?)
(define (substitute/dict u subst
                         #:simplify? [simplify? #t]
                         #:recur? [recur? #f])
  (unless (dict? subst)
    (raise-argument-error 'substitute/dict "dict?" subst))
  (define changed? #f)
  (define new-u
    (let loop ([u u])
      (define new-u (dict-ref subst u no-key))
      (cond [(not (eq? new-u no-key))
             (unless (eq? new-u u)
                 (set! changed? #t))
               (if recur?
                 (loop new-u)
                 new-u)]
            [(list? u) (map loop u)] ; warning: this is not aware of let*
            [else      u])))
  (if changed?
    (if simplify?
      (automatic-simplify new-u)
      new-u)
    u))

(module+ test
  (require "arithmetic.rkt")

  (check-equal? (substitute (* 'x (+ 1 (/ (sqr 'x)))) 'x 0)
                +nan.0)
  (check-equal? (substitute (* 'x (+ 1 (/ (sqr 'x)))) 'x 0.)
                +nan.0)
  (check-equal? (substitute (* 'x (+ 1 (/ (sqr 'x)))) 'x +inf.0)
                +inf.0)
  (check-equal? (substitute (* 'x (+ 1 (/ (sqr 'x)))) 'x -inf.0)
                -inf.0)

  ;; Recurse through the substitutions too.
  (check-equal?
   (recurrent-substitute '(+ a b) '([a (+ b 2)] [b c]))
   (+ (* 2 'c) 2)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Returns a racket procedure from the tree
;; If inexact? is not #f, all numbers are turned into inexact numbers.
(define (tree->procedure tree
                         #:inexact? [inexact? #f]
                         . syms)
    (λ l
      ; TODO: check number of args (map does it though)
      (define subst (map list syms l))
      (define res (recurrent-substitute tree subst))
      (if inexact?
        (->inexact res)
        res)))

(module+ test
  (check-equal? ((tree->procedure '(* x (op1 x y)) 'x 'y) 'a 'b)
                '(* a (op1 a b))))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Tentative substitute with shadowing in let*
;;; Does not work if a binding appears twice in the same let*, e.g.,
;; (let* ([a 5] [b (+ a 2)] [a 6]) (+ a b))

#;
(begin
  (require "letstar.rkt") ; cycle
  (define (subst u bindings)
    (define h (make-hash))
    (for ([b (in-list bindings)])
      (hash-set! h (car b) (cadr b)))
    (let loop ([u u])
      (cond
        [(hash-has-key? h u) (hash-ref h u)]
        [(list? u)
         (match u
           [`(let* ,bindings ,body)
            (let bind-loop ([bindings bindings]
                            [new-bindings '()]
                            [restore-bindings '()])
              (cond
                [(null? bindings)
                 (define new-body (loop body))
                 ; Restore the shadowed bindings
                 (for ([(id btree) (in-list restore-bindings)])
                   (hash-set! h id btree))
                 ; Reconstruct the let* and sort the bindings 
                 (_let* new-bindings new-body)]
                [else
                 (define-values (id btree) (apply values (car bindings)))
                 (define shadowed? (hash-has-key? h id))
                 (when shadowed?
                   (hash-remove! h id))
                 (bind-loop (cdr bindings)
                            (cons (list id (loop btree))
                                  new-bindings)
                            (if shadowed?
                              (cons (cons id btree) restore-bindings)
                              restore-bindings))]))]
           [else (map loop u)])]
        [else u]))))
