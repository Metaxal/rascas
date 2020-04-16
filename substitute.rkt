#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(provide substitute
         substitute/no-simplify
         substitute-this
         substitute-in
         sequential-substitute
         concurrent-substitute
         recurrent-substitute
         apply//
         tree->procedure)

(require racket/match
         racket/dict
         "automatic-simplify.rkt")

(module+ test
  (require rackunit))


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
           (map loop u)]
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
;; automatic-simplify is triggerred only if any change has been made.
(define (sequential-substitute u L)
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
       (automatic-simplify new-u)
       u)]))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; automatic-simplify is triggerred only if any change has been made and simplify? is #t.
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
     (apply// u (make-hasheq (map cons syms vals)))] ; immutable?
    ;; Substitutions are provided as a dictionary
    ;; THIS DIFFERS FROM concurrent-substitute
    [(u subst-dict)
     (unless (dict? subst-dict)
       (raise-argument-error 'apply// "subst-dict must be a dict?"))
     ;; TODO: Apply automatic-simplify only if a change has been detected?
     (automatic-simplify
      (let loop ([u u])
        (cond [(symbol? u) (dict-ref subst-dict u u)]
              [(list? u)   (map loop u)]
              [else        u])))]))

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

