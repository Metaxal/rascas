#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(provide substitute
         substitute-this
         substitute-in
         sequential-substitute
         concurrent-substitute
         apply//)

(require racket/match
         racket/dict
         "automatic-simplify.rkt")

(module+ test
  (require rackunit))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Apply automatic-simplify only if a change has been detected?
;; BETTER: simplify along the branch of the change only.
(define (substitute u t r)
  (automatic-simplify
   (cond ((equal? u t) r)
         ((list? u)
          (map (substitute-this t r) u))
         (else u))))

(define (substitute-this t r)
  (lambda (u)
    (substitute u t r)))

(define (substitute-in u)
  (lambda (t r)
    (substitute u t r)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Apply automatic-simplify only if a change has been detected?
(define (sequential-substitute u L)
  (automatic-simplify
   (match L
     ['() u]
     [`((,t ,r) . ,reste)
      (sequential-substitute (substitute u t r)
                             reste)])))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: Apply automatic-simplify only if a change has been detected?
;; TODO: to speed up, specialized variant that just substitutes
;; TODO: also allow for a hash table.
;; symbols with eq?, and sort the symbols(not sure it's worth it)
;; u : tree
;; S : assoc list, but instead of cons pairs, it takes lists '(id val)
(define (concurrent-substitute u S [=? equal?])
  (automatic-simplify
   (let loop ([u u])
     (cond [(assoc u S =?) => (λ (p) (loop (cadr p)))] ; need to recur 
           [(list? u)         (map loop u)]
           [else              u]))))

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

  ;; Recur through the substitutions too.
  (check-equal?
   (concurrent-substitute '(+ a b) '([a (+ b 2)] [b c]))
   (+ (* 2 'c) 2)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tree->function f . syms)
    (λ l
      (define subst (map list syms l))
      (concurrent-substitute f subst)))

(module+ test
  (check-equal? ((tree->function '(* x (op1 x y)) 'x 'y) 'a 'b)
                '(* a (op1 a b))))

