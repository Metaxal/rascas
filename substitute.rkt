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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define (sequential-substitute u L)
  (automatic-simplify
   (match L
     ['() u]
     [`((,t ,r) . ,reste)
      (sequential-substitute (substitute u t r)
                             reste)])))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: to speed up, specialized variant that just substitutes
;; symbols with eq?, and sort the symbols(not sure it's worth it)
(define (concurrent-substitute u S [=? equal?])
  (automatic-simplify
   (let loop ([u u])
     (cond [(assoc u S =?) => cadr]
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
    [(u subst-dict)
     (unless (dict? subst-dict)
       (raise-argument-error 'apply// "subst-dict must be a dict?"))
     (automatic-simplify
      (let loop ([u u])
        (cond [(symbol? u) (dict-ref subst-dict u u)]
              [(list? u)   (map loop u)]
              [else        u])))]))

(module+ test
  (require rackunit
           "arithmetic.rkt")
  (check-equal? (substitute (* 'x (+ 1 (/ (sqr 'x)))) 'x 0)
                +nan.0)
  (check-equal? (substitute (* 'x (+ 1 (/ (sqr 'x)))) 'x 0.)
                +nan.0)
  (check-equal? (substitute (* 'x (+ 1 (/ (sqr 'x)))) 'x +inf.0)
                +inf.0)
  (check-equal? (substitute (* 'x (+ 1 (/ (sqr 'x)))) 'x -inf.0)
                -inf.0))

