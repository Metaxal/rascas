#lang racket/base

(provide substitute
         substitute-this
         substitute-in
         sequential-substitute
         concurrent-substitute)

(require racket/match
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

(define (concurrent-substitute u S)
  (automatic-simplify
   (let ((result (findf (lambda (elt)
                         (equal? u (car elt)))
                       S)))
     (cond ( result (list-ref result 1) )
           ( (list? u)
             (map (lambda (elt)
                    (concurrent-substitute elt S))
                  u) )
           ( else u )))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

