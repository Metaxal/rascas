#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(require "order-relation.rkt"
         racket/match
         (prefix-in rkt: (only-in racket/base + *)))

(provide + * ^
         simplify-sum
         simplify-product
         simplify-power)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (list-or-null-if-0 x)
  (if (equal? x 0)
      '()
      (list x)))
  
(define (list-or-null-if-1 x)
  (if (equal? x 1)
      '()
      (list x)))

(define (any-are-zero? l)
  (ormap (λ(x)(and (number? x)
                   (zero? x))) l))

(define ((equal-to x) y)
  (equal? x y))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ^
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (raise-to expo)
  (lambda (base)
    (^ base expo)))

(define (^ v w)
  (match (list v w)
    [(list 0 _) 0]
    [(list 1 _) 1]
    [(list _ 0) 1]
    [(list _ 1) v]
    [(list (? number?) (? integer?)) (expt v w)]
    [(list `(^ ,r ,s)  (? integer?)) (^ r (* s w))]
    [(list `(* . ,vs)  (? integer?)) (apply * (map (raise-to w) vs))]
    [else  `(^ ,v ,w)]))

(define (simplify-power u)
  (^ (list-ref u 1)
     (list-ref u 2)))
  
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; *
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (merge-products p-elts q-elts)
  (match (list p-elts q-elts)
    [(list '() x) x]
    [(list x '()) x]
    [`((,p . ,ps) (,q . ,qs))
     (match (simplify-product-rec (list p q))
       ['()
        (merge-products ps qs)]
       [(list x)
        (cons x (merge-products ps qs))]
       [(? (equal-to (list p q)))
        (cons p (merge-products ps q-elts))]
       [(? (equal-to (list q p)))
        (cons q (merge-products p-elts qs))])]))

(define (simplify-product-rec elts)
  (match elts
    [(list `(* . ,p-elts) `(* . ,q-elts))
     (merge-products p-elts q-elts)]
    [`((* . ,p-elts) ,q)
     (merge-products p-elts (list q))]
    [`(,p (* . ,q-elts))
     (merge-products (list p) q-elts)]
    [(list (? number? p) (? number? q))
     (list-or-null-if-1 (rkt:* p q))]
    [(list 1 x) (list x)]
    [(list x 1) (list x)]
    [(list p q) (cond ((equal? (base p) (base q))
                       (list-or-null-if-1
                        (^ (base p)
                           (+ (exponent p)
                              (exponent q)))))

                      ((order-relation q p) (list q p))

                      (else (list p q)))]
    [`((* . ,ps) . ,qs)
     (merge-products ps (simplify-product-rec qs))]
    [`(,x . ,xs)
     (merge-products (list x) (simplify-product-rec xs))]))

(define (simplify-product u)
  (match u
    [`(* ,x) x]
    [`(* . ,(? any-are-zero?)) 0]
    [`(* . ,elts)
     (match (simplify-product-rec elts)
       ['() 1]
       [(list x) x]
       [xs `(* ,@xs)])]))

(define (* . elts)
  (simplify-product `(* ,@elts)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; +
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (merge-sums p-elts q-elts)
  (match (list p-elts q-elts)
    [`(() ,x) x]
    [`(,x ()) x]
    [`((,p . ,ps) (,q . ,qs))
     (match (simplify-sum-rec (list p q))
       ['()                       (merge-sums ps qs)]
       [(list x)                  (cons x (merge-sums ps qs))]
       [(? (equal-to (list p q))) (cons p (merge-sums ps q-elts))]
       [(? (equal-to (list q p))) (cons q (merge-sums p-elts qs))])]))

(define (simplify-sum-rec elts)
  (match elts
    [`((+ . ,p-elts) (+ . ,q-elts))     (merge-sums p-elts q-elts)]
    [`((+ . ,p-elts) ,q)                (merge-sums p-elts (list q))]
    [`(,p (+ . ,q-elts))                (merge-sums (list p) q-elts)]
    [(list (? number? p) (? number? q)) (list-or-null-if-0 (rkt:+ p q))]
    [`(0 ,x) (list x)]
    [`(,x 0) (list x)]
    [(list p q)
     (cond ((equal? (term p) (term q))
            (list-or-null-if-0
             (* (term p)
                (+ (const p)
                   (const q)))))

           ((order-relation q p)
            (list q p))

           (else (list p q)))]
    [`((+ . ,ps) . ,qs) (merge-sums ps       (simplify-sum-rec qs))]
    [`(,x        . ,xs) (merge-sums (list x) (simplify-sum-rec xs))]))

(define (simplify-sum u)
  (match u
    [`(+ ,x) x]
    [`(+ . ,elts)
     (match (simplify-sum-rec elts)
       ['() 0]
       [(list x) x]
       [xs `(+ ,@xs)])]))

(define (+ . elts)
  (simplify-sum `(+ ,@elts)))
