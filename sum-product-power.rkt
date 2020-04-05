#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(require "misc.rkt"
         "order-relation.rkt"
         racket/match
         (prefix-in rkt: (only-in racket/base + * expt abs /))
         (prefix-in rkt: (only-in racket/math sgn)))

(provide + * ^ / (rename-out [^ expt]) sqr abs sgn
         simplify-sum
         simplify-product
         simplify-power
         simplify-quotient)

(module+ test
  (require rackunit))

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
;; abs
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (abs u)
  (match u
    [`(abs ,a) u]
    [(? number?) (rkt:abs u)]
    [else `(abs ,u)]))

(module+ test
  (check-equal? (abs (abs 'x))
                '(abs x))
  (check-equal? (abs -3)
                3)
  (check-equal? (abs -3.2)
                3.2)
  (check-equal? (abs 'x)
                '(abs x)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sgn
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sgn u)
  (match u
    [(? number?) (rkt:sgn u)]
    [`(sgn ,v) `(sgn , v)]
    [else `(sgn ,u)]))

(module+ test
  (check-equal? (sgn 0) 0)
  (check-equal? (sgn 0.) 0.)
  (check-equal? (sgn 'x) '(sgn x))
  (check-equal? (sgn (sgn 'x)) (sgn 'x))
  (check-equal? (sgn (abs 'x)) (sgn (abs 'x)))) ; 0 or 1.

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ^
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (raise-to expo)
  (lambda (base)
    (^ base expo)))

(define (^ v w)
  (match (list v w)
    [(list 0 0) +nan.0] ; undefined
    [(list 0 _) 0] ; lim_{x->0} 0^x = 0 
    [(list 1 _) 1] ; lim_{x->0} 1^x = 1
    [(list _ 0) 1] ; lim_{x->0} x^0 = 1
    [(list _ 1) v]
    [(list (? exact-number?) (? exact-number?))
     (define z (rkt:expt v w))
     (if (exact? z)
         z ; reducing is fine
         `(^ ,v ,w))] ; don't reduce. Ex: (^ 2 1/2)
    [(list (? number?) (? number?)) ; one is inexact
     (rkt:expt v w)]
    [(list `(abs ,x) (? even-number? w))
     (^ x w)]
    [(list `(^ ,r ,s) w)
     (cond [(even-number? s)
            (^ (abs r) (* s w))]
           [(or (number? s)
                (number? w))
            (^ r (* s w))]
           [else
            `(^ (^ ,r ,s) ,w)
            ])]
    [(list `(* . ,vs)  (? number?)) ; not if w not a number?
     (apply * (map (raise-to w) vs))]
    [else `(^ ,v ,w)]))

(define simplify-power
  (match-lambda
    [`(^ ,u ,v) (^ u v)]))

(module+ test
  (check-equal? (^ 'a 1) 'a)
  (check-equal? (^ 0 0) +nan.0)
  (check-equal? (^ 'a 0) 1) ; WARNING: only for a≠0
  (check-equal? (^ 'a 2) '(^ a 2))
  (check-equal? (^ 'a -4) '(^ a -4))
  (check-equal? (^ (^ 'a 2) 3) '(^ a 6))
  (check-equal? (^ (^ 'a 3) 2) '(^ a 6))
  (check-equal? (^ (^ 'a 3/2) 2) '(^ a 3))
  (check-equal? (^ (^ 'a 2) 3/2) '(^ (abs a) 3))
  (check-equal? (^ (^ 'a 2) 1/2) '(abs a))
  (check-equal? (^ (* 'a 'a) 1/2) '(abs a))
  (check-equal? (^ (^ 'a 1/2) 2) 'a)
  (check-equal? (^ (^ 'x 'a) 2) '(^ x (* 2 a)))
  (check-equal? (^ (^ 'x 2) 'a) '(^ (abs x) (* 2 a)))
  (check-equal? (^ (^ 'x 'a) 'b) '(^ (^ x a) b))
  (check-equal? (^ (^ 'x 'a) (/ 2 'a)) '(^ (^ x a) (* 2 (^ a -1))))
  (check-equal? (* (^ 'x 2/3)
                   (^ 'x 4/3))
                `(^ x 2)))
  
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

(define (sqr x)
  (* x x))

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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; /
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (simplify-quotient u)
  (match u
    [`(/ ,x . ,ys)
     (define y (apply * ys))
     (if (and (number? x) (number? y))
       (rkt:/ x y)
       (* x (^ y -1)))]))

(define /
  (case-lambda
    [(u) (simplify-quotient `(/ 1 ,u))]
    [(u . vs) (simplify-quotient `(/ ,u . ,vs))]))

(module+ test
  (require rackunit)
  #;(displayln "tests...")
  (check-equal? (/ 0. 1.) 0.)
  (check-equal? (/ 0 1.) 0)
  (check-equal? (/ 1. 1.) 1.)
  (check-equal? (/ 1 1.) 1.)
  (check-equal? (/ 4 1) 4)
  (check-equal? (/ 4) 1/4)
  (check-equal? (/ 'a) '(^ a -1))
  (check-equal? (/ 3 4) 3/4)
  (check-equal? (/ 3 'a) '(* 3 (^ a -1)))
  (check-equal? (/ 3 1 2 4) 3/8)
  (check-equal? (/ 'a 2 (* 2 'a)) 1/4)
  (check-equal? (/ 'a 2 'b) '(* 1/2 a (^ b -1))))


