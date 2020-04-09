#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(provide sin)

(require (rename-in (only-in racket/base sin) (sin rkt:sin))
         racket/match
         "arithmetic.rkt"
         "numerator.rkt"
         "denominator.rkt"
         "misc.rkt")

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (simplify-sin-first-quadrant a/b)

  (cond ( (> a/b 2)

          (sin (* (mod a/b 2) pi)) )

        ( (> a/b 1)

          (- (sin (- (* a/b pi) pi))) )

        ( (> a/b 1/2)

          (sin (* (- 1 a/b) pi)) )

        ( else `(sin ,(* a/b pi)) )))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (simplify-sin-k/n*pi k/n)

  (let ((k (numerator   k/n))
        (n (denominator k/n)))

    (case n

      ((1) 0)

      ((2) (case (mod k 4)
             ((1)  1)
             ((3) -1)))

      ((3) (case (mod k 6)
             ((1 2) (/ (sqrt 3) 2))
             ((4 5) (- (/ (sqrt 3) 2)))))

      ((4) (case (mod k 8)
             ((1 3) (/ 1 (sqrt 2)))
             ((5 7) (- (/ 1 (sqrt 2))))))

      ((6) (case (mod k 12)
             ((1 5)   1/2)
             ((7 11) -1/2))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (n*pi? elt)
  (match elt
    [`(* ,(? (λ(n)(and (number? n)
                       (exact? n)
                       (>= (abs n) 2)))
             n)
         pi)
      #t]
    [else #f]))

(define (simplify-sum-with-pi elts)

  (let ((pi-elt (findf n*pi? elts)))

    (let ((n (list-ref pi-elt 1)))

      (sin (+ (- (apply + elts) pi-elt)
              (* (mod n 2) pi))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (n/2*pi? elt)
  (define (n/2? x)
    (and (number? x)
         (exact? x)
         (equal? (denominator x) 2)))
  (match elt
    [`(* ,(? n/2?) pi) #t]
    [else              #f]))

(define (simplify-sin-sum-with-n/2*pi elts)
  (let ((n/2*pi (findf n/2*pi? elts)))
    (let ((other-elts (- (apply + elts) n/2*pi)))
      (let ((n (numerator (list-ref n/2*pi 1))))
        (case (mod n 4)
          ((1) `(cos ,other-elts))
          ((3) (- `(cos ,other-elts))))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sin u)
  (or
   (try-apply-number rkt:sin u)
   (match u
     ['pi 0]

     [(? (λ(n)(and (number? n)
                   (negative? n)))
         n)
      (- (sin (* -1 n)))]

     [`(* ,(? (λ(n)(and (number? n)
                        (negative? n)))
              n) . ,elts)
      (- (sin (apply * (- n) elts)))]

     [`(* ,(? (λ(a/b) (and (number? a/b)
                           (exact? a/b)
                           (> a/b 1/2)))
              a/b) pi) 
      (simplify-sin-first-quadrant a/b)]

     [`(* ,(? (λ(k/n)(and (member (denominator k/n) '(1 2 3 4 6))
                          (integer? (numerator k/n))))
              k/n) pi)
      (simplify-sin-k/n*pi k/n)]

     [`(+ . ,(? (λ(elts)(findf n*pi? elts))
                elts))
      (simplify-sum-with-pi elts)]

     [`(+ . ,(? (λ(elts)(findf n/2*pi? elts))
                elts))
      (simplify-sin-sum-with-n/2*pi elts)]

     [else `(sin ,u)])))
(register-function 'sin sin)
