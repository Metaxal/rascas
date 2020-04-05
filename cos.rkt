#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(provide cos)

(require  (prefix-in rkt: (only-in racket/base cos))
          racket/match
          "arithmetic.rkt"
          "numerator.rkt"
          "denominator.rkt"
          "misc.rkt"
          )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pi 'pi)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (simplify-cos-first-quadrant a/b)

  (cond ( (> a/b 2)   (cos (* (mod a/b 2) pi))    )

        ( (> a/b 1)   (- (cos (- (* a/b pi) pi))) )

        ( (> a/b 1/2) (- (cos (- pi (* a/b pi)))) )

        ( else       `(cos ,(* a/b pi))           )))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (simplify-cos-k/n*pi k/n)

  (let ((k (numerator   k/n))
        (n (denominator k/n)))

    (case n

      ((1) (case (mod k 2)
             ((1) -1)
             ((0)  1)))

      ((2) (case (mod k 2)
             ((1) 0)))

      ((3) (case (mod k 6)
             ((1 5)  1/2)
             ((2 4) -1/2)))

      ((4) (case (mod k 8)
             ((1 7)    (/ 1 (sqrt 2)))
             ((3 5) (- (/ 1 (sqrt 2))))))

      ((6) (case (mod k 12)
             ((1 11)    (/ (sqrt 3) 2))
             ((5 7)  (- (/ (sqrt 3) 2))))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (n*pi? elt)
  (define (n? x)
    (and (number? x)
         (exact? x)
         (>= (abs x) 2)))
  (match elt
    [`(* ,(? n?) pi) #t]
    [else            #f]))

(define (simplify-cos-sum-with-pi elts)
  (let ((pi-elt (findf n*pi? elts)))
    (let ((n (list-ref pi-elt 1)))
      (cos (+ (- (apply + elts) pi-elt)
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

(define (simplify-cos-sum-with-n/2*pi elts)
  (let ((n/2*pi (findf n/2*pi? elts)))
    (let ((other-elts (- (apply + elts) n/2*pi)))
      (let ((n (numerator (list-ref n/2*pi 1))))
        (case (mod n 4)
          ((1) (- `(sin ,other-elts)))
          ((3) `(sin ,other-elts)))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (simplify-cos u)

  (match u

    ['(cos 0) 1]

    ['(cos pi) -1]

    [`(cos ,(? inexact-number? n)) (rkt:cos n)]

    [`(cos ,(? (λ(n)(and (number? n)
                         (negative? n)))
               n))
     (cos (- n))]

    [`(cos (* ,(? (λ(n)(and (number? n)
                            (negative? n)))
                  n)
              . ,elts))
     (cos (apply * (append (list -1 n) elts)))]

    [`(cos (* ,(? (λ(a/b)(and (number? a/b)
                              (exact? a/b)
                              (> a/b 1/2)))
                  a/b)
              pi))
     (simplify-cos-first-quadrant a/b)]

    [`(cos (* ,(? (λ(k/n)
                    (and (member (denominator k/n) '(1 2 3 4 6))
                         (integer? (numerator k/n))))
                  k/n)
              pi))
     (simplify-cos-k/n*pi k/n)]

    [`(cos (+ . ,(? (λ(elts)(findf n*pi? elts))
                    elts)))
     (simplify-cos-sum-with-pi elts)]

    [`(cos (+ . ,(? (λ(elts)(findf n/2*pi? elts))
                    elts)))
     (simplify-cos-sum-with-n/2*pi elts)]

    [else u]))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cos x)
  (simplify-cos `(cos ,x)))

