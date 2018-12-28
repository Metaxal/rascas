#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(require racket/match)

(provide product? quotient? sum? difference? power? factorial? function?
         exp?
         log?
         sin?
         cos?
         tan?
         vars
         base
         exponent
         inexact-number?
         mod
         while)

(module+ test
  (require rackunit))

;; Racket's `modulo' takes only integers and behaves differently.
(define (mod x n)
  (define nn (abs n))
  (define z (- x (* nn (floor (/ x nn)))))
  (if (< z 0)
      (+ z n)
      z))

(module+ test
  ;; Testing against Chez Scheme 9.5
  (check-equal? (mod 2 2) 0)
  (check-equal? (mod 2815 17) 10)
  (check-equal? (mod 13 3) 1)
  (check-equal? (mod 17/5 2) 7/5)
  (check-equal? (mod -17/5 2) 3/5)
  (check-equal? (mod -153/7 5) 22/7)
  (check-equal? (mod -153/7 -3) 15/7)
  (check-equal? (mod -153/7 3) 15/7)
  (check-equal? (mod 153/7 -3) 6/7)
  (check-equal? (mod 153/7 -2/5) 9/35)
  (check-equal? (mod -253/7 -12/13) 71/91)
  (check-equal? (mod -253/7 -27/13) 113/91)
  )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (product? expr)
  (match expr
    [`(* . ,elts) #t]
    [else         #f]))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (quotient? expr)
  (match expr
    [`(/ . ,elts) #t]
    [else         #f]))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sum? expr)
  (match expr
    [`(+ . ,elts) #t]
    [else         #f]))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (difference? expr)
  (match expr
    [`(- . ,elts) #t]
    [else        #f]))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (power? expr)
  (match expr
    [`(^ ,x ,y) #t]
    [else       #f]))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (factorial? expr)
  (match expr
    [`(! ,n) #t]
    [else   #f]))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (exp? expr)
  (match expr
    [`(exp ,n) #t]
    [else      #f]))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (log? expr)
  (match expr
    [`(log ,n) #t]
    [else      #f]))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sin? expr)
  (and (pair? expr)
       (eq? (car expr) 'sin)))

(define (cos? expr)
  (and (pair? expr)
       (eq? (car expr) 'cos)))

(define (tan? expr)
  (and (pair? expr)
       (eq? (car expr) 'tan)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (function? expr)
  (and (list? expr)
       (> (length expr) 1)
       (symbol? (car expr))
       (not (member (car expr)
                    '(+ - * / ^ !)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (base expr)
  (if (power? expr)
      (list-ref expr 1)
      expr))

(define (exponent expr)
  (if (power? expr)
      (list-ref expr 2)
      1))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax vars
  (syntax-rules ()
    ((vars name ...)
     (begin (define name 'name)
            ...))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (inexact-number? x)
  (and (number? x)
       (inexact? x)))

(define-syntax-rule (while test body ...)
  (let loop ()
    (when test
      body
      ...
      (loop))))
