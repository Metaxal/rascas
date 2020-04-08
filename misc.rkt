#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(require racket/match)

(provide pi
         try-apply-number
         define-simple-function
         register-simple-function symbol->function
         exact-number? inexact-number? even-number?
         product? quotient? sum? difference? power? factorial? function?
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

(define pi 'pi)

(define (exact-number? x)
  (and (number? x)
       (exact? x)))

(define (inexact-number? x)
  (and (number? x)
       (inexact? x)))

(define (even-number? x)
  (and (integer? x)
       (even? x)))


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

;; Useful to define functions:
;; If f is a racket function, try to apply f to x with result r.
;; If both x and r are exact, or if x is inexact, return r,
;; otherwise return #f. (inexactness is contagious.)
(define (try-apply-number f x)
  (and (number? x)
    (let ([r (f x)])
      (and (or (inexact? x)
               (exact? r))
           r))))

(module+ test
  (check-equal? (try-apply-number + 1) 1)
  (check-equal? (try-apply-number + 1.) 1.)
  (check-equal? (try-apply-number exp 0) 1)
  (check-equal? (try-apply-number exp 1) #f)
  (check-equal? (try-apply-number + 'x) #f)
  (check-equal? (try-apply-number + '(+ 3 2)) #f))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; For functions of 1 argument
;;; A function registered here allows to be used by automatic-simplify
;;; and ->inexact.
;;; Functions like cos, sqr, gamma fit the bill.

(define-syntax-rule (define-simple-function name rkt)
  (begin
    (define (name v)
      (or (try-apply-number rkt v)
          `(name ,v)))
    (register-simple-function 'name name)))

(define function-dict (make-hasheq))

(define (register-simple-function sym fun)
  (if (hash-has-key? function-dict sym)
    (error "function symbol already defined" sym)
    (hash-set! function-dict sym fun)))

(define (symbol->function sym)
  (hash-ref function-dict sym #f))


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

(define (base u)
  (match u
    [`(^ ,x ,y) x]
    [else u]))

(define (exponent u)
  (match u
    [`(^ ,x ,y) y]
    [else       1]))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax vars
  (syntax-rules ()
    ((vars name ...)
     (begin (define name 'name)
            ...))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (while test body ...)
  (let loop ()
    (when test
      body
      ...
      (loop))))
