#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(require racket/list
         racket/match
         racket/math)

(provide pi
         try-apply-number
         define-simple-function
         register-function
         symbol->function
         no-fun
         rev-append
         tree-size
         remove+?
         zero-number?
         nan-number?
         infinite-number?
         nonpositive-number?
         nonnegative-number?
         exact-number?
         inexact-number?
         even-number?
         nonempty-list?
         operator-kind
         length<=
         product? quotient? sum? difference? power? factorial? function?
         exp?
         log?
         sin?
         cos?
         tan?
         vars
         base
         exponent
         mod
         while)

(module+ test
  (require rackunit))

(define pi 'pi)

(define (nan-number? x)
  (and (real? x)
       (nan? x)))

(define (zero-number? x)
  (and (real? x)
       (zero? x)))

(define (infinite-number? x)
  (and (real? x)
       (infinite? x)))

(define (nonnegative-number? x)
  (and (number? x)
       (>= x 0)))

(define (nonpositive-number? x)
  (and (number? x)
       (<= x 0)))

(define (exact-number? x)
  (and (number? x)
       (exact? x)))

(define (inexact-number? x)
  (and (number? x)
       (inexact? x)))

(define (even-number? x)
  (and (integer? x) ; may not be exact
       (even? x)))

;=============;
;=== Lists ===;
;=============;

(define (nonempty-list? l)
  ; pair? would do almost the same job though.
  (and (list? l)
       (not (empty? l))))

;; Returns #t if the list l has length at most n.
;; Takes time O(min{length(l), n}), so can be significantly faster
;; than (<= (length l) n) for large l and small n.
(define (length<= l n)
  (cond [(< n 0) #f]
        [(empty? l) #t]
        [else (length<= (rest l) (- n 1))]))

(module+ test
  (check-equal? (length<= '() 0)
                #t)
  (check-equal? (length<= '() -1)
                #f)
  (check-equal? (length<= '(a) 0)
                #f)
  (check-equal? (length<= '(a) 1)
                #t)
  (check-equal? (length<= '(a b) 1)
                #f)
  (check-equal? (length<= '(a b) 2)
                #t)
  (check-equal? (length<= '(a b) 3)
                #t))

(define (rev-append l1 l2)
  (if (null? l1)
    l2
    (rev-append (cdr l1) (cons (car l1) l2))))

(module+ test
  (require rackunit)

  (check-equal? (rev-append '(a b c) '(1 2 3))
                '(c b a 1 2 3)))

;; Number of nodes and leaves in the AST (does not count the cons cells themselves).
;; Useful to compare the size of equivalent expressions.
(define (tree-size tree)
  (cond [(pair? tree)
         (+ (tree-size (car tree))
            (tree-size (cdr tree)))]
        [else 1]))

;; Returns the list l where v has been removed once at most,
;; and whether v was removed at all.
(define (remove+? v lin [=? equal?])
  (let loop ([l lin] [rev-left '()])
    (if (null? l)
      ; not removed
      (values lin #f)
      (let ([x (car l)])
        (if (=? x v)
          (values (rev-append rev-left (cdr l)) #t)
          (loop (cdr l) (cons (car l) rev-left)))))))

(module+ test
  (check-equal?
   (call-with-values
    (λ () (remove+? 'c '(a b c d) eq?))
    list)
   '((a b d) #t))
  (check-equal?
   (call-with-values
    (λ () (remove+? 'e '(a b c d) eq?))
    list)
   '((a b c d) #f))
  (check-equal?
   (call-with-values
    (λ () (remove+? '(e f) '(a b c d (e f)) equal?))
    list)
   '((a b c d) #t)))

(define (operator-kind v)
  (and (pair? v)
       (let ([x (car v)])
         (and (symbol? x)
              x))))

(module+ test
  (check-equal? (operator-kind '(+ a b c)) '+)
  (check-equal? (operator-kind '((+ a b c))) #f)
  (check-equal? (operator-kind '()) #f)
  (check-equal? (operator-kind 'a) #f))

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
    (register-function 'name name)))

(define function-dict (make-hasheq))

;; Register a function symbol.
;; When the symbol is encountered in an s-exp and a function such as
;; automatic-simplify or ->inexact are called, the occurrence of the
;; symbol in prefix position triggers a call to the function fun.
(define (register-function sym fun)
  (if (hash-has-key? function-dict sym)
    (error "function symbol already defined" sym)
    (hash-set! function-dict sym fun)))

(define (symbol->function sym)
  (hash-ref function-dict sym #f))

;; Function that does nothing but cons sym into l
(define ((no-fun sym) . l)
  (cons sym l))

(module+ test
  (check-equal? ((no-fun 'aaa) 'a 'b 'c)
                '(aaa a b c))
  (check-equal? ((no-fun 'aaa))
                '(aaa)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (define opk (operator-kind expr))
  (and opk
       (not (memq opk '(+ - * / ^ !)))))

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
