#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(require "misc.rkt"
         "order-relation.rkt"
         racket/match
         (only-in racket/math nan? infinite?)
         (prefix-in rkt: (only-in racket/base + * expt abs / exp sqrt log))
         (prefix-in rkt: (only-in racket/math sgn))
         (prefix-in rkt: (only-in math/number-theory factorial binomial))
         racket/list
         (for-syntax racket/base))

(provide + - * ^ / (rename-out [^ expt]) sqr sqrt abs sgn dirac
         exp log ! (rename-out [! factorial])
         expand-main-op
         expand-exp
         expand-power
         expand-product
         contract-exp)

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
;; DiracDelta
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dirac u)
  (match u
    [(? number?) (if (zero? u) 1 0)]
    [else `(dirac ,u)]))
(register-function 'dirac dirac)
(register-derivative 'dirac (λ (x) 0))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sgn
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sgn u)
  (match u
    [(? number?) (rkt:sgn u)]
    [`(sgn ,v) (sgn v)] ; remove one level and try again
    [else `(sgn ,u)]))
(register-function 'sgn sgn)
(register-derivative 'sgn (λ (x) (* 2 (dirac x)))) ; by lim_{h->0} (f(x+h)-f(x-h))/(2h)

(module+ test
  (check-equal? (sgn 0) 0)
  (check-equal? (sgn 0.) 0.)
  (check-equal? (sgn 3) 1)
  (check-equal? (sgn 3.) 1.)
  (check-equal? (sgn -3) -1)
  (check-equal? (sgn 'x) '(sgn x))
  (check-equal? (sgn (sgn 'x)) (sgn 'x))
  (check-equal? (sgn (sgn (sgn 'x))) (sgn 'x))
  (check-equal? (sgn (abs 'x)) (sgn (abs 'x))) ; 0 or 1 (DiracDelta function)
  (check-equal? (sgn +inf.0) 1.)
  (check-equal? (sgn -inf.0) -1.)
  (check-equal? (sgn +nan.0) +nan.0)
  (check-equal? (sgn -nan.0) +nan.0)
  #;(check-equal? (* (sgn 'x) 'x) (abs 'x)) ; not checked for now
  )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; abs
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (abs u)
  (match u
    [`(abs ,a) u]
    [`(^ ,a ,(? even-number? b))
     `(^ ,a ,b)]
    [(? number?) (rkt:abs u)]
    [else `(abs ,u)]))
(register-function 'abs abs)
(register-derivative 'abs sgn)

(module+ test
  (check-equal? (abs (abs 'x))
                '(abs x))
  (check-equal? (abs (abs (abs 'x)))
                '(abs x))
  (check-equal? (abs -3)
                3)
  (check-equal? (abs (* 'x 'x))
                '(^ x 2))
  (check-equal? (abs -3.2) 3.2)
  (check-equal? (abs +inf.0) +inf.0)
  (check-equal? (abs -inf.0) +inf.0)
  (check-equal? (abs 'x)
                '(abs x)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ^
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (raise-to expo)
  (lambda (base)
    (^ base expo)))

(define (^ v w)
  (if (or (nan-number? v) (nan-number? w))
    +nan.0
    (match (list v w)
      [(list 0 (? nonpositive-number?)) +nan.0] ; undefined
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
      [(list `(sgn ,x) (? even-number? w))
       1]
      [(list `(exp ,a) w)
       (exp (* a w))]
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
      [else `(^ ,v ,w)])))

(register-function '^ ^)
(register-function 'expt ^)
(register-derivatives ; plural
 '^
 ; One derivative per argument
 (list
  (λ (v w) (* w (^ v (- w 1))))
  (λ (v w) (* (log v) (^ v w)))))

(module+ test
  ; Checked with maxima for the harder cases
  (check-equal? (^ 0 0) +nan.0)
  (check-equal? (^ +nan.0 0) +nan.0) ; overrides Racket's default
  (check-equal? (^ 0 +nan.0) +nan.0)
  (check-equal? (^ 0 -0.2) +nan.0)
  (check-equal? (^ 0 -2) +nan.0)
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
                `(^ x 2))
  (check-equal? (^ (exp 'u) 'v) '(exp (* u v)))

  ;; Numeric over symbolic
  (check-equal? (^ 'a 1) 'a)
  (check-equal? (^ 'a 0) 1) ; because 0^0 is nan
  (check-equal? (^ 0 'a) 0)
  )


(define (expand-power u n)
  (if (sum? u)
      (let ((f (list-ref u 1)))
        (let ( (r (- u f)) )
          (let loop ( (s 0)
                      (k 0) )
            (if (> k n)
                s
              (let ([c (rkt:binomial n k)])
                  (loop (+ s 
                           (expand-product (* c (^ f (- n k)))
                                           (expand-power r k)))
                        (+ k 1)))))))
      (^ u n)))

(define (sqrt x)
  (or (try-apply-number rkt:sqrt x)
      (^ x 1/2)))
(register-function 'sqrt sqrt)

(module+ test
  (require rackunit)
  (check-equal? (sqrt 2) '(^ 2 1/2))
  (check-equal? (sqrt 'a) '(^ a 1/2))
  (check-equal? (sqrt (* 'a 'a)) (abs 'a))
  (check-equal? (sqrt 4) 2)
  (check-true (number? (sqrt 2.))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ! (factorial)
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (! n)
  (if (number? n)
    (rkt:factorial n)
    `(! ,n)))
(register-function '! !)
(register-function 'factorial !)
  
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; *
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Assumes that subexpressions (powers in particular) have already been reduced.
(define (* . elts)
  (let/ec return ; +nan.0 bypass
    ; Hash containing the sum of the exponents as a list (may be symbolic).
    ; The sum is applied at the end.
    (define counts (make-hash))
    ; Product of the (real) numbers, initially maintained as a list so as
    ; to apply the absorption loop at the end (instead of earlier).
    (define nums '())
    ;; Count how many elements of each kind, merging products and looking inside powers.
    (let loop ([l elts])
      (unless (null? l)
        (define x (car l))
        (cond
          [(eqv? x +nan.0) (return +nan.0)] ; fully absorbing
          [(number? x)
           (set! nums (cons x nums))
           (loop (cdr l))]
          [(product? x)
           ; Merge products.
           (loop (cdr x))
           (loop (cdr l))]
          [(power? x)
           (define ba (base x))
           (define ex (exponent x))
           (hash-update! counts ba (λ (exps) (cons ex exps)) '())
           (loop (cdr l))]
          [else
           (hash-update! counts x (λ (exps) (cons 1 exps)) '())
           (loop (cdr l))])))
    ; End of loop, return a sorted and compact list.
    (define lres
      (for/fold ([lres '()]
                 #:result (sort lres order-relation))
                ([(x exps) (in-hash counts)])
        (define pow (^ x (apply + exps)))
        (cond
          [(eqv? +nan.0 pow) (return +nan.0)]
          [(number? pow)
           (set! nums (cons pow nums))
           lres]
          [else (cons pow lres)])))
    ; Return value.
    (let ([tot-nums (apply rkt:* nums)])
      (cond
        [(or (null? lres) ; Single element (number), remove '*
             (eqv? +nan.0 tot-nums) ; +nan.0 is contagious
             ; Numeric over symbolic:
             (eqv? 0 tot-nums)) ; Excludes 0.0 and -0.0
         tot-nums]
        [(and (null? (cdr lres))
              (= 1 tot-nums))
         ; Single element (not number), remove '*.
         (car lres)]
        [(= 1 tot-nums)
         (cons '* lres)]
        [else
         `(* ,tot-nums . ,lres)]))))
(register-function '* *)

(module+ test
  (check-equal? (* 2 3) 6)
  (check-equal? (* 2 'x) '(* 2 x))
  (check-equal? (* 'x) 'x)
  (check-equal? (* 'x 'x) '(^ x 2))
  (check-equal? (* (* 'a 'b) (* 'b 'c) 'a)
                '(* (^ a 2) (^ b 2) c))

  (check-equal? (* -0.0 0.0) -0.0)
  (check-equal? (* 1 +inf.0) +inf.0)
  (check-equal? (* -1 +inf.0) -inf.0)
  (check-equal? (* 0 +inf.0) 0)

  ;; +nan.0 is contagious.
  (check-equal? (* 0. +inf.0) +nan.0)  
  (check-equal? (* 0. +nan.0) +nan.0)
  (check-equal? (* +inf.0 -inf.0) -inf.0)
  (check-equal? (* +inf.0 +inf.0) +inf.0)
  (check-equal? (* 1 +nan.0) +nan.0)
  (check-equal? (* 1 +nan.0) +nan.0)
   ; Overrides Racket's default, but consistent with NSpire and Wolfram Alpha.
  (check-equal? (* 0 +nan.0) +nan.0)
  
   ;; Numeric over symbolic.
  (check-equal? (* 0 'x) 0)
  ; These can't be reduced because if the sign of x matters.
  (check-equal? (* 0.0 'x) '(* 0.0 x))
  (check-equal? (* -0.0 'x) '(* -0.0 x))
  (check-equal? (* +inf.0 'x) '(* +inf.0 x))
  (check-equal? (* -inf.0 'x) '(* -inf.0 x))
  )

(define (sqr x)
  (^ x 2))
(register-function 'sqr sqr)
; Should we register a derivative too?
; Shouldn't be necessary if the function is applied first.

(module+ test
  (check-equal? (sqr (sqrt 'x)) 'x)
  (check-equal? (sqrt (sqr 'x)) '(abs x)))

(define (expand-product r s)
  (cond ( (sum? r)
          (let ((f (list-ref r 1)))
            (+ (expand-product f s)
               (expand-product (- r f) s))) )
        ( (sum? s) (expand-product s r) )
        ( else (* r s) )))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; +
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Assumes that the (always at most single) number in a product is always the first element.
(define (+ . l)
  (let/ec return
    (define counts (make-hash))
    (define tot-nums 0)
    ; Count how many elements of each kind, merging sums and looking inside products.
    (let loop ([l l])
      (unless (null? l)
        (define x (car l))
        (cond
          [(eqv? +nan.0 x) (return +nan.0)] ; nan is contagious
          [(number? x)
           (set! tot-nums (rkt:+ tot-nums x))
           (loop (cdr l))]
          [(sum? x)
           ; Merge sums.
           (loop (cdr x))
           (loop (cdr l))]
          [(product? x)
           (define args (cdr x))
           (match args
             ['() (set! tot-nums (rkt:+ 1 tot-nums))] ; should not happen though.
             [`(,(? number? n) ,y)
              (hash-update! counts y (λ (m) (rkt:+ n m)) 0)]
             [`(,(? number? n) . ,rs)
              (hash-update! counts `(* . ,rs) (λ (m) (rkt:+ n m)) 0)]
             [else
              (hash-update! counts x add1 0)])
           (loop (cdr l))]
          [else
           (hash-update! counts x add1 0)
           (loop (cdr l))])))
    ; End of loop, return a sorted and compact list.
    (define lres
      (for/fold ([lres '()]
                 #:result (sort lres order-relation))
                ([(x n) (in-hash counts)])
        (cond
          [(= 0 n) lres]
          [(= 1 n) (cons x lres)]
          [else (cons (* n x) lres)])))
    ; Return value.
    (cond [(or (null? lres) ; no non-numeric element, remove '+ and return number
               (= tot-nums +inf.0)
               (= tot-nums -inf.0)
               (eqv? tot-nums +nan.0))
           tot-nums]
          [(and (null? (cdr lres))
                (= 0 tot-nums))
           ; Single non-numeric element after removing 0, remove '+ .
           (car lres)]
          [(= 0 tot-nums)
           (cons '+ lres)]
          [else
           `(+ ,tot-nums . ,lres)])))
(register-function '+ +)

(module+ test
  (check-equal? (+ 3 4 5)
                12)
  (check-equal? (+ 'x 'x)
                '(* 2 x))
  (check-equal? (+ 'a 3 'a (exp 'x) '4 (* 3 'a) (exp 'x))
                '(+ 7 (* 5 a) (* 2 (exp x))))
  (check-equal? (+ (* 'a 'x) 'b)
                '(+ b (* a x)))

  ;; Numeric over symbolic
  (check-equal? (+ +inf.0 'x) +inf.0) ; because x=-inf.0 is invalid
  (check-equal? (+ -inf.0 'x) -inf.0)

  ; +nan.0 is contagious
  (check-equal? (+ +nan.0 'x) +nan.0) ; nan is contagious
  (check-equal? (+ +inf.0 'x -inf.0) +nan.0)

  )

;; TODO: for/sum by for/fold/derived

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (- . elts)
  (match elts
    [(list x)    (* -1 x)]
    [`(,x . ,ys) (+ x (* -1 (apply + ys)))]))

(module+ test
  ;; - defers to +, so numeric over symbolic should be respected automatically.
  (check-equal? (- 1 2 3) -4)
  (check-equal? (- 5) -5)
  (check-equal? (- +inf.0) -inf.0)
  (check-equal? (- +inf.0 +inf.0) +nan.0)
  (check-equal? (- 'x) (* -1 'x))
  (check-equal? (- 'x 'y 'z) (+ 'x (* -1 (+ 'y 'z)))))
(register-function '- -)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; /
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define /
  (case-lambda
    [(u) (/ 1 u)]
    [(x . ys)
     (define y (apply * ys))
     (if (and (number? x) (number? y))
       (if (and (zero? x) (zero? y))
         +nan.0 ; override racket's default
         (rkt:/ x y))
       (* x (^ y -1)))]))
(register-function '/ /)

(module+ test
  (require rackunit)
  (check-equal? (/ 0 0.) +nan.0)
  (check-equal? (/ 0. 0) +nan.0)
  (check-equal? (/ -0. 0.) +nan.0)
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
  (check-equal? (/ 'a 2 'b) '(* 1/2 a (^ b -1)))

  ;; Numeric over symbolic
  (check-equal? (/ 0 'x) 0)
  (check-equal? (/ 0.0 'x) '(* 0.0 (^ x -1))) ; cannot reduce
  )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; exp
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (exp u)
  (or
   (try-apply-number rkt:exp u)
   (match u
     [`(log ,v) v]
     ; todo: what if product or a sum with a log in the middle?
     [else `(exp ,u)])))
(register-function 'exp exp)
(register-derivative 'exp exp)

(module+ test
  (check-equal? (exp 0) 1)
  (check-equal? (exp (log 'x)) 'x)
  (check-equal? (exp (log 1.1)) 1.1))


(define (expand-exp-rules u)
  (match u
    [`(+ ,v)
     (expand-exp-rules v)]
    [`(+ ,v . ,w)
     (* (expand-exp-rules v)
        (expand-exp-rules (cons '+ w)))]
    [`(* ,v)
     (expand-exp-rules v)]
    [`(* ,v . ,w)
     #:when (integer? v)
     (^ (expand-exp-rules (cons '* w)) v)]
    [else
     (exp u)]))

(define (expand-exp u)
  (if (list? u)
    (match (map expand-exp u)
      [`(exp ,v)
       (expand-exp-rules v)]
      [v v])
    u))

(define (contract-exp-rules u)
  (match (expand-main-op u)
    [`(^ (exp ,a) ,s)
     (define p (* a s))
     (if (or (product? p)
             (power? p))
       (exp (contract-exp-rules p))
       (exp p))]
    [`(* . ,vs)
     (define-values (vs-exp vs-other)
       (partition exp? vs))
     (apply *
            (exp (apply + (map second vs-exp)))
            vs-other)]
    [`(+ . ,vs)
     (apply + (map contract-exp-rules vs))]
    [else u]))

(define (contract-exp u)
  (if (list? u)
    (let ((v (map contract-exp u)))
      (if (or (product? v)
              (power?   v))
        (contract-exp-rules v)
        v))
    u))



(define (expand-main-op u)
  (match u
    [`(* ,a . ,rest)
     (expand-product a
                     (expand-main-op (apply * rest)))]
    [`(^ ,a ,b)
     (expand-power a b)]
    [else u]))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; log
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define log
  (case-lambda
    [(u)
     (or
      (try-apply-number rkt:log u)
      (match u
        [`(* . ,vs) (apply + (map log vs))]
        [`(exp ,v) v]
        [`(^ ,v ,w)
         ; That's what maxima does, but this is a little incorrect (see tests)
         ; since it reduces the domain of v to positive numbers if w is even.
         (* w (log v))
         #;(* w (log (abs v)))]
        #;[`(gamma ,v)
           ; This can't work, because (gamma v) is reduced even before
           ; the log has a chance to catch it.
           ; also, ideally, this should be defined with gamma in special-functions.rkt
           (or (try-apply-number log-gamma v)
               `(log (gamma ,v)))]
        [else `(log ,u)]))]
    [(u v)
     (/ (log u) (log v))]))
(register-function 'log log)
(register-derivative 'log (λ (x) (/ 1 x)))

(module+ test
  (require rackunit)
  (check-equal? (log 1) 0)
  (check-equal? (log 0.) -inf.0)
  (check-equal? (log +inf.0) +inf.0)
  (check-equal? (log 2) '(log 2))
  (check-equal? (log 2 2) 1)
  (check-equal? (log (exp 2)) 2)
  (check-equal? (log (exp 'x)) 'x)
  (check-equal? (log (^ 2 'x) 2) 'x)
  (check-equal? (log (^ 'a 'x) 'a) 'x)
  (check-equal? (log (* 3 'x)) (+ (log 3) (log 'x)))

  ;; +nan.0
  (check-equal? (log +inf.0 +nan.0) +nan.0)
  (check-equal? (log +inf.0 +inf.0) +nan.0)

  ;; Numeric over symbolic
  (check-equal? (log +inf.0 'x) '(* +inf.0 (^ (log x) -1))) ; (log x) could be negative
  ; (we could actually remove the ^-1. This could be done in sgn)

  ; Some annoying cases:
  ; (log (sqr x)) is defined for all x, but not (* 2 (log x))
  ; So should we write:
  #;(check-equal? (log (sqr 'x)) (* 2 (log (abs 'x))))
  ; then what about
  #;(substitute (log (^ 'x 'a)) 'a 2)
  ; but we also cannot write
  ;(log (^ x a)) -> (* a (log (abs x)))
  ; Actually, maxima does not simplify when defining functions:
  #|
(%i49) f(x,a) := log(x^a);
                                              a
(%o49)                        f(x, a) := log(x )
(%i50) f(-2, 2);
(%o50)                              log(4)
|#
  )

