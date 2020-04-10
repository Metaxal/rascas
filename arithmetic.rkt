#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(require "misc.rkt"
         "order-relation.rkt"
         racket/match
         (only-in racket/math nan? infinite?)
         (prefix-in rkt: (only-in racket/base + * expt abs / exp sqrt log))
         (prefix-in rkt: (only-in racket/math sgn))
         (prefix-in rkt: (only-in math/number-theory factorial))
         racket/list)

(provide + - * ^ / (rename-out [^ expt]) sqr sqrt abs sgn
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

(module+ test
  (check-equal? (abs (abs 'x))
                '(abs x))
  (check-equal? (abs (abs (abs 'x)))
                '(abs x))
  (check-equal? (abs -3)
                3)
  (check-equal? (abs (* 'x 'x))
                '(^ x 2))
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
    [`(sgn ,v) (sgn v)] ; remove one level and try again
    [else `(sgn ,u)]))
(register-function 'sgn sgn)

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
  #;(check-equal? (* (sgn 'x) 'x) (abs 'x)) ; not checked for now
  )

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

(module+ test
  ; Checked with maxima for the harder cases
  (check-equal? (^ 'a 1) 'a)
  (check-equal? (^ 0 0) +nan.0)
  (check-equal? (^ 0 -0.2) +nan.0)
  (check-equal? (^ 0 -2) +nan.0)
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
                `(^ x 2))
  (check-equal? (^ (exp 'u) 'v) '(exp (* u v))))


(define (expand-power u n)
  (if (sum? u)
      (let ((f (list-ref u 1)))
        (let ( (r (- u f)) )
          (let loop ( (s 0)
                      (k 0) )
            (if (> k n)
                s
                (let ((c (/ (! n)
                            (* (! k)
                               (! (- n k))))))
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
  (check-not-equal? (sqrt (* 'a 'a)) 'a)
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

;; Returns one of the absorbing elements 0, 0.0, -0.0, +nan.0, +inf.0, -inf.0.
;; +nan.0 is contagious,
;; zero and infinite make +nan.0,
;; otherwise we follow racket's defaults.
;; Racket's default is that 0*+inf.0 = 0, which I consider a suprising and risky result.
;; Although I understand the logic, I prefer to return +nan.0 for safety.
(define (absorb-product elts)
  (let loop ([elts elts] [res #f])
    (if (empty? elts)
      res
      (let ([x (first elts)])
        (cond [(not (real? x)) ; +inf.0 and +nan.0 are reals(!)
               (loop (rest elts) res)]
              [(nan? x)
               +nan.0]
              [(and (not (infinite? x)) (not (zero? x)))
               (loop (rest elts) res)]
              [(not res)
               (loop (rest elts) x)]
              ; override some of racket's defaults
              [(and (infinite? res) (zero? x))
               +nan.0]
              [(and (zero? res) (infinite? x))
               +nan.0]
              [else
               ; otherwise use racket's default
               (define new-res (if res (rkt:* res x) x))
               (if (nan? new-res)
                 +nan.0
                 (loop (rest elts) new-res))])))))

(module+ test
  (check-equal? (absorb-product '(1 2))
                #f)
  (check-equal? (absorb-product '())
                #f)
  (check-equal? (absorb-product '(a b c))
                #f)
  (check-equal? (absorb-product '(a b 0))
                0)
  (check-equal? (absorb-product '(a +inf.0 b 0))
                +nan.0)
  (check-equal? (absorb-product '(a b 0. 0 c))
                0)
  (check-equal? (absorb-product '(a b 0. -0.0 -0.0 0 c))
                0)
  (check-equal? (absorb-product '(a b 0. -0.0 -0.0 c))
                0.)
  (check-equal? (absorb-product '(a b 0. -0.0 c))
                -0.0)
  (check-equal? (absorb-product '(a b 0. 0 -0.0 c +inf.0))
                +nan.0)
  (check-equal? (absorb-product '(a -inf.0 b -inf.0 c))
                +inf.0)
  (check-equal? (absorb-product '(a -inf.0 b +inf.0 c))
                -inf.0)
  (check-equal? (absorb-product '(a 0 b +inf.0 c))
                +nan.0)
  (check-equal? (absorb-product '(a -0.0 b -inf.0 c))
                +nan.0)
  )

(define (* . elts)
  (match elts
    ['() 1]
    [(list x) x]
    [else
     (cond
       [(absorb-product elts)] ; return nan, 0, 0., +inf.0 or -inf.0 or skip to next clause.
       [else
        (match (simplify-product-rec elts)
          ['() 1]
          [(list x) x]
          [xs `(* . ,xs)])])]))
(register-function '* *)

(define (sqr x)
  (^ x 2))
(register-function 'sqr sqr)

(define (expand-product r s)
  (cond ( (sum? r)
          (let ((f (list-ref r 1)))
            (+ (expand-product f s)
               (expand-product (- r f) s))) )
        ( (sum? s) (expand-product s r) )
        ( else (* r s) )))

(module+ test
  (check-equal? (* 0 +inf.0) +nan.0)
  (check-equal? (* 1 +nan.0) +nan.0)
  (check-equal? (* 1 +nan.0) +nan.0)
  )

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

(define (+ . elts)
  (match elts
    ['() 0]
    [(list x) x]
    [else
     (match (simplify-sum-rec elts)
       ['() 0]
       [(list x) x]
       [xs `(+ . ,xs)])]))
(register-function '+ +)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (- . elts)
  (match elts
    [(list x)    (* -1 x)]
    [`(,x . ,ys) (+ x (* -1 (apply + ys)))]))

(module+ test
  (check-equal? (- 1 2 3) -4)
  (check-equal? (- 5) -5)
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
  (check-equal? (/ 'a 2 'b) '(* 1/2 a (^ b -1))))

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
         (* w (log v))]
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
