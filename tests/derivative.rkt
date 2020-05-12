#lang racket/base

(require "../derivative.rkt"
         "../misc.rkt"
         "../algorithmic.rkt"
         "../arithmetic.rkt"
         "../rackunit.rkt"
         "../automatic-simplify.rkt"
         "../distribute.rkt"
         "../substitute.rkt"
         "../trig-functions.rkt"
         "../special-functions.rkt")

;; Tool to test the symbolic derivative value against the numeric one
;; f must be a procedure of 1 argument
(define (check-derivative f [xs (build-list 10 (λ (i) (/ (- (random) .5)
                                                         (random))))])
  (define dfnum (numeric-derivative f))
  (define dfsym (derivative/proc f))
  (for ([v (in-list xs)])
    (check-= (->inexact
              (sqr (- (dfnum v)
                      (dfsym v))))
             0.
             0.001)))
  
(check-equal? (derivative (+ (* 3 (^ 'x 2)) (* 4 'x)) 'x)
              (+ (* 6 'x) 4))
(check-equal? (derivative (/ 3 'x) 'x)
              (/ -3 (sqr 'x)))
(check-equal? (derivative (exp (* 3 'x)) 'x)
              '(* 3 (exp (* 3 x))))

(check-equal? (derivative (log (* 3 'x)) 'x)
              '(^ x -1))

(check-equal? (derivative (sin (* 3 'x)) 'x)
              '(* 3 (cos (* 3 x))))
(check-equal? (derivative (cos (* 3 'x)) 'x)
              '(* -3 (sin (* 3 x))))
(check-equal? (derivative (tan (* 3 'x)) 'x)
              '(* 3 (^ (cos (* 3 x)) -2)))

;; TODO: Check registering a derivative of 3 args
  

; Check that 'derivative is registered as a function
(check-equal? (automatic-simplify '(derivative (sqr x) x))
              (* 2 'x))

;; Register a function /after/ it has been used for derivation.
;; Simplify the expression to obtain the correct derivative.
(let ()
  (define bad-deriv (derivative '(__unknown-deriv (* x 2)) 'x))
  (check-equal? bad-deriv '(derivative (__unknown-deriv (* x 2)) x))
  (register-function '__unknown-deriv (λ (x) (sqr x)))
  (check-equal? (automatic-simplify bad-deriv)
                (* 8 'x))) 

(check-derivative gamma (build-list 10 (λ (i) (* 10 (random)))))


;;; list

(check-equal? (derivative (_list 'x 'y 'x 'z) 'x)
              '(list 1 0 1 0))

(check-equal? (derivative (_list (* 'x 2) (^ 'x 'a) (log 'x)) 'x)
              '(list 2 (* a (^ x (+ -1 a))) (^ x -1)))

(check-equal? (derivative (_list (* 'x 2) (^ 'x 'a) (log 'x)) 'a)
              '(list 0 (* (log x) (^ x a)) 0))

;;; let*
(check-equal? (derivative (_let* `([a ,(+ 'x 2)])
                                 (* (+ 1 'a) (+ 2 'a))) 'x)
              '(+ 3 (* 2 (+ 2 x))))
(check-equal? (derivative (expand-let* (_let* `([a ,(+ 'x 2)])
                                              (* (+ 1 'a) (+ 2 'a)))) 'x)
              '(+ 7 (* 2 x)))
; error: Cannot differentiate for a bound id (a)
(check-exn exn:fail?
           (λ () (derivative (_let* `([a ,(+ 'x 2)]
                                      [b ,(* 'a 2)])
                                    (* (+ 1 'b) (+ 2 'a)))
                             'a)))
(check-equal? (derivative (_let* `([a (+ x 2)])
                                 (* (+ 1 'a) (+ 2 'a))) 'y)
              0)

(check-equal? (distribute-product
               (expand-let*
                (derivative (_let* `([b (* (+ 1 a) (+ 2 a))] [c (* (+ b 1) (+ b 2))] [d c])
                                   (* (+ 'd 1) (+ 'd 2)))
                            'a)))
              (distribute-product
               (derivative (expand-let*
                            (_let* `([b (* (+ 1 a) (+ 2 a))] [c (* (+ b 1) (+ b 2))] [d c])
                                   (* (+ 'd 1) (+ 'd 2))))
                           'a)))

(check-not-exn
 (λ () (derivative
        '(let* ((_u0 (+ (* _w_0_0_0 a) 1))
                (_w_out_1_0 (* 0.5 _u0 (+ 1 (sgn _u0))))
                (_u1 4)
                (_w_out_1_1 (* 0.5 _u1 (+ 1 (sgn _u1))))
                (_w_out_2_0
                 (+ 1.0 (exp (* -1 (+ (* 3 _w_out_1_0) (* 2 _w_out_1_1))))))
                (_w_out_2_1
                 (+ 1.0 (exp (* -1 (+ (* 4 _w_out_1_0) (* 5 _w_out_1_1))))))
                (_u2 (+ (* 3 _w_out_2_0) (* 2 _w_out_2_1))))
           (+ (* 0.5 _u2 _w_3_0_0 (+ 1 (sgn _u2))) (* 0.5 _u2 _w_3_0_1 (+ 1 (sgn _u2)))))
        '_w_0_0_0)))

(check-equal?
 (expand-let*
  (contract-let*
   (derivative
    '(let* ((a (exp x))
            (b (+ 1.0 a))
            (c (+ -1.0 a))
            (d (^ b -2)))
       (* a (+ (^ b -1) (* -1 d c) (* -1 d a) (* (+ (* -1 d) (* 2 (^ b -3) c)) a))))
    'x)))
 (expand-let*
  '(let* ((_g4 (exp x))
          (b (+ 1.0 _g4))
          (c (+ -1.0 _g4))
          (_g0 (^ b -3))
          (_g3 (^ b -2))
          (_g1 (* 2 _g0 c))
          (_g2 (* -1 _g3)))
     (*
      _g4
      (+
       (* (+ _g1 _g2) _g4)
       (* -1 _g3 _g4)
       (* (+ _g1 (* -2 _g3)) _g4)
       (* _g4 (+ _g2 (* 2 _g0 _g4)))
       (^ b -1)
       (* -1 _g3 c)
       (* -2 _g0 _g4 (+ (* -2 _g4) (* -1 c)))
       (* _g4 (+ (* -1 _g3) (* -6 _g4 (^ b -4) c))))))))

(check-derivative
 (λ (x)
   `(let* ((a (exp ,x))
           (b (+ 1.0 a))
           (c (+ -1.0 a))
           (d (^ b -3))
           (e (^ b -2))
           (f (* 2 d c))
           (g (* -1 e)))
      (*
       a
       (+
        (^ b -1)
        (* -1 e c)
        (* -1 e a)
        (* (+ (* -2 e) f) a)
        (* (+ (* -1 e) f) a)
        (* -2 d a (+ (* -1 c) (* -2 a)))
        (* a (+ g (* 2 d a)))
        (* a (+ g (* -6 (^ b -4) c a))))))))


(check-equal? ((derivative/proc sqr) 'a)
              (* 'a 2))
(check-equal? ((derivative/proc sqr) 2)
              4)
(check-equal? ((derivative/proc log) 1/2)
              2)
(check-equal? ((derivative/proc log #:inexact? #t) (sqrt 2))
              (->inexact (/ (sqrt 2))))
  

(check-equal?
 (jacobian (+ (* 'c 'a) (* 'd (^ 'b 2)))
           '(a b))
 '(list c
        (* 2 b d)))

(check-equal?
 (rebind-all-let*
  (jacobian (exp (+ (* 'c 'a) (* 'd (^ 'b 2))))
            '(a b))
  '_s)
 '(let* ((_s0 (exp (+ (* a c) (* (^ b 2) d)))))
    (list (* _s0 c)
          (* 2 _s0 b d))))
  
(check-equal?
 (rebind-all-let*
  (jacobian (sin (exp (+ (* 'c 'a) (* 'd (^ 'b 2)))))
            '(a b))
  '_s)
 '(let* ((_s0 (+ (* a c) (* (^ b 2) d)))
         (_s1 (* (cos (exp _s0)) (exp _s0))))
    (list (* _s1 c) (* 2 _s1 b d))))

;; 'variables' bound in let*
(check-equal?
 (simplify-top
   (jacobian
    `(let* ([a (+ 3 x)]
            [b (+ 2 x a)])
       (+ (* a b)
          (* (log a) (log b))))
    '(a b)))
 '(list 0 0))


;; Compare:
#;(tree-size (jacobian (apply * (build-list 100 (λ (i) (+ i 1 'x))))
            '(x)))
#;(tree-size (jacobian (apply * (build-list 100 (λ (i) (+ i 1 'x))))
            '(x))
             #:log-product? #t)



