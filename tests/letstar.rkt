#lang racket/base

(require rascas/rackunit
         rascas/arithmetic
         rascas/letstar
         rascas/automatic-simplify
         rascas/trig-functions
         rascas/derivative
         rascas/special-functions)

;; TODO: use unbound-ids to check for missing ids

(check-equal?
 (_let* `((__s_436 (f __s_435))
          (__s_435 x))
        '__s_436)
 '(f x))

;; b is used twice, but by bindings that are not used.
;; Not sure if we should cover this case here. Applying automatic-simplify
;; works anyway.
#;
(check-equal?
 (_let* `([b (+ a 3)]
          [c (+ b 3)]
          [e (+ b 3)])
        (* (+ 3 'b)))
 (+ 6 'b))

;; Shadowing.
(check-equal?
 (automatic-simplify
  '(let* ([c (+ b 3)])
     (+ 3
        (let* ([b (exp x)])
          c))))
 '(+ 6 b))

;; Subtree more frequent
(check-equal?
 (rebind-all-let*
  (contract-let*
   (* (+ 'b (* 2 'a (+ 3 'd)))
      (+ 'c (* 2 'a (+ 3 'd)))
      (+ 3 'd)))
  '_s)
 '(let* ((_s0 (+ 3 d))
         (_s1 (* 2 _s0 a)))
    (* _s0 (+ _s1 b) (+ _s1 c))))

(check-equal?
 (unbound-ids
  (contract-let*
   ;; id b is going to get removed because (exp a) is lifted to top-level let*
   ;; which means it becomes unbound in sublet*.
   ;; Indeed we are lifting an id out of its context.
   ;; Solution: Start by lifting all let*?
   '(+ (let* ([a (+ 5 x)]
              [b (exp a)])
         (+ a (exp a) (exp b)))
       (let* ([c (+ 5 x)]
              [d (exp c)])
         (+ c (sqr c) (sqr d))))))
 '(x))

;; TODO: Make sure than any id has an index that is at least as high
;; as any of its dependencies +1.
(check-equal?
 (remove '(e 5)
         (sort-bindings
          '([a (+ b c)]
            [d (* b c)]
            [e 5] ; causes problems with `sort` as it doesn't compare with b
            [b (* c (exp c))])))
 ; Make sure that b is placed before a and d.
 '((b (* c (exp c))) (a (+ b c)) (d (* b c))))

(check-equal?
 (rebind-all-let*
  '(let* ([a z] [b (+ 2 a)] [c (* b 3)])
     (+ a b c z x))
  '_a)
 '(let* ([_a0 z] [_a1 (+ 2 _a0)] [_a2 (* _a1 3)])
    (+ _a0 _a1 _a2 z x)))

(check-equal?
 (variadic-replace '* '(_xx12 _xx14) 'ID
                   '(* 0.5 _xx12 _xx14 (+ (* _xx13 _xx9) _xx12 _xx14 (* 2 _xx12 _xx14 _xx9))))
 '(* 0.5 ID (+ (* _xx13 _xx9) _xx12 _xx14 (* 2 ID _xx9))))
  
(check-equal?
 (rebind-all-let*
  (contract-let* '(op3 (op1 (op2 a b)
                            (op2 a b))
                       (op1 (op2 a b)
                            (op2 a b))))
  '_w)
 '(let* ((_w0 (op2 a b))
         (_w1 (op1 _w0 _w0)))
    (op3 _w1 _w1)))

(check-equal? (_let* `([a x] [b y] [c (* z z)])
                     '(+ a b c))
              '(+ x y (^ z 2)))
(check-equal? (_let* `([a x] [b y] [c ,(* 'z 'z)])
                     (* 'c (+ 'a 'b 'c)))
              '(let* ((c (^ z 2))) (* c (+ c x y))))
(check-equal? (_let* '() (+ 'a 2)) (+ 'a 2))
(check-equal? (_let* '([a 5]) (+ 'a 2)) 7)
(check-equal? (_let* '([a 5] [b a] [c (+ b a)]) (+ 'a 'b 'c))
              20)
(check-equal? (_let* '([a b]) (+ 'a 2)) (+ 'b 2))
(check-equal? (_let* '([a (+ b 5)]) (+ 'a 2))
              (+ 'b 7))

; nested let
(check-equal? (_let* `([a 5])
                     (+ 3
                        (_let* `([b 6])
                               (* 'a 'b))
                        (_let* `([c 10])
                               (+ 'a 'c))))
              48)


  
(check-equal? (rebind-all-let*
               (contract-let* (+ (log (+ 'x 3))
                                 (exp (+ 'x 3))
                                 (^ (+ 'x 3) 'a)))
               '_y)
              `(let* ([_y0 ,(+ 'x 3)])
                 ,(+ (log '_y0) (exp '_y0) (^ '_y0 'a))))
(check-equal? (expand-let*
               (contract-let* (+ (log (+ 'x 3))
                                 (exp (+ 'x 3))
                                 (^ (+ 'x 3) 'a))))
              '(+ (exp (+ 3 x)) (log (+ 3 x)) (^ (+ 3 x) a)))

(check
 member
 (rebind-all-let*
  (contract-let*
   '(*
     2
     (^ a 2)
     (cos (exp (cos (* (^ a 2) (^ (+ b x) 2)))))
     (exp (cos (* (^ a 2) (^ (+ b x) 2))))
     (^ (sin (exp (cos (* (^ a 2) (^ (+ b x) 2))))) -1)
     (sin (log (sin (exp (cos (* (^ a 2) (^ (+ b x) 2)))))))
     (sin (* (^ a 2) (^ (+ b x) 2)))
     (+ b x)))
  '_y)
 '[(let* ((_y0 (+ b x))
          (_y1 (^ a 2))
          (_y2 (* (^ _y0 2) _y1))
          (_y3 (exp (cos _y2)))
          (_y4 (sin _y3)))
     (* 2 _y0 _y1 _y3 (^ _y4 -1) (cos _y3) (sin _y2) (sin (log _y4))))
   (let* ((_y0 (^ a 2))
          (_y1 (+ b x))
          (_y2 (* _y0 (^ _y1 2)))
          (_y3 (exp (cos _y2)))
          (_y4 (sin _y3)))
     (* 2 _y0 _y1 _y3 (^ _y4 -1) (cos _y3) (sin _y2) (sin (log _y4))))])

(check-equal?
 (expand-let*
  `(let* ((_y0 ,(* (^ 'a 2) (^ (+ 'b 'x) 2)))
          (_y1 ,(exp (cos '_y0)))
          (_y2 ,(sin '_y1)))
     (*
      2
      _y1
      (^ _y2 -1)
      (^ a 2)
      (cos _y1)
      (sin _y0)
      (sin (log _y2))
      (+ b x))))
 '(*
   2
   (^ a 2)
   (cos (exp (cos (* (^ a 2) (^ (+ b x) 2)))))
   (exp (cos (* (^ a 2) (^ (+ b x) 2))))
   (^ (sin (exp (cos (* (^ a 2) (^ (+ b x) 2))))) -1)
   (sin (log (sin (exp (cos (* (^ a 2) (^ (+ b x) 2)))))))
   (sin (* (^ a 2) (^ (+ b x) 2)))
   (+ b x)))

;; Two sublets with identical ids
(check-equal?
 (rebind-all-let*
  (contract-let*
   (+
    (_let* `([a (+ 2 x)]
             [b (* 2 a)])
           (+ 'a (log 'a) 'b (log 'b)))
    (_let* `([a (+ 2 x)]
             [b (* 3 a)])
           (* 'b (log 'b) (+ 'a (cos 'a))))))
  '_a)
 '(let* ((_a0 (+ 2 x))
         (_a1 (* 2 _a0))
         (_a2 (* 3 _a0)))
    (+ _a1 _a0 (* _a2 (+ _a0 (cos _a0)) (log _a2)) (log _a1) (log _a0))))

(check-equal?
 (rebind-all-let*
  (contract-let*
   (+
    (_let* `([a (+ 2 x)]
             [b (* 2 a)])
           (+ 'a (log 'a) 'b (log 'b)))
    (_let* `([a (+ 2 x)]
             [b (* 2 a)])
           (* 'b (log 'b) (+ 'a (cos 'a))))))
  '_a)
 '(let* ((_a0 (+ 2 x))
         (_a1 (* 2 _a0))
         (_a2 (log _a1)))
    (+ _a0 _a1 _a2 (* _a1 _a2 (+ _a0 (cos _a0))) (log _a0))))

(check-equal?
 (rebind-all-let*
  (contract-let*
   '(let* ((_a0 (+ 2 x))
           (_a1 (* 2 _a0))
           (_a2 (+ 2 x))
           (_a3 (* 2 _a2)))
      (+ _a2 _a3 (* _a1 (+ _a0 (cos _a0)) (log _a1)) (log _a2) (log _a3))))
  '_b)
 '(let* ((_b0 (+ 2 x))
         (_b1 (* 2 _b0))
         (_b2 (log _b1)))
    (+ _b0 _b1 _b2 (* _b1 _b2 (+ _b0 (cos _b0))) (log _b0))))
  
;; When extracting from the bindings,
;; the new binding must be above, not after.
(check-equal?
 (rebind-all-let*
  (contract-let*
   (_let* `([c ,(* (+ 'a 'b) (exp (+ 'a 'b)))])
          (* 'c (log 'c))))
  '_y)
 '(let* ((_y0 (+ a b))
         (_y1 (* _y0 (exp _y0))))
    (* _y1 (log _y1))))

;; The new binding must be in the middle of the list(!)
(check-equal?
 (rebind-all-let*
  (contract-let*
   (_let* `([a ,(* 'b (exp 'b))]
            [c ,(* (^ 'a (+ 'a 'b)) (exp (^ 'a (+ 'a 'b))))])
          (* 'c (log 'c))))
   '_y)
 '(let* ((_y0 (* b (exp b)))
         (_y1 (^ _y0 (+ _y0 b)))
         (_y2 (* _y1 (exp _y1))))
    (* _y2 (log _y2))))

(check-equal?
 (expand-let*
  (contract-let*/ascovars
   '(list (* a b c d e t) (* a b c d e x) (* a b c d e y) (* a b c d e z)
          (+ a b c d e t) (+ a b c d e x) (+ a b c d e y) (+ a b c d e z))))
 '(list
   (* a b c d e t)
   (* a b c d e x)
   (* a b c d e y)
   (* a b c d e z)
   (+ a b c d e t)
   (+ a b c d e x)
   (+ a b c d e y)
   (+ a b c d e z)))

;; Nothing to simplify.
(check-equal?
 (contract-let*/ascovars '(* (* a b) (* a b)))
 '(* (^ a 2) (^ b 2)))

(check-equal?
 (rebind-all-let*
  (contract-let*/ascovars
   '(let* ([a (* 2 aa)])
      (op1 (* a b c d e t) (* a b c d e x))))
  '_w)
 '(let* ((_w0 (* 2 aa b c d e)))
    (op1 (* _w0 t) (* _w0 x))))


(check-not-exn
 (λ ()
   (contract-let*
    '(list
      (let* ((_w_out_1_0
              (* 0.5 (+ (* _w_0_0_0 a) (* _w_0_0_1 b) (* _w_0_0_2 c) (* _w_0_0_3 d) (* _w_0_0_4 e)) (+ 1 (sgn (+ (* _w_0_0_0 a) (* _w_0_0_1 b) (* _w_0_0_2 c) (* _w_0_0_3 d) (* _w_0_0_4 e)))))))
        (*
         0.5
         (list
          (+
           (* 2.0 _w_1_0_0 _w_2_0_0 'a 'b)
           (* 2.0 _w_1_1_0 _w_2_0_1 'c 'd)))
         (+
          (* 2 b (dirac (+ (* _w_0_0_0 a) (* _w_0_0_1 b) (* _w_0_0_2 c) (* _w_0_0_3 d) (* _w_0_0_4 e))) (+ (* _w_0_0_0 a) (* _w_0_0_1 b) (* _w_0_0_2 c) (* _w_0_0_3 d) (* _w_0_0_4 e)))
          (* b (+ 1 (sgn (+ (* _w_0_0_0 a) (* _w_0_0_1 b) (* _w_0_0_2 c) (* _w_0_0_3 d) (* _w_0_0_4 e))))))))
      (let* ((_w_out_1_0
              (* 0.5 (+ (* _w_0_0_0 a) (* _w_0_0_1 b) (* _w_0_0_2 c) (* _w_0_0_3 d) (* _w_0_0_4 e)) (+ 1 (sgn (+ (* _w_0_0_0 a) (* _w_0_0_1 b) (* _w_0_0_2 c) (* _w_0_0_3 d) (* _w_0_0_4 e)))))))
        (*
         0.5
         (list
          (+
           (* 2.0 _w_1_0_0 _w_2_0_0 'a 'b)
           (* 2.0 _w_1_1_0 _w_2_0_1 'c 'd)))
         (+
          (* 2 c (dirac (+ (* _w_0_0_0 a) (* _w_0_0_1 b) (* _w_0_0_2 c) (* _w_0_0_3 d) (* _w_0_0_4 e))) (+ (* _w_0_0_0 a) (* _w_0_0_1 b) (* _w_0_0_2 c) (* _w_0_0_3 d) (* _w_0_0_4 e)))
          (* c (+ 1 (sgn (+ (* _w_0_0_0 a) (* _w_0_0_1 b) (* _w_0_0_2 c) (* _w_0_0_3 d) (* _w_0_0_4 e)))))))))
    )))

;; The problem here is that contract-let* will find ([c (+ a b)]) as
;; something to compress, which is not right.
;; we should look only inside expressions
(check-not-exn
 (λ () (contract-let*
        (+
         (_let* `([c ,(+ 'a 'b)])
                (* 'c (log 'c)))
         (_let* `([c ,(+ 'a 'b)])
                (* 'c 'd (log 'c)))))))

;; Duplicate identifier.
(check-exn
 exn:fail?
 (λ () (_let* `([a (+ f h)] [b (+ e f)] [a (+ d e)])
              (* (+ 'a 'b) (+ 1 'a 'b)))))
  
;; Duplicate identifier, but not currently checked.
#;(check-exn
   exn:fail?
   (_let* `([a 5] [b 2] [a 6]) (+ 'a 'b)))
#;(check-exn
   exn:fail?
   (_let* `([a 5] [b 'e] [a 'd])
          (* (+ 'a 'b) (+ 1 'a 'b))))

;; This doesn't work because (derivative '__s_0 'x) gets reduced to 0...
#;
(check-equal?
 (expand-let*
  (contract-let* '(+
                  (* (derivative (derivative (psi0 x) x) x) (gamma x))
                  (* 3 (derivative (psi0 x) x) (gamma x) (psi0 x))
                  (* (gamma x) (^ (psi0 x) 3)))))
 '(+
   (* (derivative (derivative (psi0 x) x) x) (gamma x))
   (* 3 (derivative (psi0 x) x) (gamma x) (psi0 x))
   (* (gamma x) (^ (psi0 x) 3))))