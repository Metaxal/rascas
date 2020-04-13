#lang racket/base
(require "misc.rkt"
         "arithmetic.rkt"
         "substitute.rkt"
         racket/list
         racket/match
         )

(provide _let*
         contract-let*
         expand-let*)

;; Recursively replaces the most used sub-expression (syntactic check) in tree
;; with a let* binding and returns the result.
;; Returns the original expression if no sub-expression occurs at least twice.
;; This is probably not the most efficient implementation.
;; TODO: Check sublists of sums and products (and other variadic commutative operators)
;;   Maybe proceed by growing pairs (then contract)
;;   (Factor lifts one by one)
(define (contract-let* tree
                        #:prefix [prefix (gensym)]
                        #:first-idx [first-idx 0])

  (let lift-loop ([tree tree] [idx first-idx])
  (define h (make-hash))
  ;; Count the number of occurrences of each list (really, subtree).
  (let loop ([tree tree])
    (when (list? tree)
      (hash-update! h tree add1 0)
      (map loop tree)))

  ;; Remove the lists that appear only once and sort the rest
  ;; according to tree-size reduction
  (define counts
    (sort (filter (λ (p) (> (cdr p) 1))
                  (hash->list h))
          > 
          #:key
          (λ (p)
            ; By how much can the tree be reduced
            ; (the -1 is because it must still appear once).
            (* (- (cdr p) 1)
               (tree-size (car p))))))
  
  ;; Replace the most used list with a binding.
  (cond
    [(or (empty? counts)
         (= 1 (cdr (first counts))))
     ; Can't reduce anything.
     tree]
    [else
     (define id (string->symbol (format "~a~a" prefix idx)))
     (define ex1 (caar counts))
     (define new-dict
       (map (λ (p)
              (cons (substitute (car p) ex1 id)
                    (cdr p)))
            (rest counts)))

     (define sub-tree (lift-loop (substitute tree ex1 id) (+ idx 1)))
     (match sub-tree
       [`(let* ,(list bindings ...) ,expr )
        `(let* ([,id ,ex1] . ,bindings)
           ,expr)]
       [else
        `(let* ([,id ,ex1])
           ,sub-tree)])])))

;; The purpose of this 'function' is to remove the binding pairs when the
;; value is an atom (and not an expression), since it would take no more room
;; (in terms of tree size). Also, this ensures that the body can be reduced
;; if possible.
;; Naming it `let*` would badly interact with Racket, because `_let*` doesn't
;; behave exactly like `let*`. The list of bindings are not ids, but expressions
;; so one must write like (_let* `([a ,(+ 2 'b)]) ...).
;; NOTICE: _let* triggers `automatic-simplify`, which recurs through the tree.
(define _let*
  (match-lambda*
    [`( (,bindings ...) ,body)
     ; Recursively reduce the bindins if possible.
     (define-values (subst new-let-bindings)
       (for/fold ([subst '()] ; substitution list (not an assoc)
                  [new-let-bindings '()]) ; new list of binding to keep in let*
                 ([bind (in-list bindings)])
         (define new-val (concurrent-substitute (cadr bind) subst))
         (define new-bind (list (car bind) new-val))
         (if (pair? new-val)
           (values subst (cons new-bind new-let-bindings))
           (values (cons new-bind subst) new-let-bindings))))
     (define new-body (concurrent-substitute body subst))
     (if (null? new-let-bindings)
       new-body
       `(let* ,new-let-bindings ,new-body))]
    [other (error "_let* bad form" other)]))
(register-function 'let* _let*)

(define (expand-let* u)
  (match u
    [`(let* (,bindings ...) ,body) ; ps are pairs
     ;; Recursively replace the ids with their values
     (define subst
       (for/fold ([subst '()]) ; substitution list (not an assoc)
                 ([bind (in-list bindings)])
         (define new-val (concurrent-substitute (cadr bind) subst))
         (cons (list (car bind) new-val) subst)))
     (concurrent-substitute body subst)]
    [other (error "_let* bad form" other)]))

(module+ test
  (require rackunit
           "trig-functions.rkt")
  (check-equal? (_let* '() (+ 'a 2)) (+ 'a 2))
  (check-equal? (_let* '([a 5]) (+ 'a 2)) 7)
  (check-equal? (_let* '([a 5] [b a] [c (+ b a)]) (+ 'a 'b 'c))
                20)
  (check-equal? (_let* '([a b]) (+ 'a 2)) (+ 'b 2))
  (check-equal? (_let* '([a (+ b 5)]) (+ 'a 2))
                `(let* ([a ,(+ 'b 5)]) ,(+ 'a 2)))
  (check-equal? (contract-let* (+ (log (+ 'x 3))
                                  (exp (+ 'x 3))
                                  (^ (+ 'x 3) 'a))
                               #:prefix '_y)
                `(let* ([_y0 ,(+ 'x 3)])
                   ,(+ (log '_y0) (exp '_y0) (^ '_y0 'a))))
  (check-equal? (expand-let*
                 (contract-let* (+ (log (+ 'x 3))
                                   (exp (+ 'x 3))
                                   (^ (+ 'x 3) 'a))
                                #:prefix '_y))
                '(+ (exp (+ 3 x)) (log (+ 3 x)) (^ (+ 3 x) a)))
  (check-equal?
   (contract-let* '(*
                    2
                    (^ a 2)
                    (cos (exp (cos (* (^ a 2) (^ (+ b x) 2)))))
                    (exp (cos (* (^ a 2) (^ (+ b x) 2))))
                    (^ (sin (exp (cos (* (^ a 2) (^ (+ b x) 2))))) -1)
                    (sin (log (sin (exp (cos (* (^ a 2) (^ (+ b x) 2)))))))
                    (sin (* (^ a 2) (^ (+ b x) 2)))
                    (+ b x))
                  #:prefix '_y)
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
     (+ b x))))


;; TODO: derivative through let*
;;  either expand, derivate, contract, or
;;  derivate sub-expressions with the binding.
;; d/dx (let* ([a <a(x)>] [b <b(x)>] ...) <f(a, b, ... x)>)
;; is syntactically df/dx + da/dx df/da + db/dx df/db...
;; https://en.wikipedia.org/wiki/Chain_rule#Multivariable_case
;; TODO: autodiff(?)
;;  (Can we do batch optimization with autodiff?)
;; https://stats.stackexchange.com/questions/224140/step-by-step-example-of-reverse-mode-automatic-differentiation

(module+ drracket
  (require "derivative.rkt")
  (define f '(cos (exp (sin (log (+ (^ x 2) (* 3 y) z))))))

  (define xp1
    (list (derivative f 'x)
          (derivative f 'y)
          (derivative f 'z)))

  (define lifted-xp1 (contract-let* xp1 #:prefix 'X_))

  lifted-xp1

  (list (tree-size xp1)
        '---reduced-to--->
        (tree-size lifted-xp1))

  )

(module+ drracket
  (require "derivative.rkt"
           "trig-functions.rkt")
  (newline)
  (derivative (cos (log (sin (exp (cos (sqr (* (+ 'x 'b) 'a)))))))
              'x)
  #;'(*
      2
      (^ a 2)
      (cos (exp (cos (* (^ a 2) (^ (+ b x) 2)))))
      (exp (cos (* (^ a 2) (^ (+ b x) 2))))
      (^ (sin (exp (cos (* (^ a 2) (^ (+ b x) 2))))) -1)
      (sin
       (log (sin (exp (cos (* (^ a 2) (^ (+ b x) 2)))))))
      (sin (* (^ a 2) (^ (+ b x) 2)))
      (+ b x))
  (newline)
  (contract-let*
   (derivative (cos (log (sin (exp (cos (sqr (* (+ 'x 'b) 'a)))))))
               'x)
   #:prefix '_y)
  #;  
  '(let* ((_y0 (* (^ a 2) (^ (+ b x) 2)))
          (_y1 (exp (cos _y0)))
          (_y2 (sin _y1)))
     (*
      2
      _y1
      (^ _y2 -1)
      (^ a 2)
      (cos _y1)
      (sin _y0)
      (sin (log _y2))
      (+ b x))))
