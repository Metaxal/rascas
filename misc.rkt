#lang racket/base

;;;; This file has been changed from its original dharmatech/mpl version.

(require racket/dict
         racket/list
         racket/match
         racket/math
         syntax/location
         (for-syntax racket/base))

(provide pi
         debug
         debug-expr
         time/line
         ids->assoc
         symbol-format
         try-apply-number
         define-simple-function
         register-function
         symbol->function
         no-fun
         simplify-top
         register-derivatives
         register-derivative
         function-derivatives
         function-derivative
         hash-best+key+value
         rev-append
         tree-size
         ids-occurrences
         remove+?
         remove-all+
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
         while
         in-list+rest)

;; TODO: parameterize (nan) to raise an exception as early as possible
;; instead of silently propagating.

(module+ test (require "rackunit.rkt"))

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

(define (symbol-format fmt . args)
  (string->symbol (apply format fmt args)))

(define-syntax-rule (ids->assoc id ...)
  (list (cons 'id id) ...))

(define-syntax-rule (debug expr ...)
  (begin
    (printf "~a = ~a\n" 'expr expr)
    ...))

(define-syntax-rule (debug-expr expr)
  (let ([res expr])
    (printf "~a = ~a\n" 'expr res)
    res))

(define-syntax (time/line stx)
  (syntax-case stx ()
    [(_ body ...)
     #`(let ()
         (define-values (res cpu real gc)
           (time-apply (λ () body ...) '()))
         ; Print afterwards in case body includes other time/lines.
         (printf "Line: ~a\tFile: ~a\n" (quote-line-number #,stx) (quote-source-file #,stx))
         (printf "cpu time: ~a real time: ~a gc time: ~a\n" cpu real gc)
         (apply values res))]))

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

;; Returns a dictionary of occurrences of the given ids in tree.
;; ids : (listof symbol?)
;; tree : any/c
(define (ids-occurrences ids tree)
  (define h (make-hasheq))
  (for ([id (in-list ids)])
    (hash-set! h id 0))
  (let loop ([tree tree])
    (cond
      [(hash-ref h tree #f)
       (hash-update! h tree add1)]
      [(pair? tree)
       (loop (car tree))
       (loop (cdr tree))]))
  h)

(module+ test
  (check-equal?
   (sort (hash->list (ids-occurrences '(a b z c) '(a (a (a) d a) e (* b (* a c) c))))
         symbol<? #:key car)
   '((a . 5) (b . 1) (c . 2) (z . 0))))

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
  (check-values equal?
                (λ () (remove+? 'c '(a b c d) eq?))
                '((a b d) #t))
  (check-values equal?
                (λ () (remove+? 'e '(a b c d) eq?))
                '((a b c d) #f))
  (check-values equal?
                (λ () (remove+? '(e f) '(a b c d (e f)) equal?))
                '((a b c d) #t)))

;; Returns the list where some of the vs have been removed at most once
;; as well as the remaining vs that could not be removed.
(define (remove-all+ vs l [=? equal?])
  (let loop ([l l] [lres '()] [vs vs] [rms '()])
    (cond
      [(null? l) (values (reverse lres) (reverse rms) vs)]
      [else
       (define x (car l))
       (if (member x vs =?)
         (loop (cdr l) lres (remove x vs =?) (cons x rms))
         (loop (cdr l) (cons x lres) vs rms))])))

(module+ test
  (check-values equal?
                (λ () (remove-all+ '(a b c) '(z a b x a y)))
                '((z x a y) (a b) (c)))
  (check-values equal?
                (λ () (remove-all+ '(a b c) '(z x y)))
                '((z x y) () (a b c)))
  (check-values equal?
                (λ () (remove-all+ '(a b c) '(z a a b x a y c)))
                '((z a x a y) (a b c) ())))

;; TODO: rename key to value to avoid confusion with hash keys.
;; extract-key: hash-key hash-val -> T ; This differs from `best+key+index` for lists
;; better? : T T -> boolean (or any/c)
;; Notice: If the hash is empty, all values are #f. It is the duty of the user
;; to check whether the hash is empty beforehand if these default values are not suitable.
;; The name conflict with `key` is annoying, but it's for consistency
;; with `sort` and best-* variants.
;; So we call the hash key the 'index' instead.
(define (hash-best+key+value h better? #:value [extract-value (λ (key val) val)])
  (for/fold ([best-elt #f]
             [best-key #f]
             [best-val #f])
            ([(x-key x-elt) (in-hash h)]
             [i (in-naturals)])
    (define x-val (extract-value x-key x-elt))
    (if (or (= i 0)
            (better? x-val best-val))
      (values x-elt x-key x-val)
      (values best-elt best-key best-val))))


(module+ test
  
  (let ([h (hash "a" 2 "aa" 3 "abaaa" 3 "d" 5)])
    (check-values equal?
                  (λ () (hash-best+key+value h <))
                  '(2 "a" 2))
    (check-values equal?
                  (λ () (hash-best+key+value h >))
                  '(5 "d" 5))
    (check-values equal?
                  (λ () (hash-best+key+value h > #:value (λ (idx val) (string-length idx))))
                  '(3 "abaaa" 5))))

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

;;; For functions of 1 argument.
;;; A function registered here allows to be used by automatic-simplify
;;; and ->inexact.
;;; Functions like cos, sqr, gamma fit the bill.
(define-syntax-rule (define-simple-function name rkt)
  (begin
    (define (name v)
      (or (try-apply-number rkt v) ; First, try if racket reduces it.
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

;; Function that does nothing but cons sym into l.
;; Used in distribute and factor.
(define ((no-fun sym) . l)
  (cons sym l))

(module+ test
  (check-equal? ((no-fun 'aaa) 'a 'b 'c)
                '(aaa a b c))
  (check-equal? ((no-fun 'aaa))
                '(aaa)))

;; Applies to top level operator to the rest of the tree and returns the result.
(define (simplify-top tree)
  (define op (operator-kind tree))
  (define fun (and op (symbol->function op)))
  (if fun
    (apply fun (rest tree))
    tree))

(module+ test
  #; ; doesn't work without arithmetic, but circular deps...
  (check-equal? (simplify-top `(+ 3 4 (* 'a (+ 3 4))))
                '(+ 7 (* 'a (+ 3 4)))))

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

;; TODO: Rename to times?
(define (product? expr)
  (eq? '* (operator-kind expr)))

(define (quotient? expr)
  (eq? '/ (operator-kind expr)))

;; TODO: Rename to plus?
(define (sum? expr)
  (eq? '+ (operator-kind expr)))

(define (difference? expr)
  (eq? '- (operator-kind expr)))

(define (power? expr)
  (eq? '^ (operator-kind expr)))

(define (factorial? expr)
  (eq? '! (operator-kind expr)))

(define (exp? expr)
  (eq? 'exp (operator-kind expr)))

(define (log? expr)
  (eq? 'log (operator-kind expr)))

(define (sin? expr)
  (eq? 'sin (operator-kind expr)))

(define (cos? expr)
  (eq? 'cos (operator-kind expr)))

(define (tan? expr)
  (eq? 'tan (operator-kind expr)))

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

(define-syntax-rule (vars name ...)
  (begin (define name 'name)
         ...))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (while test body ...)
  (let loop ()
    (when test
      body
      ...
      (loop))))

;; This is as efficient as a named let.
(define-sequence-syntax in-list+rest
  (lambda () #'in-list+rest/proc)
  (lambda (stx)
    (syntax-case stx ()
      [[(x r) (_ lst)]
       #'[(x r)
          (:do-in
           ([(l) lst])
           (unless (list? l)
             (raise-argument-error 'in-list+rest "list?" l))
           ([l l])
           (not (null? l))
           ([(x r) (values (car l) (cdr l))])
           #true
           #true
           [r])]]
      [_ #f])))

(define (in-list+rest/proc l)
  (for/list ([(x r) (in-list+rest l)]) (list x r)))

(module+ test
  (check-equal? (for/list ([(x r) (in-list+rest '(a b c))])
                  (cons x r))
                '((a b c) (b c) (c)))
  (check-equal? (for*/list ([(x r) (in-list+rest '(a b c))]
                            [y (in-list r)])
                  (list x y))
                '((a b) (a c) (b c))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Derivatives dict
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Dictionary and functions to register and obtain the
;;; per-argument derivatives of a function.

(define diff-dict (make-hasheq))

(define (register-derivatives fsym dfs)
  (unless (list? dfs)
    (raise-argument-error 'register-derivatives fsym "list?" dfs))
  (define fun (symbol->function fsym))
  (when fun
    (unless (procedure-arity-includes? fun (length dfs) #t)
      (raise-argument-error
       'register-derivatives fsym
       "Same number of derivatives as the number of arguments of the corresponding function."
       (length dfs))))
  (when (dict-has-key? diff-dict fsym)
    (error "derivative already defined for " fsym))
  (dict-set! diff-dict fsym dfs))

(define (register-derivative fsym df)
  (when (dict-has-key? diff-dict fsym)
    (error "derivative already defined for " fsym))
  (dict-set! diff-dict fsym (list df)))

(define (function-derivatives fsym)
  (dict-ref diff-dict fsym #f))

(define (function-derivative fsym)
  (car (dict-ref diff-dict fsym '(#f))))

;; Ex:
#;(register-derivative 'log (λ (x) (/ x)))
#;(begin (define (my-fun a b) ...)
         (register-function 'my-fun my-fun)
         (register-derivatives
          'my-fun
          (list
           ; derivative of my-fun for the first argument
           (λ (a b) ...)
           ; derivative of my-fun for the second argument
           (λ (a b) ... ))))

