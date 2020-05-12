#lang racket/base

(require "misc.rkt"
         racket/list
         racket/dict)

(provide factor-product
         factor)

#;(distribute-product (derivative (beta (+ 'a 'α) (+ 'b 'α)) 'α))


;; Like `factor`, but specialized for '* and '+ (which means the warning in `factor`
;; does not apply) to `factor-product`.
(define (factor-product t)
  (factor t '* '+ #:times-neutral 1))

(define no-neutral (gensym))

;; Recursively factors t if `times` distributes over `plus` and returns the result.
;; May be costly, but not as much as `distribute`.
;; WARNING: This function does *not* check whether the operator `times` actually distributes
;; over the operator `plus`, so this may lead to erroneous output.
;; TODO: I think the current algorithm does far too much work. There must be a nicer way.
;; TODO: We should start by factoring all the lower levels first, then going up. Then a single
;; pass should be sufficient.
(define (factor t times plus
                #:times-neutral [times-neutral no-neutral])
  (define times-fun (or (symbol->function times) (no-fun times)))
  (define plus-fun  (or (symbol->function plus)  (no-fun plus)))
  (define try->times  
    (if (eq? no-neutral times-neutral)
      (λ (u) u)
      ; if u is not a times list, try to make it one (it may fail)
      (λ (u)
        (if (or (eq? times (operator-kind u)))
          u
          (list times times-neutral u))))) ; don't reduce as this mean just become u
  ; Count how many times each element appears in the us 
  (let loop ([t t])
    (let ([t (if (list? t)
               (map loop t)
               t)])
      (cond
        [(eq? plus (operator-kind t))
         (define us (map try->times (rest t)))
         (define h (make-hash))
         (for ([u (in-list us)] [i (in-naturals)])
           (when (eq? times (operator-kind u))
             ;; Count in how many us each element appears (count only once per u).
             (for ([v (in-list (rest u))])
               (hash-update! h v
                             (λ (p) (if (= i (car p)) ; already done for this u
                                      p
                                      (cons i (+ 1 (cdr p)))))
                             ; car = index of last counted u.
                             ; cdr = appears in how many us.
                             '(-1 . 0)))))
         ; find max
         (define-values (best-v best-n)
           (for/fold ([best-v #f]
                      [best-n 0])
                     ([(v p) (in-dict h)])
             (if (and (not (equal? times-neutral v))
                      (or (not best-v)
                          (> (cdr p) best-n)))
               (values v (cdr p))
               (values best-v best-n))))
         #;(displayln (list best-v best-n))
         (cond
           [(<= best-n 1) t]
           [else
            ; factor-out best-v
            (define-values (us-rm us-other)
              (for/fold ([us-rm '()]
                         [us-other '()]
                         #:result (values (reverse us-rm) (reverse us-other)))
                        ([u (in-list us)])
                (cond [(eq? times (operator-kind u))
                       (define-values (l rm?) (remove+? best-v (rest u)))
                       (if rm?
                         (values (cons (apply times-fun l) us-rm) us-other)
                         (values us-rm (cons (apply times-fun l) us-other)))]
                      [else
                       (values us-rm (cons u us-other))])))
            (define us-rm-reduced
              (apply plus-fun us-rm))
            (define times-tree
              (let ([opk (operator-kind us-rm-reduced)])
                (cond [(eq? times opk)
                       (apply times-fun best-v (rest us-rm-reduced))]
                      [else
                       (times-fun best-v us-rm-reduced)])))
            (define new-t
              (if (empty? us-other)
                ; remove top-level +
                times-tree
                (apply plus-fun times-tree us-other)))
            (if (equal? t new-t)
              t
              ; There has been a change, try to factor more
              (loop new-t))])]
        [(symbol->function (operator-kind t))
         =>
         (λ (fun) (apply fun (map loop (cdr t))))]
        [else t]))))

(module+ test
  (require rackunit
           "arithmetic.rkt")
  (check-equal? (factor-product '(+ a b))
                (+ 'a 'b))
  (check-equal? (factor-product '(+ (* a b) (* c d)))
                (+ (* 'a 'b) (* 'c 'd)))
  (check-equal? (factor-product '(+ a (* a b) (* a c)))
                (* 'a (+ 1 'b 'c)))
  (check-equal? (factor-product '(+ a b (* a b) (* a c)))
                (+ 'b (* 'a (+ 1 'b 'c))))
  (check-equal? (factor-product '(* (+ a (* a b) (* a c))))
                '(* a (+ 1 b c)))

  (check-equal? (factor-product '(+ a (* a b x) (* a c x) (* a d y) (* a e y)))
                '(* a (+ 1 (* (+ b c) x) (* (+ d e) y))))

  (check-equal?
   (factor-product
    '(+
      (* a e)
      (* b e)
      (* c e)
      (* d e)
      (* a f)
      (* b f)
      (* c f)
      (* d f)
      (* a g)
      (* b g)
      (* c g)
      (* d g)
      (* a h)
      (* b h)
      (* c h)
      (* d h)))
   '(* (+ a b c d) (+ e f g h)))

  ; does a decent job, but does not refactor to (+ 1 a)(+ 1 b)...
  #;
  (factor-product
   '(+
     1
     a
     b
     (* a b)
     c
     (* a c)
     (* b c)
     (* a b c)
     d
     (* a d)
     (* b d)
     (* a b d)
     (* c d)
     (* a c d)
     (* b c d)
     (* a b c d))))

