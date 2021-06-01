#lang racket
(require expect/rackunit)

;; One thing Kent does that we do not is to replace a list* of two elements with a solitary cons. To be fixed.

(define (quasicons x y)
  (match y
    [(list 'quote dy)
     (match x
       [(list 'quote dx)
        (list 'quote (cons dx dy))]
       [_ (if (null? dy)
              (list 'list x)
              (list 'list* x y))])]
    ;; This is where we would add the missing list* enhancement
    [(cons 'list stuff) (cons 'list (cons x stuff))]
    [(cons 'list* stuff) (cons 'list* (cons x stuff))]
    [_ (list 'list* x y)]))

(define (quasilist* x y)
  (cond
    [(null? x) y]
    [else (quasicons (car x) (quasilist* (cdr x) y))]))

(define (qq-expand expr depth)
  (match expr
    ;; Finally, because we here want to produce the quoted version of this input.
    [`,p #:when (not (pair? p)) `',p]
    [(cons a d)
     (match a
       [`,a #:when (not (cons? a))
        (match a
          ['unquote
           (match d
             [`,d #:when (not (pair? d)) (quasicons (qq-expand a depth) (qq-expand d depth))]
             [(cons p n)
              (match n
                ['()
                 (cond
                   [(zero? depth) p]
                   [(positive? depth) (quasicons ''unquote (qq-expand p (sub1 depth)))])]
                [n #:when (not (eqv? n '())) (quasicons (qq-expand a depth) (qq-expand d depth))])])]
          ['quasiquote
           (match d
             [`,d #:when (not (pair? d)) (quasicons (qq-expand a depth) (qq-expand d depth))]
             [(cons p n)
              (match n
                ['() (quasicons ''quasiquote (qq-expand p (add1 depth)))]
                [n #:when (not (eqv? n '())) (quasicons (qq-expand a depth) (qq-expand d depth))])])]
          [`,a #:when (and (not (eqv? a 'unquote)) (not (eqv? a 'quasiquote)))
           (quasicons (qq-expand a depth) (qq-expand d depth))])]
       [(cons aa da)
        (match aa
          ['unquote 
           (cond
             [(zero? depth)
              (cond
                [(list? da) (quasilist* da (qq-expand d depth))] ;; the list? will vanish b/c quasilist handles
                [(not (list? da))
                 (quasicons
                  (qq-expand (cons aa da) depth)
                  (qq-expand d depth))])]
             [(positive? depth)
              (cond
                [(list? da) ;; must check b/c must force da to be a proper list
                 (quasicons
                  (quasicons ''unquote (qq-expand da (sub1 depth)))
                  (qq-expand d depth))]
                ;; In the improper list case we fall through to
                ;; a duplicate of the else case from the surrounding
                ;; match expression
                [(not (list? da)) (quasicons (qq-expand a depth) (qq-expand d depth))])])]
          [aa #:when (not (eqv? 'unquote aa)) (quasicons (qq-expand a depth) (qq-expand d depth))])])]))

(define (apply-env env y)
  (cdr (assv y env)))

;; Beginnings of an evaluator *for* these languages
(define (qq-eval expr env)
  (match expr
    [`,y #:when (symbol? y) (apply-env env y)]
    [`,n #:when (number? n) n]
    [`() `()]
    [`(append . ,args) (apply append (map (lambda (e) (qq-eval e env)) args))]
    [`(list* . ,args) (apply list* (map (lambda (e) (qq-eval e env)) args))]
    [`(list . ,args) (apply list (map (lambda (e) (qq-eval e env)) args))]
    [(list 'quote arg) arg]
    [(cons a d) #:when (or (not (pair? a)) (not (memv (car a) (list 'quote 'quasiquote 'unquote 'unquote-splicing))))
                (apply (qq-eval a env) (map (lambda (e) (qq-eval e env)) d))]))

(define ns (make-base-namespace))
(namespace-attach-module (current-namespace) 'racket ns)
(namespace-require 'racket ns)
(namespace-require 'racket/list ns)

;; Evaluating, in Racket, the value of expression ...
(let ([val `(let ((x 5) (y 7))
              ,(qq-expand '(a (unquote x y) b) 0))])
  ;; ... namely,
  (pretty-print val)  
  (check-expect 
   (eval val ns)
   '(a 5 7 b)))
;; ... is the same as evaluating, in Chez, this expression ...
'(let ((x 5) (y 7))
   `(a (unquote x y) b))

;; Evaluating, in Racket, the value of expression ...
(let ([val `(let ((x 5))
              ,(qq-expand '(unquote . x) 0))])
  ;; ... namely, 
  (pretty-print val)
  (check-expect
   (eval val ns)
   '(unquote . x)))
;; ... is the same as evaluating, in Chez, this expression ...
'(let ((x 5))
   `(unquote . x))

;; Evaluating, in Racket, the value of expression ...
(let ([val `,(qq-expand '(1 2 . (unquote (make-list 5 'b) 10)) 0)])
  ;; ... namely, 
  (pretty-print val)
  (check-expect
   (eval val ns)
   '(1 2 unquote (make-list 5 'b) 10)))
;; ... is the same as evaluating, in Chez, this expression ...
'`(1 2 . (unquote (make-list 5 'b) 10))

;; Non-examples (Chez doesn't like, programs our expander should reject)
;; > `(1 (unquote-splicing 5 10) 3)
;; Exception in append: 10 is not a proper list
;; > `(1 (unquote-splicing 5 10))
;; Exception in append: 5 is not a proper list

;; These examples test our evaluator. They demonstrate that using our evaluator to evaluate the output of our
;; expander returns the correct answer.

(check-expect
 (qq-eval '(list* 'a x y '(b)) '((x . 5) (y . 7)))
 '(a 5 7 b))

(check-expect
 (qq-eval ''(unquote . x) '((x . 5)))
 '(unquote . x))

(check-expect
 (qq-eval ''(1 2 unquote (make-list 5 'b) 10) '())
 '(1 2 unquote (make-list 5 'b) 10))


(check-expect
 (qq-eval '(list* '1 '2 (append (make-list 5 'b) 10)) `((make-list . ,make-list)))
 '(1 2 b b b b b . 10))


(check-expect
 (qq-eval '(append t '(b)) '((t . (7))))
 '(7 b))
     
(check-expect
 (qq-eval '(list* 'a (append x y '(b))) '((x . (5 6)) (y . (7 8))))
 '(a 5 6 7 8 b))

