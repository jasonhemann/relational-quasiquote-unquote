#lang racket
(require expect/rackunit)

;; One thing Kent does that we do not is to replace a list* of two elements with a solitary cons. To be fixed.

(define (quasiappend x y)
  (match y 
    [(list 'quote '())
     (match x
       ['() (list 'quote '())]
       [`(,s) s]
       [`,ls
        (if (list? ls)
            `(append . ,ls)
            (error 'quasiappend "reached a point where we should have had a list"))])]
    [_
     (match x
       ['() (list 'quote '())]
       [`,ls
        (if (list? ls)
            `(append . ,(append ls y))
            (error 'quasiappend "reached a point where we should have had a list"))])]))

(define (quasicons x y)
  (match y
    [(list 'quote dy)
     (match x
       [(list 'quote dx)
        (list 'quote (cons dx dy))]
       [_ (if (null? dy)
              (list 'list x)
              (list 'list x y))])]
    [(cons 'list stuff) (cons 'list (cons x stuff))]
    [(cons 'list* stuff) (cons 'list* (cons x stuff))]
    [_ (list 'list* x y)]))

(define (quasilist* x y)
  (cond
    [(null? x) y]
    [else (quasicons (car x) (quasilist* (cdr x) y))]))

(define (qq-expand expr depth)
  (match expr
    [(list 'unquote p)
     (cond
       [(zero? depth) p]
       [else (quasicons ''unquote (qq-expand p (sub1 depth)))])]
    [(list 'quasiquote p)
     (quasicons ''quasiquote (qq-expand p (add1 depth)))]
    [(cons a d)
     (match a
       [(cons 'unquote da)
        (cond
          [(list? da) ;; must check b/c da might be an improper list
           (cond
             [(zero? depth) (quasilist* da (qq-expand d depth))]
             [else
              (quasicons
               (quasicons ''unquote (qq-expand da (sub1 depth)))
               (qq-expand d depth))])]
          ;; In the improper list case we fall through to
          ;; a duplicate of the else case from the surrounding
          ;; match expression
          [else (quasicons (qq-expand a depth) (qq-expand d depth))])]
       [(cons 'unquote-splicing da)
        (cond
          [(list? da) ;; must check b/c da might be an improper list
           (cond
             [(zero? depth) (quasiappend da (qq-expand d depth))]
             [else
              (quasicons
               (quasicons ''unquote (qq-expand da (sub1 depth)))
               (qq-expand d depth))])]
          ;; In the improper list case we fall through to
          ;; a duplicate of the else case from the surrounding
          ;; match expression
          [else (quasicons (qq-expand a depth)
                           (qq-expand d depth))])]
       [else (quasicons (qq-expand a depth) (qq-expand d depth))])]
    ;; Finally, because we here want to produce the quoted version of this input.
    [`,p `',p]))

(define ns (make-base-namespace))
(namespace-attach-module (current-namespace) 'racket ns)
(namespace-require 'racket ns)
(namespace-require 'racket/list ns)

;; Evaluating, in Racket, the value of expression ...
(let ([val `(let ((x 5) (y 7))
              ,(qq-expand '(a (unquote x y) b) 0))])
  ;; ... namely,
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
  (check-expect
   (eval val ns)
   '(unquote . x)))
;; ... is the same as evaluating, in Chez, this expression ...
'(let ((x 5))
   `(unquote . x))

;; Evaluating, in Racket, the value of expression ...
(let ([val `(let ((x 5))
              ,(qq-expand '(unquote-splicing . x) 0))])
  ;; ... namely, 
  (check-expect
   (eval val ns)
   '(unquote-splicing . x))  )
;; ... is the same as evaluating, in Chez, this expression ...
'(let ((x 5))
   `(unquote-splicing . x))

;; Evaluating, in Racket, the value of expression ...
(let ([val `,(qq-expand '(1 2 . (unquote (make-list 5 'b) 10)) 0)])
  ;; ... namely, 
  (check-expect
   (eval val ns)
   '(1 2 unquote (make-list 5 'b) 10)))
;; ... is the same as evaluating, in Chez, this expression ...
'`(1 2 . (unquote (make-list 5 'b) 10))


;; Evaluating, in Racket, the value of expression ...
(let ([val `,(qq-expand '(1 2 . (unquote-splicing (make-list 5 'b) 10)) 0)])
  ;; ... namely, 
  (check-expect
   (eval val ns)
   '(1 2 unquote-splicing (make-list 5 'b) 10)))
;; ... is the same as evaluating, in Chez, this expression ...
'`(1 2 . (unquote-splicing (make-list 5 'b) 10))


;; Evaluating, in Racket, the value of expression ...
(let ([val `,(qq-expand '(1 2 (unquote-splicing (make-list 5 'b) 10)) 0)])
  ;; ... namely, 
  (check-expect
   (eval val ns)
   '(1 2 b b b b b . 10)))
;; ... is the same as evaluating, in Chez, this expression ...
'`(1 2 (unquote-splicing (make-list 5 'b) 10))

'(The following tests fail)

;; Evaluating, in Racket, the value of expression ...
`(let ((x '(5 6)) (y '(7 8)))
   ,(qq-expand '(a (unquote-splicing x y) b) 0))
;; ... is the same as evaluating, in Chez, this expression ...
'(let ((x '(5 6)) (y '(7 8)))
   `(a (unquote-splicing x y) b))
;; ... namely, 
'(a 5 6 7 8 b)

;; Evaluating, in Racket, the value of expression ...
(let ([val `,(qq-expand '(1 (unquote-splicing 5 . 10) 3) 0)])
  ;; ... namely, 
  (check-expect
   (eval val ns)
   '(1 (unquote-splicing 5 . 10) 3)))
;; ... is the same as evaluating, in Chez, this expression ...
'`(1 (unquote-splicing 5 . 10) 3)

;; Evaluating, in Racket, the value of expression ...
(let ([val `,(qq-expand '(1 (unquote-splicing '(5) . 10) . 3) 0)])
  ;; ... namely,
  (printf "does this look right ~s~n" val)
  (check-expect
   (eval val ns)
   '(1 (unquote-splicing '(5) . 10) . 3)))
;; ... is the same as evaluating, in Chez, this expression ...
'`(1 (unquote-splicing '(5) . 10) . 3)


;; Non-examples (Chez doesn't like, programs our expander should reject)
;; > `(1 (unquote-splicing 5 10) 3)
;; Exception in append: 10 is not a proper list
;; > `(1 (unquote-splicing 5 10))
;; Exception in append: 5 is not a proper list


