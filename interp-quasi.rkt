#lang racket
(require minikanren)

(define (quo-qua-unq t)
  (conde
    ((== t '()))
    ((== t 'x))
    ((== t '5))
    ((conde
       ((== t 'unquote))
       ((== t 'quasiquote))
       ((== t 'quote))))
    ((fresh (a d)
       (== t `(,a . ,d))
       (quo-qua-unq a)
       (conde 
         ((quo-qua-unq d))
         ((== d '())))))))

(define (lookup x e o)
  (fresh (y v d)
    (== `((,y . ,v) . ,d) e)
    (conde
      ((== x y) (== v o))
      ((=/= x y) (lookup x d o)))))

(define (evaluate-exprs exprs env vs)
  (conde
    [(== '() exprs) (== vs '())]
    [(fresh (expr exprs^ vs^ v)
       (== exprs `(,expr . ,exprs^))
       (== vs `(,v . ,vs^))
       (evaluate expr env v)
       (evaluate-exprs exprs^ env vs^))]))

(define (evaluate expr env o)
  (conde 
    [(numbero expr) (== expr o)]
    [(symbolo expr) (lookup expr env o)]
    [(fresh (expr2)
       (== expr (list 'quasiquote expr2))
       (eval-quasi expr2 env o))]
    [(== expr (list 'quote o)) 
     (absento 'closure o)]
    ;; sort of a hack. 
    [(fresh (exprs)
       (== expr `(list . ,exprs))
       (evaluate-exprs exprs env o))]
    [(fresh (x b)
       (== expr `(lambda (,x) ,b))
       (== o `(closure ,x ,b ,env)))]
    [(fresh (rator rand)
       (== expr `(,rator ,rand))
       (fresh (x b env^ v)
         (evaluate rator env `(closure ,x ,b ,env^))
         (evaluate rand env v)
         (evaluate b `((,x . ,v) . ,env^) o)))]))

 ;; this isn't *really* right though. Yeah?
    ;; It's actually too restrictive. It just can't be *used*
    ;; dynamically ever in any way that would fake an
    ;; honest-to-goodness #<procedure>. Otherwise it's a fine symbol
;; A second grammar. But eagerly produced! Yuck!

;; [(fresh (a d)
;;    (conde
;;      ((== a 'quote) )
;;      ((== a 'unquote) )
;;      ((== a 'quasiquote ))))]

(define (eval-quasi expr env o)
  (conde 
    [(numbero expr) (== expr o)]
    [(symbolo expr)
     (== expr o)
     (=/= expr 'closure)]
    [(fresh (a res)
       (== expr (list 'quote a))
       (== o (list 'quote res))
       (eval-quasi a env res))]
    [(fresh (a res)
       (== expr (list 'quasiquote a))
       (== o (list 'quasiquote res))
       (eval-quasi a env res))]
    [(fresh (a)
       (== expr (list 'unquote a))
       (evaluate a env o))]
    [(fresh (a d v1 v2)
       (== expr (cons a d))
       (=/= 'quasiquote a)
       (=/= 'quote a)
       (=/= 'unquote a)
       (== o (cons v1 v2))
       (eval-quasi a env v1)
       (eval-quasi d env v2))]))

;; Can't do it w/Fail, cut. 



;; What is the grammar of things X for which you can write (quasiquote X) ?
;; symbol
;; number
;; valid improper lists
;; what is a valid improper list? (that, is a superset of the proper-lists)
;; 

#| 
> (quasiquote (quote (unquote unquote)))

Exception: misplaced aux keyword unquote
Type (debug) to enter the debugger.

|# 

#| 
This is far more complicated than I had at first anticipated
> (quasiquote (quote (unquote unquote)))

Exception: misplaced aux keyword unquote
Type (debug) to enter the debugger.
> (quasiquote (unquote quuz))
Exception: variable quuz is not bound
Type (debug) to enter the debugger.
> (quasiquote (unquote . quuz))
(unquote . quuz)
> (quasiquote (quasiquote . (unquote 5)))
(quasiquote . 5)
|# 


#| 

One thing to fix is to ensure that we disallow the symbol closure
*only* in the places where we could try and form or use it as a
simulation of a defunctionalized closure. Thus changes dep on the rest
of the lang.

|# 

#| 
One thing to fix is to make sure we disallow unquotes only in the
right places (non-list pairs are just fine) 

> (quasiquote (unquote quuz))
Exception: variable quuz is not bound
Type (debug) to enter the debugger.
> (quasiquote (unquote . quuz))
(unquote . quuz)
> 
|# 
;; General arbitrary data

;; This is strange because you cannot do the absento trick to restrict
;; away the things.

#| 

We can't have multiple overlapping clauses, where particular ground
values (ambiguous grammars)! So that we don't end up with two ways to
get to the same value, or generic description thereof. That 


Mutually recursive constraints? 

|#


#| 

((`,() failure)           ;; unquoted nil 
 (`,x failure)            ;; free variable
 (`(() . ,()) failure)    ;; unquoted nil
 (`,unquote failure)      ;; free var (bad syntax?)
 (`(() . ,x) failure)     ;; free var 
 (`(x . ,()) failure)  
 (`(() () . ,()) failure)
 (`(() . ,unquote) failure) 
 (`(x . ,x) failure)
 (`(() () . ,x) failure)
 (`,quasiquote failure)   ;; bad syntax / free var
 (`(unquote . ,()) failure)  ;; 
 (`(() x . ,()) failure)
 (`(x () . ,()) failure)
 (`(x . ,unquote) failure)
 (`(() () () . ,()) failure)
 (`(() () . ,unquote) failure)
 (`(unquote . ,x) failure)
 (`(() x . ,x) failure)
 (`(x () . ,x) failure)
 (`(() () () . ,x) failure)
 (`,quote failure)  ;; another syntax 
 (`(() . ,quasiquote) failure)
 (`(5 . ,()) failure)
 (`(() unquote . ,()) failure)
 (`(x x . ,()) failure)
 (`(() () x . ,()) failure)
 (`(unquote () . ,()) failure)
 (`(unquote . ,unquote) failure)
 (`(() x () . ,()) failure)
 (`(() x . ,unquote) failure)
 (`(x () () . ,()) failure)
 (`(x () . ,unquote) failure)
 (`(() () () () . ,()) failure)
 (`(() () () . ,unquote) failure)
 (`(5 . ,x) failure)
 (`(() unquote . ,x) failure)
 (`(x x . ,x) failure)
 (`(() () x . ,x) failure)
 (`(unquote () . ,x) failure)
 (`(() x () . ,x) failure)
 (`(x () () . ,x) failure)
 (`(() () () () . ,x) failure)
 (`(() . ,quote) failure)
 (`(x . ,quasiquote) failure)
 (`(() () . ,quasiquote) failure)
 (`(quasiquote . ,()) failure)
 (`(() 5 . ,()) failure)
 (`(x unquote . ,()) failure)
 (`(() () unquote . ,()) failure)
 (`(unquote x . ,()) failure)
 (`(() x x . ,()) failure)
 (`(x () x . ,()) failure)
 (`(() () () x . ,()) failure)
 (`(5 () . ,()) failure)
 (`(5 . ,unquote) failure)
 (`(() unquote () . ,()) failure)
 (`(() unquote . ,unquote) failure)
 (`(x x () . ,()) failure)
 (`(x x . ,unquote) failure)
 (`(() () x () . ,()) failure)
 (`(() () x . ,unquote) failure)
 (`(unquote () () . ,()) failure)
 (`(unquote () . ,unquote) failure)
 (`(() x () () . ,()) failure)
 (`(() x () . ,unquote) failure)
 (`(x () () () . ,()) failure)
 (`(x () () . ,unquote) failure)
 (`(() () () () () . ,()) failure)
 (`(() () () () . ,unquote) failure)
 (`,(()) failure)
 (`(quasiquote . ,x) failure)
 (`(() 5 . ,x) failure)
 (`(x unquote . ,x) failure)
 (`(() () unquote . ,x) failure)
 (`(unquote x . ,x) failure)
 (`(() x x . ,x) failure)
 (`(x () x . ,x) failure)
 (`(() () () x . ,x) failure)
 (`(5 () . ,x) failure)
 (`(() unquote () . ,x) failure)
 (`(x x () . ,x) failure)
 (`(() () x () . ,x) failure)
 (`(unquote () () . ,x) failure)
 (`(() x () () . ,x) failure)
 (`(x () () () . ,x) failure)
 (`(() () () () () . ,x) failure)
 (`(x . ,quote) failure)
 (`(unquote . ,quasiquote) failure)
 (`(() () . ,quote) failure)
 (`(() x . ,quasiquote) failure)
 (`(x () . ,quasiquote) failure)
 (`(() () () . ,quasiquote) failure)
 (`(quote . ,()) failure)
 (`(x 5 . ,()) failure)
 (`(() quasiquote . ,()) failure)
 (`(() () 5 . ,()) failure)
 (`(unquote unquote . ,()) failure)
 (`(() x unquote . ,()) failure)
 (`(x () unquote . ,()) failure)
 (`(() () () unquote . ,()) failure)
 (`(5 x . ,()) failure)
 (`(() unquote x . ,()) failure)
 (`(x x x . ,()) failure)
 (`(() () x x . ,()) failure)
 (`(unquote () x . ,()) failure)
 (`(() x () x . ,()) failure)
 (`(x () () x . ,()) failure)
 (`(() () () () x . ,()) failure)
 (`(quasiquote () . ,()) failure)
 (`(() 5 () . ,()) failure)
 (`(quasiquote . ,unquote) failure)
 (`(x unquote () . ,()) failure)
 (`(() 5 . ,unquote) failure)
 (`(() () unquote () . ,()) failure)
 (`(x unquote . ,unquote) failure)
 (`(() () unquote . ,unquote) failure)
 (`(unquote x () . ,()) failure)
 (`(() x x () . ,()) failure)
 (`(unquote x . ,unquote) failure)
 (`(x () x () . ,()) failure)
 (`(() x x . ,unquote) failure)
 (`(() () () x () . ,()) failure)
 (`(x () x . ,unquote) failure)
 (`(5 () () . ,()) failure)
 (`(() () () x . ,unquote) failure)
 (`(() unquote () () . ,()) failure)
 (`(5 () . ,unquote) failure)
 (`(x x () () . ,()) failure)
 (`(() unquote () . ,unquote) failure)
 (`(() () x () () . ,()) failure)
 (`(x x () . ,unquote) failure)
 (`(unquote () () () . ,()) failure)
 (`(() () x () . ,unquote) failure)
 (`(() x () () () . ,()) failure)
 (`(unquote () () . ,unquote) failure)
 (`(x () () () () . ,()) failure)
 (`(() x () () . ,unquote) failure)
 (`(() () () () () () . ,()) failure)
 (`(x () () () . ,unquote) failure)
 (`(() () () () () . ,unquote) failure)
 (`,(() . x) failure)
 (`(() . ,(())) failure)
 (`(quote . ,x) failure)
 (`(x 5 . ,x) failure)
 (`(() quasiquote . ,x) failure)
 (`(() () 5 . ,x) failure)
 (`(unquote unquote . ,x) failure)
 (`(() x unquote . ,x) failure)
 (`(x () unquote . ,x) failure)
 (`(() () () unquote . ,x) failure)
 (`(5 x . ,x) failure)
 (`(() unquote x . ,x) failure)
 (`(x x x . ,x) failure)
 (`(() () x x . ,x) failure)
 (`(unquote () x . ,x) failure)
 (`(() x () x . ,x) failure)
 (`(x () () x . ,x) failure)
 (`(() () () () x . ,x) failure)
 (`(quasiquote () . ,x) failure)
 (`(() 5 () . ,x) failure)
 (`(x unquote () . ,x) failure)
 (`(() () unquote () . ,x) failure)
 (`(unquote x () . ,x) failure)
 (`(() x x () . ,x) failure)
 (`(x () x () . ,x) failure)
 (`(() () () x () . ,x) failure)
 (`(5 () () . ,x) failure)
 (`(() unquote () () . ,x) failure)
 (`(x x () () . ,x) failure)
 (`(() () x () () . ,x) failure)
 (`(unquote () () () . ,x) failure)
 (`(() x () () () . ,x) failure)
 (`(x () () () () . ,x) failure)
 (`(() () () () () () . ,x) failure)
 (`(unquote . ,quote) failure)
 (`(5 . ,quasiquote) failure)
 (`(() x . ,quote) failure)
 (`(() unquote . ,quasiquote) failure)
 (`(x () . ,quote) failure)
 (`(() () () . ,quote) failure)
 (`(x x . ,quasiquote) failure)
 (`(() () x . ,quasiquote) failure)
 (`(unquote () . ,quasiquote) failure)
 (`(() x () . ,quasiquote) failure)
 (`(x () () . ,quasiquote) failure)
 (`(() () () () . ,quasiquote) failure)
 (`(unquote 5 . ,()) failure)
 (`(x quasiquote . ,()) failure)
 (`(() quote . ,()) failure)
 (`(() x 5 . ,()) failure)
 (`(() () quasiquote . ,()) failure)
 (`(x () 5 . ,()) failure)
 (`(() () () 5 . ,()) failure)
 (`(5 unquote . ,()) failure)
 (`(() unquote unquote . ,()) failure)
 (`(x x unquote . ,()) failure)
 (`(() () x unquote . ,()) failure)
 (`(unquote () unquote . ,()) failure)
 (`(() x () unquote . ,()) failure)
 (`(x () () unquote . ,()) failure)
 (`(() () () () unquote . ,()) failure)
 (`(quasiquote x . ,()) failure)
 (`(() 5 x . ,()) failure)
 (`((()) . ,()) failure)
 (`(x unquote x . ,()) failure)
 (`(() () unquote x . ,()) failure)
 (`(unquote x x . ,()) failure)
 (`(() x x x . ,()) failure)
 (`(x () x x . ,()) failure)
 (`(() () () x x . ,()) failure)
 (`(5 () x . ,()) failure)
 (`(() unquote () x . ,()) failure)
 (`(x x () x . ,()) failure)
 (`(() () x () x . ,()) failure)
 (`(unquote () () x . ,()) failure)
 (`(() x () () x . ,()) failure)
 (`(x () () () x . ,()) failure)
 (`(() () () () () x . ,()) failure)
 (`(quote () . ,()) failure)
 (`(x 5 () . ,()) failure)
 (`(() quasiquote () . ,()) failure)
 (`(() () 5 () . ,()) failure)
 (`(quote . ,unquote) failure)
 (`(unquote unquote () . ,()) failure)
 (`(x 5 . ,unquote) failure)
 (`(() quasiquote . ,unquote) failure)
 (`(() x unquote () . ,()) failure)
 (`(() () 5 . ,unquote) failure)
 (`(x () unquote () . ,()) failure)
 (`(unquote unquote . ,unquote) failure)
 (`(() () () unquote () . ,()) failure)
 (`(() x unquote . ,unquote) failure)
 (`(x () unquote . ,unquote) failure)
 (`(5 x () . ,()) failure)
 (`(() () () unquote . ,unquote) failure)
 (`(() unquote x () . ,()) failure)
 (`(x x x () . ,()) failure)
 (`(5 x . ,unquote) failure)
 (`(() () x x () . ,()) failure)
 (`(() unquote x . ,unquote) failure)
 (`(unquote () x () . ,()) failure)
 (`(x x x . ,unquote) failure)
 (`(() x () x () . ,()) failure)
 (`(() () x x . ,unquote) failure)
 (`(x () () x () . ,()) failure)
 (`(unquote () x . ,unquote) failure)
 (`(() () () () x () . ,()) failure)
 (`(quasiquote () () . ,()) failure)
 (`(() x () x . ,unquote) failure)
 (`(() 5 () () . ,()) failure)
 (`(x () () x . ,unquote) failure)
 (`(x unquote () () . ,()) failure)
 (`(() () () () x . ,unquote) failure)
 (`(quasiquote () . ,unquote) failure)
 (`(() () unquote () () . ,()) failure)
 (`(() 5 () . ,unquote) failure)
 (`(unquote x () () . ,()) failure)
 (`(x unquote () . ,unquote) failure)
 (`(() x x () () . ,()) failure)
 (`(() () unquote () . ,unquote) failure)
 (`(x () x () () . ,()) failure)
 (`(unquote x () . ,unquote) failure)
 (`(() () () x () () . ,()) failure)
 (`(5 () () () . ,()) failure)
 (`(() x x () . ,unquote) failure)
 (`(() unquote () () () . ,()) failure)
 (`(x () x () . ,unquote) failure)
 (`(() () () x () . ,unquote) failure)
 (`(x x () () () . ,()) failure)
 (`(5 () () . ,unquote) failure)
 (`(() () x () () () . ,()) failure)
 (`(() unquote () () . ,unquote) failure)
 (`(unquote () () () () . ,()) failure)
 (`(x x () () . ,unquote) failure)
 (`(() x () () () () . ,()) failure)
 (`(() () x () () . ,unquote) failure)
 (`(x () () () () () . ,()) failure)
 (`(unquote () () () . ,unquote) failure)
 (`(() () () () () () () . ,()) failure)
 (`(() x () () () . ,unquote) failure)
 (`(x () () () () . ,unquote) failure)
 (`(() () () () () () . ,unquote) failure)
 (`,(() . unquote) failure)
 (`(() . ,(() . x)) failure))

|# 

