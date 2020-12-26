#lang racket
(require minikanren)
(require rackunit)
#| 

This file purports to be a relational interpreter for a language with
quasiquoted and unquoted expressions. Students in my FA-2020-CS2800
prompted this discussion. The interpreter relies on a second grammar
for generating and matching quasiquoted expressions.

|#


#| Lookup relation, standard relational miniKanren practice.  |#
(define (lookup x e o)
  (fresh (y v d)
    (== `((,y . ,v) . ,d) e)
    (conde
      ((== x y) (== v o))
      ((=/= x y) (lookup x d o)))))

#| Standard nested miniKanren list evaluation relation |#
(define (evaluate-exprs exprs env vs)
  (conde
    [(== '() exprs) (== vs '())]
    [(fresh (expr exprs^ vs^ v)
       (== exprs `(,expr . ,exprs^))
       (== vs `(,v . ,vs^))
       (evaluate expr env v)
       (evaluate-exprs exprs^ env vs^))]))

#| Standard miniKanren "quine interpreter" with a quasiquote form. |#

;; This too is incorrect in a number of the usual ways:

;; - It is too restrictive, in that it rejects everywhere the use of
;; 'closure. Really we should just restrict its use in programs that
;; would generate fake closures as a result. Notice this would break
;; programs' contextual equivalence.

;; - It does not appropriately handle shadowing. As it stands, the
;; interpreter treats quasiquote, quote, lambda, and list as special
;; forms, rather than variables in an initial environment that can be
;; shadowed. We might enforce that we restrict these variables away
;; from the programmer's usage with constraints.

;; - Preventing shadowing with the `absento` constraint overly
;; restricts the programmer's use of such special variable names. This
;; would, for instance, totally restrict the use of `lambda` as a
;; variable.

(define (evaluate expr env o)
  (conde 
    [(numbero expr) (== expr o)]
    [(symbolo expr) (lookup expr env o)]
    [(fresh (expr2)
       (== expr (list 'quasiquote expr2))
       (eval-quasi expr2 env o))]
    [(== expr (list 'quote o)) 
     (absento 'closure o)]
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

#| This is the grammar that we use to generate quasiquoted expressions
for the language of the other interpreter. Our trick is to, rather
than trying to add quasiquote and unquote both to the original
interpreter, instead create a second grammar for the valid expressions
under a quasiquote. Because unquotes in these expressions then permit
any program of the language, these are mutually recursive
grammars. The reader should recall the Hemann and Friedman 2020
miniKanren workshop paper.
|#

;; This second eval-quasi grammar also we overly restricts the
;; programmer's use of the symbol `closure`.

;; We cannot, however, use the `absento` constraint to (overly)
;; restrict miniKanren's placement of some other expressions.

;; We must not have overlapping clauses, where we have multiple ways
;; to generate particular ground values. These would equate to
;; ambiguous grammars! Preventing these requires additionally
;; complicating this grammar's clauses. 

x
5
()
quasiquote
unqoute
quote
(x . 5)
(x . ())


(define (eval-quasi expr env o)
  (conde
    [(== expr '()) (== o '())]
    [(numbero expr) (== expr o)]
    [(symbolo expr)
     (== expr o)
     (=/= expr 'closure)]
    ((fresh (a d)
       (== expr `(,a . ,d))
       (conde
         ((=/= 'quasiquote a)
          (=/= 'quote a)
          (=/= 'unquote a)
          (== o (cons v1 v2))
          (eval-quasi a env v1)
          (eval-quasi d env v2))
         ((fresh (da dd)
            (== d `(,da . ,dd))
            (conde
              ((== dd '()) 
               (conde
                 ((== 'unquote a)
                  (evaluate da env o))
                 ((fresh (res)
                    (conde
                      ((== 'quasiquote a))
                      ((== 'quote a)))
                    (== o (list a res))
                    (eval-quasi a env res)))))
              ((=/= dd '())
               (== o (cons v1 v2))
               (eval-quasi a env v1)
               (eval-quasi d env v2)))))))))
    
    [(fresh (a res)
       (== expr (list 'quote a))
       (== o (list 'quote res))
       (eval-quasi a env res))]
    [(fresh (a res)
       (== expr (list 'quasiquote a))
       (== o (list 'quasiquote res))
)]
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



#| 

> (quasiquote             (unquote (quote (unquote (+ 2 3)))))
,(+ 2 3)

> (quasiquote      (quote (unquote (quote (unquote (+ 2 3))))))
',(+ 2 3)

> (quasiquote (quasiquote (unquote (quote (unquote (+ 2 3))))))
`,'5

> (quasiquote             (unquote (quote (unquote . (+ 2 3)))))
(unquote + 2 3)

> (quasiquote      (quote (unquote (quote (unquote . (+ 2 3))))))
'(unquote + 2 3)

> (quasiquote (quasiquote (unquote (quote (unquote . (+ 2 3))))))
`,(quote #<procedure +> 2 3)

> (quasiquote (quasiquote (unquote (quote (unquote . 9)))))
`,'(unquote . 9)

> (quasiquote (quasiquote (unquote (quote (unquote)))))
`,(quote)

> (quasiquote (quasiquote (unquote (quote (unquote 8 9 10 11) b c d))))
`,(quote 8 9 10 11 b c d)

(let ((a 'foo) (b 'bar))
    `(let ((z 'mat))
       `(,',b (lambda (,z) body))))

|# 


