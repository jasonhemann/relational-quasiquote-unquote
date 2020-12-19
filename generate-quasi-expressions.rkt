#lang racket
(require minikanren)

#| 

Use this relation in miniKanren to generate quasiquote expressions.
Use these generated expressions to test either the relational
interpreter or Chez's behavior. 

miniKanren's biased interleaving search complicates generating good
mixtures of quasiquoted expressions. Because faster-minikanren still
relies on `conde` for interleaving, we add additional `conde`s to
increase the frequency of interleaving.

We also added an additional, superfluous, explicit case for true
lists. So many of the interesting quasiquote behaviors rely on and
require true lists that without this additional behavior we got
insufficiently many true lists.

|# 

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


#| 

(run 1 (q) 
  (fresh (x)
    (== q (list 'quasiquote x))
    (quo-qua-unq x)))

|# 
