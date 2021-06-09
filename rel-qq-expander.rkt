#lang racket
(require minikanren-ee)
(require "numbers.rkt")

;; requires a miniKanren with not-pair and (im)proper-list constraints to properly execute.
;; Suspiciously bad performance on qq-expand; suggests this isn't viable or needs work


(define-relation (quasiconso x y o)
  (conde
    [#;(not-pairo y) (== o `(list* ,x ,y))
     ]
    [(fresh (a rest)
       (== y `(,a . ,rest))
       (conde 
         [(== a 'quote)
          (conde
            [#;(not-pairo rest) (== o `(list* ,x ,y))
             ]
            [(fresh (dy tl)
               (== rest `(,dy . ,tl))
               (conde
                 [(== tl '())
                  (conde
                    [(fresh (a d)
                       (== x (cons a d))
                      (conde 
                        [(== a 'quote)
                         (conde 
                           [#;(not-pairo d)
                            (conde
                              [(== dy '()) (== o `(list ,x))]
                              [(=/= dy '()) (== o `(list* ,x ,y))])]
                           [(fresh (da dd)
                              (== d (cons da dd))
                              (conde 
                                [(== dd '()) (== o `(quote (,da . ,dy)))]
                                [(=/= dd '())
                                 (conde
                                   [(== dy '()) (== o `(list ,x))]
                                   [(=/= dy '()) (== o `(list* ,x ,y))])]))])]
                        [(=/= a 'quote)
                         (conde
                           [(== dy '()) (== o `(list ,x))]
                           [(=/= dy '()) (== o `(list* ,x ,y))])]))]
                    [#;(not-pairo x)
                     (conde
                       [(== dy '()) (== o `(list ,x))]
                       [(=/= dy '()) (== o `(list* ,x ,y))])])]
                 [(=/= tl '()) (== o `(list* ,x ,y))]))])]
         [(== a 'list)
          (== o `(list ,x . ,rest))]
         [(== a 'list*)
          (== o `(list* ,x . ,rest))]
         [(=/= a 'quote)
          (=/= a 'list)
          (=/= a 'list*)
          (== o `(list* ,x ,y))]))]))

(define-relation (quasilist* x y o)
  (conde 
    [(== x '()) (== y o)]
    [(fresh (a d)
       (== x `(,a . ,d))
       (fresh (res)
         (quasilist* d y res)
         (quasiconso a res o)))]))

(define-relation (qq-expand expr depth o)
  (conde
    [#;(not-pairo expr) (== o `',expr)] ;; We wanna quote the input.
    [(fresh (a d)
       (== o (cons a d))
       (conde 
         [#;(not-pairo a)
          (conde 
           [(== a 'unquote)
            (conde
              [#;(not-pairo d)
               (fresh (qa qd)
                 (qq-expand a depth qa)
                 (qq-expand d depth qd)
                 (quasiconso qa qd o))]
              [(fresh (p n)
                 (== d (cons p n))
                 (conde
                   [(== n '())
                    (conde
                      [(zeroo depth) (== p o)]
                      [(poso depth)
                       (fresh (res)
                         (minuso depth '(1) res) ;; build-num 1
                         (fresh (res2)
                           (qq-expand p res res2)
                           (quasiconso ''unquote res2 o)))])]
                   [(=/= n '())
                    (fresh (qa qd)
                      (qq-expand a depth qa)
                      (qq-expand d depth qd)
                      (quasiconso qa qd o))]))])]
           [(== a 'quasiquote)
            (conde
              [#;(not-pairo d)
               (fresh (qa qd)
                 (qq-expand a depth qa)
                 (qq-expand d depth qd)
                 (quasiconso qa qd o))]
              [(fresh (p n)
                 (== d (cons p n))
                 (conde
                   [(== n '())
                    (fresh (d2 qd)
                      (pluso '(1) depth d2)
                      (qq-expand p d2 qd)
                      (quasiconso ''quasiquote qd o))]
                   [(=/= n '())
                    (fresh (qa qd)
                      (qq-expand a depth qa)
                      (qq-expand d depth qd)
                      (quasiconso qa qd o))]))])]
           [(=/= a 'unquote)
            (=/= a 'quasiquote)
            (fresh (qa qd)
              (qq-expand a depth qa)
              (qq-expand d depth qd)
              (quasiconso qa qd o))])]
         [(fresh (aa da)
            (== a (cons aa da))
            (conde
              [(== aa 'unquote)
               (conde
                 [(zeroo depth)
                  (conde
                    [(fresh (qd)
                       (qq-expand d depth qd)
                       (quasilist* da qd o))] ;; the list? will vanish b/c quasilist handles
                    [#;(improper-listo da)
                     (fresh (qa qd)
                       (qq-expand a depth qa)
                       (qq-expand d depth qd)
                       (quasiconso qa qd o))])]
                 [(poso depth)
                  (conde
                    [#;(proper-listo da) ;; must check b/c must force da to be a proper list
                     (fresh (qd res)
                       (minuso depth '(1) res) ;; build-num 1
                       (qq-expand d depth qd)
                       (fresh (qda)
                         (qq-expand da res qda)
                         (fresh (qa)
                           (quasiconso ''unquote qda qa)
                           (quasiconso qa qd o))))]
                    ;; In the improper list case we fall through to
                    ;; a duplicate of the else case from the surrounding
                    ;; match expression
                    [#;(improper-listo da)
                     (fresh (qa qd)
                       (qq-expand a depth qa)
                       (qq-expand d depth qd)
                       (quasiconso qa qd o))])])]
              [(=/= 'unquote aa)
               (fresh (qa qd)
                 (qq-expand a depth qa)
                 (qq-expand d depth qd)
                 (quasiconso qa qd o))]))]))]))
