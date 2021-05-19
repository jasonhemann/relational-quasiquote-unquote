# relational-quasiquote-unquote
A relational miniKanren interpreter with `quasiquote` and
`unquote`. 

This problem has several interesting situations and edge cases. This
implementation has several problems, and several *kinds* of problems:

### Shadowing unquote
```
> ((lambda (unquote) (unquote 5)) add1)
6 
> ((lambda (unquote) `(unquote 5)) add1)
,5 
```

### Unquoting for precisely proper lists of one argument.

```
> (quasiquote (unquote quuz))
Exception: variable quuz is not bound
Type (debug) to enter the debugger.
> (quasiquote (unquote . quuz))
(unquote . quuz)
> (quasiquote (unquote a b c))
(unquote a b c)
```

### Unquote through invalid syntax. 

```
> (quasiquote (quasiquote (unquote (quote (unquote . (+ 2 3))))))
`,(quote #<procedure +> 2 3)

> (quasiquote (quasiquote (unquote (quote (unquote . 9)))))
`,'(unquote . 9)

> (quasiquote (quasiquote (unquote (quote (unquote)))))
`,(quote)

```

Note Racket's behavior differs in some subtle ways on these
expressions.

### Overly eager

This implementation subjectively feels overly eager in how it
generates quoted and quasiquoted expressions. We generate a high
percentage of quasiquoted expressions after adding these new
forms. For any expression E, `(quasiquote (unquote E))` is also a
valid expression. It seems like constraints we could more nicely,
lazily express these quasiquoted and unquoted expressions with
constraints.

### Prerequisites 

This repository presumes a current [Racket
installation](https://racket-lang.org/), and that in this installation
you have downloaded the `minikanren` package via Ballantyne's the
`faster-minikanren` repository.

We based this interpreter's behavior on [Chez
Scheme's](https://github.com/cisco/ChezScheme/) behavior. To compare
this interpreter's behavior you should also install Chez Scheme.

We have not yet completed this implementation. The program still has a
number of deficiencies.

### Further study

Later, I will reconsider mutually recursive constraints from my
dissertation work, and to determine if quasiquote and unquote
exemplify good mutually recursive constraints.

We right now cannot, but should, figure out how to parameterize these
two grammars by the other special forms. Right now, for instance a
programmer will have to add string constants in two places. 

We might wish also to, with an additional constraint, disallow the
symbol closure *only* in the places it would simulate a
defunctionalized closure. The implementation of this constraint would
thus depend on the rest of the language of the interpreter. This
bolsters some of the points from my dissertation research.

### References 

In addition to Chez Scheme itself as a reference implementation, we
 also referenced the following: 
 
 - Bawden's [Quasiquatation in
   Lisp](https://3e8.org/pub/scheme/doc/Quasiquotation%20in%20Lisp%20(Bawden).pdf)
 - Fare's [Portable quasiquote for Common
   Lisp](https://github.com/fare/fare-quasiquote)
 - Dybvig's [early old quasiquote
   implementation](https://groups.csail.mit.edu/mac/ftpdir/scheme-mail/HTML/rrrs-1986/msg00002.html)
   email thread. 
 - Chez's [current definition of
   quasiquote](https://github.com/cisco/ChezScheme/blob/master/s/syntax.ss#L7642)
   
Fare claims to have described and caught several important corner
cases to which other implementations often fall victim. At some point
we must check our implementation against these cases.

