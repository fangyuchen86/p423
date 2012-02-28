;; helpers.ss
;;
;; part of p423-sp12/srwaggon-p423 assign3
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in a4
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; 2012/2/11

#!chezscheme
(library (compiler helpers)
  (export
   binop?
   loc?
   relop?
   triv?
   var?
   error-unless
   error-when
   invalid
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (framework match)
    (framework helpers)
  )

#| relop? : sym --> boolean
 | relop? takes a symbol and returns #t
 | iff the symbol is a relational operator
 |
 | Relop --> < | <= | = | >= | >
 |#
(define (relop? exp)
  (define relops '(< <= = >= >))
  (and (memq exp relops) #t)
)

#| binop? : sym --> boolean
 | binop? takes a symbol and returns #t 
 | iff the symbol is a binary operator.
 | 
 | Binop --> + | - | * | logand | logor | sra
 |#
(define (binop? exp)
  (define binops '(+ - * logand logor sra))
  (and (memq exp binops) #t)
)

(define-syntax error-unless
  (syntax-rules ()
    [(_ bool who string exp ...)
     (unless bool
       (errorf who string exp ...))]
    [(_ bool string exp ...)
     (unless bool
       (errorf string exp ...))]
  )
)
(define-syntax error-when
  (syntax-rules ()
    [(_ bool who string exp ...)
     (when bool
       (errorf who string exp ...))]
    [(_ bool string exp ...)
     (when bool
       (errorf string exp ...))]
  )
)


#;(define-syntax invalid
  (syntax-rules ()
    [(_ what exp)
     (errorf "invalid ~s ~s" what exp)
    ]
  ))

(define (invalid who what exp)
  (errorf who "invalid ~s: ~s" what exp))

#| loc? : exp --> boolean
 | loc? takes an expression and returns #t
 | iff the expression is a location.
 |
 | Loc --> ,Register | <frame variable>
 |#
(define (loc? exp)
  (or (register? exp) (frame-var? exp))
)

#| var? : exp --> boolean
 | var? takes an expression and returns #t
 | iff the expression is a variable.
 |
 | Var --> ,<uvar> | ,Loc
 |#
(define (var? exp)
  (or
    (uvar? exp)
    (loc? exp)
  )
)

#| triv? : exp --> boolean
 | triv? takes an expression and returns #t
 | iff the expression is trivial.
 |
 | Triv --> ,Var | <integer> | <label>
 |#
(define (triv? exp)
  (or
    (var? exp)
    (integer? exp)
    (label? exp)
  )
)


) ;; End Library