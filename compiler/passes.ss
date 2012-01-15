;; passes.ss
;;
;; part of p423-sp12/srwaggon-p423
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; 2012/1/15

#!chezscheme
(library (compiler passes)
  (export
    verify-scheme generate-x86-64)
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (framework match)
    (framework helpers))

#| verify-scheme : program --> program
 | verify-scheme takes an expression representing a program and verifies
 | that it is an expression consiting solely of the provided language.
 | A descrition of the language is as follows.

Program   -->  (begin ,statement)
Statement -->  (set! ,var int64)
           |   (set! ,var1 ,var2)
           |   (set! ,var (,binop var int32))
           |   (set! ,var (,binop var ,var2))
Var       -->  rax | rcx | rdx | rbx | rbp | rsi | rdi
           |   r8 | r9 | r10 | r11 | r12 | r13 | r14 | r15
Binop     -->  + | - | *

 | If the program matches the language, the expression is returned.
 |#
(define-who (verify-scheme program)

  #| binop? : exp --> void
   | binop? takes an expression and throws an error if the given expression
   | does not qualify as a binary operation.
   |#
  (define-who (binop? exp)
    (define binops '(+ - *))
    (unless (and (memq exp binops) #t)
      (errorf who "invalid binop: ~s" exp))
  )
  
  #| var? : exp --> void
   | var? takes an expression and throws an error if the given expression
   | does not qualify as a variable.
   |#
  (define-who (var? exp)
    (unless (register? exp)
      (errorf who "invalid register: ~s" exp))
  )

  #| statement : exp --> void
   | statement throws an error unless the given expression obeys the language.
   |#
  (define-who (statement exp)
    (match exp
      [(set! ,var1 ,n)
       (guard (int64? n))
       (var? var1)]
      [(set! ,var1 ,var2)
       (guard (symbol? var2))
       (var? var1)
       (var? var2)]
      [(set! ,var1 (,binop ,var1 ,n))
       (guard (int32? n))
       (var? var1)
       (binop? binop)]
      [(set! ,var1 (,binop ,var1 ,var2))
       (guard (binop? binop))
       (var? var1)
       (var? var2)]
      [,x (errorf who "invalid statement: ~s" x)]
    )
  )

  (match program
    [(begin ,[statement -> st] ...) program]
    [,x (errorf who "invalid syntax for Program: expected (begin stmt+)")]
  )
)


#| generate-x86-64 : scheme-exp --> assembly-exp
 | generate-x86-64 takes a scheme expression and converts it into
 | x86-64 assembly code.
 |#
(define-who (generate-x86-64 exp)

  (define-who (statement exp)

    (define-who (binop->instr exp)
      (match exp
        [+ 'addq]
        [- 'subq]
        [* 'imulq]
        [,x (errorf who "unexpected binop ~s" x)]
      )
    )

    (match exp
      [(set! ,dst (,binop ,dst ,src))
       (emit (binop->instr binop) src dst)]
      [(set! ,dst ,src)
       (emit 'movq src dst)]
      [,x (errorf who "unexpected statement ~S" x)]
    )
  )

  (match exp
    [(begin ,st* ...) (emit-program (for-each statement st*))]
    [,exp (errorf who "unexpected program ~s" exp)]
  )
)


) ;; End Library