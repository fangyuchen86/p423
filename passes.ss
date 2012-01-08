(library (p423 compiler passes)
  (export
    verify-scheme
    generate-x86-64)
  (import
    (chezscheme)
    (p423 compiler helpers)
    (p423 compiler match))

;;; Andy Keep, Kent Dybvig
;;; P423/P523
;;; Spring 2010

;;; verify-scheme accept a single value and verifies that the value
;;; is a valid program in the current source language.
;;;
;;; Program   -> (begin Statement+)
;;; Statement -> (set! Var1 int64)
;;;            | (set! Var1 Var2)
;;;            | (set! Var1 (Binop Var1 int32))
;;;            | (set! Var1 (Binop Var1 Var2))
;;; Var       -> rax | rcx | rdx | rbx | rbp | rsi | rdi
;;;            | r8 | r9 | r10 | r11 | r12 | r13 | r14 | r15
;;; Binop     -> + | - | *
;;;
;;; If the value is a valid program, verify-scheme returns the value
;;; unchanged; otherwise, it signals an error.
;;;
;;; define-who is defined in helpers.ss.  It not only defines the
;;; variable who to be the name of the pass being defined but also
;;; gives us a handy place to stick local helpers.

(define-who verify-scheme
  (define verify-binop
    (lambda (x)
      (unless (memq x '(+ - *))
        (error who "invalid binop ~s" x))))
  (define verify-var
    (lambda (x)
      (unless (memq x '(rax rcx rdx rbx rbp rsi rdi
                        r8 r9 r10 r11 r12 r13 r14 r15))
        (error who "invalid variable name ~s" x))))
  (define verify-int32
    (lambda (x)
      (unless (and (integer? x) (exact? x)
                   (<= (- (expt 2 31)) x (- (expt 2 31) 1)))
        (error who "invalid int32 ~s" x))))
  (define verify-int64
    (lambda (x)
      (unless (and (integer? x) (exact? x)
                   (<= (- (expt 2 63)) x (- (expt 2 63) 1)))
        (error who "invalid int64 ~s" x))))
  (define Statement
    (lambda (st)
      (match st
        [(set! ,var ,n)
         (guard (number? n))
         (verify-var var)
         (verify-int64 n)]
        [(set! ,var1 ,var2)
         (guard (symbol? var2))
         (verify-var var1)
         (verify-var var2)]
        [(set! ,var (,prim ,var ,n))
         (guard (number? n))
         (verify-binop prim)
         (verify-var var)
         (verify-int32 n)]
        [(set! ,var1 (,prim ,var1 ,var2))
         (verify-binop prim)
         (verify-var var1)
         (verify-var var2)]
        [,st (error who "invalid syntax for Statement ~s" st)])))
  (lambda (x)
    (match x
      [(begin ,[Statement -> st1] ,[Statement -> st2*] ...) x]
      [,x (error who "invalid syntax for Program: expected (begin stmt+)")])
    x))

;;; generate-x86-64 accepts a valid program in the grammar described
;;; in the comment block for verify-scheme above, and writes an
;;; equivalent x86_64 assembly language program to the current output
;;; port.  It uses the emit macro from helpers.ss, which is smart enough
;;; to handle register and immediate operands properly (among others),
;;; which simplifies the task.

(define-who generate-x86-64
  (define-syntax emit-program
    (syntax-rules ()
      [(_ code)
       (begin
         (emit '.globl "_scheme_entry")
         (emit-label "_scheme_entry")
         code
         (emit 'ret))]))
  (define prim->inst
    (lambda (op)
      (case op
        [(+) 'addq]
        [(-) 'subq]
        [(*) 'imulq]
        [else (error who "unexpected binop ~s" op)])))
  (define Statement
    (lambda (st)
      (match st
        [(set! ,dst (,prim ,dst ,src))
         (emit (prim->inst prim) src dst)]
        [(set! ,dst ,src)
         (emit 'movq src dst)]
        [,st (error who "unexpected statement ~s" st)])))
  (lambda (x)
    (match x
      [(begin ,st* ...) (emit-program (for-each Statement st*))]
      [,x (error who "unexpected program ~s" x)])))

)
