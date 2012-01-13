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


;; passes.ss
;;
;; part of p423-sp12/srwaggon-p423
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; 2012/1/12

#|  Language
Program   -->  (begin ,statement)
Statement -->  (set! ,var int64)
           |   (set! ,var1 ,var2)
           |   (set! ,var (,binop var int32))
           |   (set! ,var (,binop var ,var2))
Var       -->  rax | rcx | rdx | rbx | rbp | rsi | rdi
           |   r8 | r9 | r10 | r11 | r12 | r13 | r14 | r15
Binop     -->  + | - | *
|#

  
(define (driver program)
  (with-output-to-file "t.s"
    (lambda ()
      (generate-x86-64 (verify-scheme program)))))

(define-who (verify-scheme program)

    (define-who (binop? exp)
        (define binops '(+ - *))
        (unless (and (memq exp binops) #t)
          (error who "invalid binop: ~s" exp)))

    (define-who (var? exp)
        (unless (register? exp)
          (error who "invalid register: ~s" exp)))

    (define-who (statement exp)
        (match exp
          [(set! ,var1 ,n)
           (guard (number? n))
           (var? var1)
           (int64? n)]
          [(set! ,var1 ,var2)
           (guard (symbol? var2))
           (var? var1)
           (var? var2)]
          [(set! ,var1 (,binop ,var1 ,n))
           (guard (number? n))
           (var? var1)
           (binop? binop)
           (int32? n)]
          [(set! ,var1 (,binop ,var1 ,var2))
           (binop? binop)
           (var? var2)
           (var? var1)]
          ))

    (match program
      [(begin ,[statement -> st] ...) program]
      [,x (error who "invalid syntax for Program: expected (begin stmt+)") ]
      ))

(define-who (generate-x86-64 exp)
    
    (define-who (emit-boilerplate exp)
      (begin
        (printf ".globl _scheme_entry")
        (emit-label "_scheme_entry")
        exp
        (emit 'ret)))

    (define-who (statement exp)
      exp)
    (match exp
      [(begin ,st* ...) (emit-program (for-each statement st*))]
      [,exp (error who "unexpected program ~s" exp) ]
      ))

) ;; End Library