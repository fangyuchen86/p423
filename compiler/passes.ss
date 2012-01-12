;; passes.ss
;;
;; part of p423-sp12/srwaggon-p423
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; 2012/1/11

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

(library (compiler passes)
  (export verify-scheme generate-x86-64)
  (import
   ;; Load Chez Scheme primitives:
   (chezscheme)
   ;; Load compiler framework:
   (framework match)
   (framework helpers))
  
(define (driver program)
  (with-output-to-file "t.s"
    (lambda ()
      (generate-x86-64 (verify-scheme program)))))

(define-who verify-scheme
  (lambda (program)
    
    (define-who binop?
      (lambda (exp)
        (define binops `(+ - *))
        (unless (and (memq exp binops) #t)
          (error who "invalid binop: ~s" exp))))
    
    (define-who statement
      (lambda (exp)
        (match exp
               [(set! ,var1 ,n)
                (guard (int64? n))
                                        ;(register?)
                ]
               [(set! ,var1 ,var2)
                                        ;guard
                                        ;evaluate
                ]
               [(set! ,var1 (,binop ,var1 ,n))
                (guard (int32? n))
                                        ;evaluate
                ]
               [(set! ,var1 (,binop ,var1 ,var2))
                                        ;guard
                                        ;evaluate
                ]
               ))))
  
  (lambda (exp)
    (match exp
           [(begin ,[statement -> st] ...) exp]
           [,x (error "invalid syntax for Program: expected (begin stmt+)") ]
           )))

(define-who generate-x86-64
  (lambda (exp)

    (define-who emit-boilerplate
      (lambda (exp)
        (begin
          (emit '.globl "_scheme_entry")
          (emit-label "_scheme_entry")
          code
          (emit 'ret))))


    (define-who statement
      (lambda (exp)
        ))
    (match exp
           [(begin ,st* ...) (emit-boilerplate (for-each statement st*))]
           [,exp (error who "unexpected program ~s" exp) ]
           )))

) ;; End Library