;; generate-x86-64.ss
;;
;; part of p423-sp12/srwaggon-p423 assign3
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in a1
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; 2012/2/13

#!chezscheme
(library (compiler generate-x86-64)
  (export generate-x86-64)
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (framework match)
    (framework helpers)
  )


#| generate-x86-64 : pseudo-assembly-exp --> assembly-exp
 | generate-x86-64 takes an expression which closely resembles 
 | assembly and converts it into x86-64 assembly code.
 |#
(define-who (generate-x86-64 exp)
  (define (Code code)

    (define (binop->instr binop)
      (match binop
        [+ 'addq]
        [- 'subq]
        [* 'imulq]
        [logand 'andq]
        [logor 'orq]
        [sra 'sarq]
        [,x (errorf who "unexpected binop ~s" x)]
      )
    )
    
    (define (relop->instr relop not?)
      (if not?
          (match relop
            [<  'jge]
            [<= 'jg ]
            [=  'jne]
            [>= 'jl ]
            [>  'jle]
            [,x (errorf who "unexpected relop: ~s" x)]
          )
          (match relop
            [<  'jl ]
            [<= 'jle]
            [=  'je ]
            [>= 'jge]
            [>  'jg ]
            [,x (errorf who "unexpected relop: ~s" x)]
          )
      )
    )
  
    (define relops '(< <= = >= >))

    (match code
      [(jump ,label) (emit-jump 'jmp label)]
      [(set! ,dst (,binop ,dst ,src)) (emit (binop->instr binop) src dst)]
      [(set! ,dst ,src) (guard (label? src)) (emit 'leaq src dst)]
      [(set! ,dst ,src) (emit 'movq src dst)]
      [(if (not (,relop ,x ,y)) (jump ,dst))
       (emit 'cmpq y x)
       (emit-jump (relop->instr relop #t) dst)]
      [(if (,relop ,x ,y) (jump ,dst))
       (emit 'cmpq y x)
       (emit-jump (relop->instr relop #f) dst)]
      [,label (guard (label? label)) (emit-label label)]
      [,else (errorf who "unexpected statement ~S" else) else]
    )
  )

  (match exp
    [(code ,code* ...) (emit-program (for-each Code code*))]
    [,else (errorf who "unexpected program ~s" else)]
  )
)


) ;; End Library.