;; passes.ss
;;
;; part of p423-sp12/srwaggon-p423 assign2
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; 2012/1/18

#!chezscheme
(library (compiler passes)
  (export
    verify-scheme
    expose-frame-var
    flatten-program
    generate-x86-64
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (framework match)
    (framework helpers)
  )

#| verify-scheme : program --> program
 | verify-scheme takes an expression representing a program and verifies
 | that it is an expression consiting solely of the provided language.
 | A descrition of the language is as follows.

 | Defiant to scheme unquote syntax (or whatever it's called),
 | unquotes here signify a member also found within the language.
 | Consecutive unquoted members are not necessarily the same member,
 | so much as the same part of the grammar.

Program   -->  (letrec ([<label> (lambda () ,Tail)]*) ,Tail)
Tail      -->  (,Triv)
           |   (begin ,Effect* ,Tail)
Effect    -->  (set! ,Var ,Triv)
           |   (set! ,Var (,Binop ,Triv ,Triv))
Triv      -->  ,Var | <integer> | <label>
Var       -->  ,Register | <frame variable>
Register  -->  rax | rcx | rdx | rbx | rbp | rsi | rdi
           |   r8 | r9 | r10 | r11 | r12 | r13 | r14 | r15
Binop     -->  + | - | * | logand | logor | sra

 | If the program matches the language, the expression is returned.
 |#
(define-who (verify-scheme program)

  #| binop? : sym --> boolean
   | binop? takes a symbol and returns #t 
   | iff the symbol is a binary operation.
   | 
   | Binop --> + | - | * | logand | logor | sra
   |#
  (define (binop? exp)
    (define binops '(+ - * logand logor sra))
    (and (memq exp binops) #t)
  )

  #| var? : exp --> boolean
   | var? takes an expression and returns #t
   | iff the expression is a variable.
   |
   | Var --> ,Register | <frame variable>
   |#
  (define (var? exp)
    (or (register? exp) (frame-var? exp))
  )

  #| triv? : exp --> boolean
   | triv? takes an expression and returns #t
   | iff the expression is trivial.
   |
   | Triv --> ,Var | <integer> | <label>
   |#
  (define (triv? exp)
    (or (var? exp) (integer? exp) (label? exp))
  )

  #| Effect : exp --> void
   | Effect takes an expression and throws an error
   | unless the expression qualifies as an effect.
   |
   | Effect --> (set! ,Var ,Triv)
   |         |  (set! ,Var (,Binop ,Triv ,Triv))
   |#
  (define (Effect exp)
    (match exp
      [(set! ,v ,t)
       (guard (var? v) (triv? t)
              ; architecture specific constraints
              (not (and (frame-var? v) (frame-var? t))) ; v & t cannot both be frame-vars
              (if (label? t) (register? v)) ; labels only fit in registers
              (if (integer? t) (or (int32? t) ; ints must be 32bit or 
                                   (and (register? v) (int64? t)))) ; int64's only fit into registers
       )
       exp]
      [(set! ,v (,b ,t1 ,t2))
       (guard (var? v) (binop? b) (triv? t2)
              ; architecture specific constraints
              (eq? v t1) ; (set! v (b t t0)) :: valid iff v equals t
              (not (or (label? t1) (label? t1))) ; no labels as operands to binops
              (not (and (frame-var? t1) (frame-var? t2))) ; t & t0 cannot both be frame-vars
              ; Integer operands of binary operations must be an exact integer -2^31 ≤ n ≤ 2^31 - 1
              (if (number? t1) (and (int32? t1) (exact? t1)))
              (if (number? t2) (and (int32? t2) (exact? t2)))
              (if (eq? b '*) (register? v)) ; * must go into a register
              (if (eq? b 'sra) (and (<= 0 t2) (>= 63 t2))) ; whatever.
       )
       exp]
      [,x (errorf who "invalid effect: ~s" x)]
    )
  )

  #| Tail : exp --> void
   | Tail takes an expression and throws an error
   | unless the expression qualifies as a tail.
   |
   | Tail --> (,Triv)
   |       |  (begin ,Effect* ,Tail)
   |#
  (define (Tail exp)
    (match exp
      [(begin ,[Effect -> e*] ... ,[Tail -> t]) exp]
      [(,t) (guard (triv? t)
                   (not (integer? t)) ; architectural nuance.  Jump must be to label, not address.
            )
       (values)]
      [,x (errorf who "invalid tail: ~s" x)]
    )
  )

  #| This block acts as the heartbeat of verify-scheme
   | by doing the work naturally expected to be within
   | some helper 'verify-program'.
   |
   | Program --> (letrec ([<label> (lambda () ,Tail)]*) ,Tail)
   |#
  (match program
    [(letrec ([,lbl (lambda () ,[Tail -> t*])] ...) ,[Tail -> t])
     (if (set? (map string->number (map extract-suffix lbl)))
                    program
                    (errorf who "Label suffixes must be unique: ~s" lbl))]
    [,x (errorf who "invalid syntax for Program: expected (letrec ([<label> (lambda () ,Tail)]*) ,Tail) but received ~s" program)]
  )
)



#| expose-frame-var : exp --> exp
 | expose-frame-var takes an expression and tree-walks it
 | converting any occurrences of frame variables into 
 | displacement mode operands,
 | with rbp as the base register and an offset based on 
 | the frame variable's index.  Since our words are 8-bits
 | in length, the following pattern emerges:
 |
 | fv0 --> #<disp rbp 0>
 | fv1 --> #<disp rbp 8>
 | fv[i] --> #<disp rbp 8i>
 |#

(define (expose-frame-var sexp)
  (match sexp
    [,x (guard (frame-var? x)) (make-disp-opnd 'rbp (* 8 (frame-var->index x)))]
    [(,x . ,y) `(,(expose-frame-var x) . ,(expose-frame-var y))]
    [,else else]
  )
)


#| flatten-program : scheme-exp --> pseudo-assmebly-exp
 |
 |#

(define (flatten-program program)
  (match program
    [(letrec (,[block*] ...) ,[tail]) `(code ,tail ... ,block* ... ...)] ;program
    [[,label (lambda () ,[body])] `(,label ,body ...)] ;block
    [(begin ,effect* ... ,[tail]) `(,effect* ... ,tail ...)] ;tail
    [(,jump) ;(guard (or (label? jump) (register? jump))) `((jump ,jump))] ;jump
     `((jump ,jump))]
    [,x x]
  )
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

    (match code
      [(jump ,label) (emit-jump 'jmp label)]
      [(set! ,dst (,binop ,dst ,src)) (emit (binop->instr binop) src dst)]
      [(set! ,dst ,src) (guard (label? src)) (emit 'leaq src dst)]
      [(set! ,dst ,src) (emit 'movq src dst)]
      [,label (guard (label? label)) (emit-label label)]
      [,else (errorf who "unexpected statement ~S" else) else]
       ;x]
    )
  )

  (match exp
    [(code ,code* ...) (emit-program (for-each Code code*))]
    [,else (errorf who "unexpected program ~s" else)]
  )
)


) ;; End Library