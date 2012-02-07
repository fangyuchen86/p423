;; passes.ss
;;
;; part of p423-sp12/srwaggon-p423 assign3
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; 2012/2/4

#!chezscheme
(library (compiler passes)
  (export
    verify-scheme
    finalize-locations
    expose-frame-var
    expose-basic-blocks
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


#| verify-scheme : program --> program
 | verify-scheme takes an expression representing a program and verifies
 | that it is an expression consiting solely of the provided language.
 | A descrition of the language is as follows.

 | Defiant to scheme unquote syntax (or whatever it's called),
 | unquotes here signify a member also found within the language.
 | Consecutive unquoted members are not necessarily the same member,
 | so much as the same part of the grammar.

Program   -->  (letrec ([<label> (lambda () ,Body)]*) ,Body)

Body      -->  (locate ([<uvar> ,Loc]*) ,Tail)

Tail      -->  (,Triv)
           |   (if ,Pred ,Tail ,Tail)
           |   (begin ,Effect* ,Tail)

Pred      -->  (true)
           |   (false)
           |   (,Relop ,Triv ,Triv)
           |   (if ,Pred ,Pred ,Pred)
           |   (begin ,Effect* ,Pred)

Effect    -->  (nop)
           |   (set! ,Var ,Triv)
           |   (set! ,Var (,Binop ,Triv ,Triv))
           |   (if ,Pred ,Effect ,Effect)
           |   (begin ,Effect* ,Effect)

Triv      -->  ,Var | <integer> | <label>

Var       -->  ,<uvar> | ,Loc

Loc       -->  ,Register | <frame variable>

Register  -->  rax | rcx | rdx | rbx | rbp | rsi | rdi
           |   r8 | r9 | r10 | r11 | r12 | r13 | r14 | r15

Binop     -->  + | - | * | logand | logor | sra

Relop     -->  < | <= | = | >= | >

 | If the program matches the language, the expression is returned.
 |#
(define-who (verify-scheme program)
  
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
    (or (uvar? exp) (loc? exp))
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
   | Effect --> (nop)
   |   (set! ,Var ,Triv)
   |   (set! ,Var (,Binop ,Triv ,Triv))
   |   (if ,Pred ,Effect ,Effect)
   |   (begin ,Effect* ,Effect)
   |#
  (define (Effect exp)
    (match exp
      [(nop) exp]
      [(set! ,v ,t) (guard (var? v) (triv? t)
                    #| ARCHITECTURE SPECIFIC CONSTRAINTS |#
                            ; v & t cannot both be frame-vars
                           (not (and (frame-var? v) (frame-var? t)))
                           ; labels only fit in registers
                           (if (label? t) (register? v))
                           ; ints must be 32bit or 
                           (if (integer? t) (or (int32? t)
                                                ; int64's only fit into registers
                                                (and (register? v) (int64? t)))) 
                    )
       exp]
      [(set! ,v (,b ,t1 ,t2)) (guard (var? v) (binop? b) (triv? t2)
                              #| ARCHITECTURE SPECIFIC CONSTRAINTS |#
                                     ; (set! v (b t t0)) :: valid iff v equals t
                                     (eq? v t1)
                                     ; t & t0 cannot both be frame-vars
                                     (not (and (frame-var? t1) (frame-var? t2)))
                                     ; no labels as operands to binops
                                     (not (or (label? t1) (label? t1)))
                                     ; Integer operands of binary operations must be
                                     ; an exact integer -2^31 ≤ n ≤ 2^31 - 1
                                     (if (number? t1) (and (int32? t1) (exact? t1)))
                                     (if (number? t2) (and (int32? t2) (exact? t2)))
                                     ; result from * operator must go into a register
                                     (if (eq? b '*) (or (register? v) (uvar? v)))
                                     ; whatever.
                                     (if (eq? b 'sra) (and (<= 0 t2) (>= 63 t2)))
                              )
       exp]
      [(if ,[Pred -> p] ,[Effect -> e0] ,[Effect -> e1]) exp]
      [(begin ,[Effect -> e*] ... ,[Effect -> e]) exp]
      [,x (errorf who "invalid effect: ~s" x) x]
    )
  )

  #| Pred : exp --> void
   | Pred takes an expression and throws an error
   | unless the expression qualifies as a predicate.
   |
   | Pred --> (true)
   |   (false)
   |   (,Relop ,Triv ,Triv)
   |   (if ,Pred ,Pred ,Pred)
   |   (begin ,Effect* ,Pred)
   |#
  (define (Pred exp)
    (match exp
      [(true) (values)]
      [(false) (values)]
      [(,r ,t0 ,t1) (guard (relop? r)
                           (triv? t0)
                           (triv? t1)
                    )
       (values)]
      [(if ,[p0] ,[p1] ,[p2]) (values)]
      [(begin ,[Effect -> e*] ... ,[p]) exp]
      [,x (errorf who "invalid pred: ~s" x)]
    )
  )

  #| Tail : env --> procedure : exp --> void
   | Tail is a curried procedure which takes an
   | environment (list of existing labels) and
   | returns a procedure which
   | takes an expression and throws an error
   | unless the expression qualifies as a tail.
   |
   | Tail --> (,Triv)
   |       |  (if ,Pred ,Tail ,Tail)
   |       |  (begin ,Effect* ,Tail)
   |#
  (define (Tail env)
    (lambda (exp)
      (match exp
        [(,t) (guard (triv? t)
                ; Labels must be bound
                (if (label? t) (and (member t env) #t))
                ; architectural nuance.  Jump must be to label, not address.
                (not (integer? t)) 
              )
         (values)]
        [(if ,[Pred -> p] ,[(Tail env) -> t0] ,[(Tail env) -> t1]) exp]
        [(begin ,[Effect -> e*] ... ,[(Tail env) -> t]) exp]
        [,x (errorf who "invalid tail: ~s" x) x]
      )
    )
  )

  #| Body : exv --> procedure : exp --> void
   | Body is a curried procedure which takes a
   | label environment and returns a procedure
   | which takes an expression and throws an
   | error unless the expression qualifies as
   | a valid body.
   | 
   | Body --> (locate ([<uvar> ,Loc]*) ,Tail)
   |#
  (define (Body label-env)
    (lambda (exp)
      (match exp
        [(locate ([,uvar ,loc] ... ) ,[(Tail label-env) -> t]) exp]
        [,x (errorf who "invalid body: ~s" x)]
      )
    )
  )

  #| Program : exp --> void
   | Program takes an expression and throws
   | an error unless the expression is a
   | valid fully-formed program according to
   | the grammar.
   |
   | Program --> (letrec ([<label> (lambda () ,Body)]*) ,Body)
   |#
  (define (Program exp)
    (match exp
      [(letrec ([,lbl (lambda () ,bn)] ...) ,b0)
       (let ([env (cons 'r15 lbl)])
         (for-each (Body env) bn)
         ((Body env) b0)
         (if (set? (map string->number (map extract-suffix lbl)))
             exp
             (errorf who "Label suffixes must be unique: ~s" lbl)))]
      [,x (errorf who "invalid syntax for Program: expected (letrec ([<label> (lambda () ,Body)]*) ,Body) but received ~s" x) x]
    )
  )
  (Program program)
)

#| finalize-locations : exp --> exp
 | finalize-locations takes a valid Scheme expression
 | and replaces each occurrence of a uvar in the body
 | of each locate form with the corresponding Loc.
 | As it does so, it removes useless assignments, i.e.,
 | converts any assignments (set! ,x ,y) to (nop)
 | if x and y resolve to the same location.  It
 | also discareds the locate form.
 |#
(define (finalize-locations program)
  
  (define (finalize-loc env)
    (lambda (exp)
      (match exp
        [,uvar (guard (uvar? uvar)) (cadr (assq  uvar env))]
        [(set! ,[loc] (,binop ,[triv0] ,[triv1])) `(set! ,loc (,binop ,triv0 ,triv1))]
        [(set! ,[loc] ,[triv]) (if (eq? loc triv) `(nop) `(set! ,loc ,triv))]
        [(if ,[pred] ,[conseq] ,[altern]) `(if ,pred ,conseq ,altern)]
        [(begin ,[effect*] ... ,[effect]) `(begin ,effect* ... ,effect)]
        [(,relop ,[triv0] ,[triv1]) `(,relop ,triv0 ,triv1)]
        [,x x]
      )
    )
  )

  ;; optimize (set! ,x ,y)
  (define (Body body)
    (match body
      [(locate ,env ,tail)
       ((finalize-loc env) tail)
      ]
    )
  )

  (match program
    [ (letrec ([,lbl (lambda () ,[Body -> bn])] ...) ,[Body -> b0])
     `(letrec ([,lbl (lambda () ,bn)] ...) ,b0)
    ]
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


#| expose-basic-blocks : exp --> pain and suffering
 | expose-basic-blocks takes an expression and a programmer's
 | soul and converts the programmer's free time into work
 | as well as life into one of unhappiness.
 |#
(define-who (expose-basic-blocks program)

  (define (expose-tail tail)
    (match tail
      [(if ,pred ,tail0 ,tail1)
       (let ([clbl0 (unique-label 'c)]
             [albl0 (unique-label 'a)])
         (let-values ([(pexpr pbinds) (expose-pred pred clbl0 albl0)]
                      [(cexpr cbinds) (expose-tail tail0)]
                      [(aexpr abinds) (expose-tail tail1)])
           (values pexpr `(,pbinds ...
                           [,clbl0 (lambda () ,cexpr)]
                           ,cbinds ...
                           [,albl0 (lambda () ,aexpr)]
                           ,abinds ...
                           ))))]
      [(begin ,effect* ... ,tail)
       (let*-values ([(texpr tbinds) (expose-tail tail)]
                     [(eexpr ebinds) (expose-effect* effect* `(,texpr))])
         (values eexpr `(,ebinds ... ,tbinds ...)))]
      [(,triv)
       (values `(,triv) '())]
      [,x (errorf who "unexpected tail: ~s" x) x]
    )
  )

  (define (expose-pred pred clbl albl)
    (match pred
      [(true)
       (values `(,clbl) '())]
      [(false)
       (values `(,albl) '())]
      [(,relop ,r0 ,r1) (guard (relop? relop))
       (values `(if (,relop ,r0 ,r1) (,clbl) (,albl)) '())]
      [(if ,pred ,conseq ,altern)
       (let ([clbl0 (unique-label 'c)]
             [albl0 (unique-label 'a)])
         (let-values ([(pexpr pbinds) (expose-pred pred clbl0 albl0)]
                      [(cexpr cbinds) (expose-pred conseq clbl albl)]
                      [(aexpr abinds) (expose-pred altern clbl albl)])
           (values pexpr
                   `(,pbinds ...
                    [,clbl0 (lambda () ,cexpr)]
                    ,cbinds ...
                    [,albl0 (lambda () ,aexpr)]
                    ,abinds ...
                    ))))]
      [(begin ,effect* ... ,pred)
       (let*-values ([(pexp pbinds) (expose-pred pred clbl albl)]
                     [(eexp ebinds) (expose-effect* effect* `(,pexp))])
         (values eexp `(,ebinds ... ,pbinds ...)))]
      [,x (errorf who "unexpected pred: ~s" x) x]
    )
  )

  (define (expose-effect* effect* acc)
    (match effect*
      [() (values (make-begin acc) '())]
      [(,e* ... ,e) (expose-effect e* e acc)]
      [,x (errorf who "unexpected effect*: ~s" x) x]
    )
  )

  (define (expose-effect effect* effect acc)
    (match effect
      [(nop) (expose-effect* effect* acc)]
      [(set! . ,x) (expose-effect* effect* `((set! . ,x) ,acc ...))]
      [(if ,pred ,conseq ,altern)
       (let ([clbl (unique-label 'c)]
             [albl (unique-label 'a)]
             [jlbl (unique-label 'j)])
         (let*-values ([(pexpr pbinds) (expose-pred pred clbl albl)]
                       [(cexpr cbinds) (expose-effect '() conseq `((,jlbl)))]
                       [(aexpr abinds) (expose-effect '() altern `((,jlbl)))]
                       [(eexpr ebinds) (expose-effect* effect* `(,pexpr))])
           (values eexpr
                   `(,ebinds ...
                     ,pbinds ...
                     [,clbl (lambda () ,cexpr)]
                     ,cbinds ...
                     [,albl (lambda () ,aexpr)]
                     ,abinds ...
                     [,jlbl (lambda () ,(make-begin acc))])
                   )))]
      [(begin ,e* ...) (expose-effect* (append effect* e*) acc)]
      [,x (errorf who "unexpected effect: ~s" x) x]
    )
  )

  (define (Program program)
    (match program
      [(letrec ([,label* (lambda () ,[expose-tail -> texp* tbinds*])] ...) ,[expose-tail -> texp tbinds])
       `(letrec ([,label* (lambda () ,texp*)] ... ,tbinds* ... ... ,tbinds ...) ,texp)]
      [,x (errorf who "unexpected program: ~s" x) x]
    )
  )
  (Program program)
)


#| flatten-program : scheme-exp --> pseudo-assmebly-exp
 | flatten-program takes a scheme expression and 'flattens' it,
 | converting it into a pseudo-assembly code expression without nesting
 | with which it is easier to convert directly into x86-64 assembly.
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