;; verify-scheme.ss
;;
;; part of p423-sp12/srwaggon-p423 assign3
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; 2012/2/11

#!chezscheme
(library (compiler verify-scheme)
  (export verify-scheme)
  (import
   ;; Load Chez Scheme primitives:
   (chezscheme)
   ;; Load compiler framework:
   (framework match)
   (framework helpers)
   (compiler helpers)
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

   ;; idea stolen from a3 solution and by the transitive property, Kent.
   #| verify-x-list : x* x? what --> void
    | verify-x-list takes a list of symbols, a predicate which truly
    | identifies the symbols' type, and a symbol representing what
    | that type is and throws an error if any of the symbols are
    | invalid or non-unique.
    |#
    (define (verify-x-list x* x? what)
      (let loop ([x* x*] [id* '()])
        (unless (null? x*)
          (let ([x (car x*)])
            (unless (x? x)
              (errorf who "invalid ~s ~s" what x))
            (let ([id (extract-suffix x)])
              (when (member id id*)
                (errorf who "non-unique ~s suffix ~s" what id))
              (loop (cdr x*) (cons id id*)))))))

   #| Var->Loc : Var uvarEnv --> Loc
    | Var->Loc was written by R. Kent Dybvig and/or Andy Keep.
    | Var->Loc takes a Var and a uvar environment and,
    | if the Var occurs within the uvar environment, its
    | associated Loc is returned.
    |#
    (define (Var->Loc v env)
      (if (uvar? v) (cdr (assq v env)) v)
    )

    (define (Loc loc)
      (unless (or (register? loc) (frame-var? loc))
        (errorf who "invalid Loc ~s" loc))
      loc
    )


    (define (Var uvarEnv)
      (lambda (var)
        (match var
          [,v (guard (uvar? v))
              (unless (assq v uvarEnv)
                (errorf "unbound uvar ~s" v)) var]
          [,v (guard (loc? v)) var]
          [,else (errorf who "invalid Var ~s" var)]
        )
      )
    )

    (define (Triv label* uvarEnv)
      (lambda (exp)
        (match exp
          [,x (guard (uvar? exp))
              (unless (assq exp uvarEnv)
                (errorf who "unbound uvar ~s" exp)) exp]
          [,x (guard (label? exp))
              (unless (memq exp label*)
                (errorf who "unbound lable ~s" exp)) exp]
          [,x (guard (triv? exp)) exp]
          [,else (errorf who "invalid Triv ~s" exp)]
        )
      )
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
    (define (Effect label* uvarEnv)
      (lambda (exp)
        (match exp
          [(nop) exp]
          [(set! ,[(Var uvarEnv) -> v] (,b ,[(Triv label* uvarEnv) -> t1] ,[(Triv label* uvarEnv) -> t2]))
           (guard (or (binop? b) (relop? b)))
           (let ([v (Var->Loc v uvarEnv)][t1 (Var->Loc t1 uvarEnv)][t2 (Var->Loc t2 uvarEnv)])
             #| ARCHITECTURE SPECIFIC CONSTRAINTS |#
             (unless (equal? v t1)
               (errorf who "machine constraint violation: var must equal first triv: ~s ~s ~s" exp v t1))
             (when (and (frame-var? t1) (frame-var? t2))
               (errorf who "machine constraint violation: both trivs cannot be frame vars: ~s" exp))
             (when (or (label? t1) (label? t1))
               (errorf who "machine constraint violation: labels not allowed as operands to binops: ~s" exp))
             (when (number? t1)
               (unless (and (int32? t1) (exact? t1))
               (errorf who "machine constraint violation: Integer operands of binary operations must be an exact integer -2^31 ≤ n ≤ 2^31 - 1: ~s" exp)))
             (when (number? t2)
               (unless (and (int32? t2) (exact? t2))
                 (errorf who "machine constraint violation: Integer operands of binary operations must be an exact integer -2^31 ≤ n ≤ 2^31 - 1: ~s" exp)))
             (when (eq? b '*)
               (unless (or (register? v) (uvar? v))
                 (errorf who "machine constraint violation: * operator result must go directly into a register: ~s" exp)))
             (when (eq? b 'sra)
               (unless (and (<= 0 t2) (>= 63 t2))
                 (errorf who "machine constraint violation: second operand of sra operator must be 0 ≤ x ≤ 63: ~s" exp))))
           exp]
          [(set! ,[(Var uvarEnv) -> v] ,[(Triv label* uvarEnv) -> t])
           (let ([v (Var->Loc v uvarEnv)][t (Var->Loc t uvarEnv)])
             #| ARCHITECTURE SPECIFIC CONSTRAINTS |#
             (when (and (frame-var? v) (frame-var? t))
               (errorf who "machine constraint violation: both trivs cannot be frame vars: ~s" exp))
             (when (label? t)
               (unless (register? v)
                 (errorf who "machine constraint violation: labels only fit into registers: ~s" exp)))
             (when (integer? t)
               (unless (or (int32? t) (and (register? v) (int64? t)))
                 (errorf who "machine constraint violation: 64bit ints only fit into registers, other ints must be 32: ~s" exp)))
           )
           exp]
          [(if ,[(Pred label* uvarEnv) -> p] ,[(Effect label* uvarEnv) -> e0] ,[(Effect label* uvarEnv) -> e1]) exp]
          [(begin ,[(Effect label* uvarEnv) -> e*] ... ,[(Effect label* uvarEnv) -> e]) exp]
          [,else (errorf who "invalid effect: ~s" else)]
          )
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
    (define (Pred label* uvarEnv)
      (lambda (exp)
        (match exp
          [(true) exp]
          [(false) exp]
          [(,r ,[(Triv label* uvarEnv) -> t0] ,[(Triv label* uvarEnv) -> t1]) (guard (relop? r))
           (let ([t0 (Var->Loc t0 uvarEnv)][t1 (Var->Loc t1 uvarEnv)])
             ;; stolen from a3 solution
             (unless (or (and (register? t0)
                              (or (register? t1)
                                  (frame-var? t1)
                                  (int32? t1)))
                         (and (frame-var? t0)
                              (or (register? t1)
                                  (int32? t1))))
               (errorf who "machine constraint violation: ~s" exp)))
           ;; end stolen block
           exp]
          [(if ,[p0] ,[p1] ,[p2]) exp]
          [(begin ,[(Effect label* uvarEnv) -> e*] ... ,[p]) exp]
          [,else (errorf who "invalid Pred: ~s" else)]
          )
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
    (define (Tail label* uvarEnv)
      (lambda (exp)
        (match exp
          [(if ,[(Pred label* uvarEnv) -> p] ,[(Tail label* uvarEnv) -> t0] ,[(Tail label* uvarEnv) -> t1]) exp]
          [(begin ,[(Effect label* uvarEnv) -> e*] ... ,[(Tail label* uvarEnv) -> t]) exp]
          [(,[(Triv label* uvarEnv) -> t])
           (when (integer? t)
             (errorf who "machine constraint violation: jump must be to label, not address: ~s" exp))]
          [,else (errorf who "invalid Tail: ~s" else)]
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
    (define (Body label*)
      (lambda (exp)
        (match exp
          [(locate ([,uvar* ,[Loc -> loc*]]...) ,tail)
           (verify-x-list uvar* uvar? 'uvar)
           ((Tail label* (map cons uvar* loc*)) tail)
           ]
          [,else (errorf who "invalid Body: ~s" else)]
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
        [(letrec ([,label* (lambda () ,bn)] ...) ,b0)
         (verify-x-list label* label? 'label)
         ((Body label*) b0)
         (for-each (Body label*) bn)
         ]
        [,else (errorf who "invalid syntax for Program: expected (letrec ([<label> (lambda () ,Body)]*) ,Body) but received ~s" else)]
        )
      exp
      )
    (Program program)
    )
  
  ) ;; End Library.