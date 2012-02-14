;; verify-scheme.ss
;;
;; part of p423-sp12/srwaggon-p423 assign4
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; 2012/2/13

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

  Program   -->  (letrec ([,label (lambda () ,Body)]*) ,Body)

  Body      -->  (locals (,uvar*) ,Tail)

  Tail      -->  (,Triv ,Loc*)
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

  Triv      -->  ,Var | ,integer | ,label

  Var       -->  ,uvar | ,Loc

  Loc       -->  ,Register | ,frame-var

  Register  -->  rax | rcx | rdx | rbx | rbp | rsi | rdi
  |   r8 | r9 | r10 | r11 | r12 | r13 | r14 | r15

  Binop     -->  + | - | * | logand | logor | sra

  Relop     -->  < | <= | = | >= | >

  | If the program matches the language, the expression is returned.
  |#

(define-who (verify-scheme program)

  (define (verify-x-list x* x? what)
    (let loop ([x* x*] [id* '()])
      (unless (null? x*)
        (let ([x (car x*)])
          (error-unless (x? x) who "invalid ~s ~s" what x)
          (let ([id (extract-suffix x)])
            (error-when (member id id*)  who "non-unique ~s suffix ~s" what id)
            (loop (cdr x*) (cons id id*)))))))
  
  
  (define (Loc loc)
    (error-unless (or (register? loc) (frame-var? loc)) who "invalid Loc ~s" loc) loc)
  
  (define (Var uvar*)
    (lambda (v)
      (when (uvar? v)
        (error-unless (memq v uvar*) "unbound uvar ~s" v))
      ;; specific optimism for a4: that uvars are uvars
      (unless (or (uvar? v) (loc? v)) (invalid who 'Var v))
      v))
  
  (define (Triv lbl* uvar*)
    (lambda (t)
      (when (uvar? t)
        (error-unless (memq t uvar*) who "unbound uvar ~s" t))
      (when (label? t)
        (error-unless (memq t lbl*) who "unbound lable ~s" t))
      (unless (triv? t) (invalid who 'Triv t))
      t))
  
  (define (Effect lbl* uvar*)
    (lambda (exp)
      (match exp
        [(nop) exp]

        [(set! ,[(Var uvar*) -> v] (,b ,[(Triv lbl* uvar*) -> t1] ,[(Triv lbl* uvar*) -> t2]))
         (guard (or (binop? b) (relop? b)))
         #| ARCHITECTURE SPECIFIC CONSTRAINTS |#
         (error-unless (equal? v t1)  who "machine constraint violation: var (~s) must equal first triv (~s): ~s" v t1 exp)
         (error-when (and (frame-var? t1) (frame-var? t2)) who "machine constraint violation: both trivs (~s ~s) cannot be frame vars: ~s" t1 t2 exp)
         (error-when (or (label? t1) (label? t1)) who "machine constraint violation: labels not allowed as operands to binops: ~s" exp)
         (when (number? t1)
           (error-unless (and (int32? t1) (exact? t1)) who "machine constraint violation: Integer operands of binary operations must be an exact integer -2^31 ≤ n ≤ 2^31 - 1: ~s" exp))
         (when (number? t2)
           (error-unless (and (int32? t2) (exact? t2)) who "machine constraint violation: Integer operands of binary operations must be an exact integer -2^31 ≤ n ≤ 2^31 - 1: ~s" exp))
         (when (eq? b '*)
           (error-unless (or (register? v) (uvar? v)) who "machine constraint violation: * operator result must go directly into a register: ~s" exp))
         (when (eq? b 'sra)
           (error-unless (and (<= 0 t2) (>= 63 t2)) who "machine constraint violation: second operand of sra operator must be 0 ≤ x ≤ 63: ~s" exp))
         exp]
        
        [(set! ,[(Var uvar*) -> v] ,[(Triv lbl* uvar*) -> t])
         #| ARCHITECTURE SPECIFIC CONSTRAINTS |#
         (error-when (and (frame-var? v) (frame-var? t)) who "machine constraint violation: both trivs cannot be frame vars: ~s" exp)
         (when (label? t)
           (error-unless (or (register? v) (uvar? v))  who "machine constraint violation: labels only fit into registers: ~s" exp))
         (when (integer? t)
           (error-unless (or (int32? t) (and (or (register? v) (uvar? v)) (int64? t))) who "machine constraint violation: 64bit ints only fit into registers, other ints must be 32: ~s" exp))
           exp]
        #;[(set! ,[(Var uvar*) -> v] (,b ,[(Triv lbl* uvar*) -> t1] ,[(Triv lbl* uvar*) -> t2]))
         (guard (or (binop? b) (relop? b))) exp]
        #;[(set! ,[(Var uvar*) -> v] ,[(Triv lbl* uvar*) -> t]) exp]
        [(if ,[(Pred lbl* uvar*) -> p] ,[(Effect lbl* uvar*) -> e0] ,[(Effect lbl* uvar*) -> e1]) exp]
        [(begin ,[(Effect lbl* uvar*) -> e*] ... ,[(Effect lbl* uvar*) -> e]) exp]
        [,else (invalid who 'Effect else)])))
  
  (define (Pred lbl* uvar*)
    (lambda (exp)
      (match exp
        [(true) exp]
        [(false) exp]
        [(begin ,[(Effect lbl* uvar*) -> e*] ... ,[p]) (void)]
        [(if ,[p0] ,[p1] ,[p2]) exp]
        [(,r ,[(Triv lbl* uvar*) -> t0] ,[(Triv lbl* uvar*) -> t1])
         (error-unless (relop? r) who "invalid relop: ~s" exp)
         ;; specific optimism for a4
         (error-unless (or (and (or (register? t0) (uvar? t0))
                                (or (register? t1)
                                    (frame-var? t1)
                                    (int32? t1)
                                    (uvar? t1)))
                           (and (frame-var? t0)
                                (or (register? t1)
                                    (uvar? t1)
                                    (int32? t1))))
                       who "machine constraint violation: ~s" exp)]
        [,else (invalid who 'Pred else)])))
  
  (define (Tail lbl* uvar*)
    (lambda (exp)
      (match exp
        [(if ,[(Pred lbl* uvar*) -> p] ,[(Tail lbl* uvar*) -> t0] ,[(Tail lbl* uvar*) -> t1]) (void)]
        [(begin ,[(Effect lbl* uvar*) -> e*] ... ,[(Tail lbl* uvar*) -> t]) (void)]
        [(,[(Triv lbl* uvar*) -> t] ,[Loc -> loc*] ...)
         (error-when (integer? t) who "machine constraint violation: jump must be to label, not address: ~s" exp)]
        [,else (invalid who 'Tail else)])))
  
  (define (Body lbl*)
    (lambda (exp)
      (match exp
        [(locals (,uvar* ...) ,tail)
         (verify-x-list `(,uvar* ...) uvar? 'uvar)
         ((Tail lbl* uvar*) tail)]
        [,else (invalid who 'Body else)])))
  
  (define (Program exp)
    (match exp
      [(letrec ([,lbl* (lambda () ,bn)] ...) ,b0)
       (verify-x-list lbl* label? 'label)
       ((Body lbl*) b0)
       (for-each (Body lbl*) bn)]
      [,else (invalid who 'Program else)])
    exp)
  (Program program))

) ;; End Library.137
