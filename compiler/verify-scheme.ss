(library (compiler verify-scheme)
  (export verify-scheme)
  (import
    (chezscheme)
    (framework match)
    (framework helpers))

;;; verify-scheme accept a single value and verifies that the value
;;; is a valid program in the current source language.
;;;
;;; Grammar for verify-scheme (assignment 4):
;;;
;;; Program --> (letrec ((<label> (lambda () <Body>))*) <Body>)
;;; Body    --> (locals (<uvar>*) <Tail>)
;;; Tail    --> (<Triv> <Var>*)
;;;	  |  (begin <Effect>* <Tail>)
;;;	  |  (if <Pred> <Tail> <Tail>)
;;; Pred    --> (true)
;;;	  |  (false)
;;;	  |  (<relop> <Triv> <Triv>)
;;;	  |  (begin <Effect*> <Pred>)
;;;	  |  (if <Pred> <Pred> <Pred>)
;;; Effect  --> (nop)
;;;	  |  (set! <Var> <Triv>)
;;;	  |  (set! <Var> (<binop> <Triv> <Triv>))
;;;	  |  (begin <Effect>+)
;;;	  |  (if <Pred> <Pred> <Pred>)
;;; Var     --> <uvar>
;;;	  |  <frame-var>
;;;	  |  <register>
;;; Triv    --> <Var>
;;;	  |  <int>
;;;	  |  <label>
;;;
;;; Where uvar is symbol.n where (n >= 0)
;;;	  binop is +, -, *, logand, logor, or sra
;;;	  relop is <, <=, or =
;;;	  register is rax, rcx, rdx, rbx, rbp, rdi, rsi, r8,
;;;		   r9, r10, r11, r12, r13, r14, or r15
;;;	  label is symbol$n where (n >= 0)
;;;	  frame-var is fvn where (n >= 0)
;;;
;;; If the value is a valid program, verify scheme returns the value
;;; unchanged; otherwise it signals an error.
;;;
;;; At this level in the compiler verify-scheme no longer checks machine
;;; constraints, as select-instructions should now perform instruction
;;; selection and correctly select which instruction to use based on the
;;; machine constraints.
;;;

  (define-who verify-scheme
    (define verify-x-list
      (lambda (x* x? what)
        (let loop ([x* x*] [idx* '()])
          (unless (null? x*)
            (let ([x (car x*)] [x* (cdr x*)])
              (unless (x? x)
                (errorf who "invalid ~s ~s found" what x))
              (let ([idx (extract-suffix x)])
                (when (member idx idx*)
                  (errorf who "non-unique ~s suffix ~s found" what idx))
                (loop x* (cons idx idx*))))))))
    (define Var
      (lambda (uvar*)
        (lambda (var)
          (unless (or (register? var) (frame-var? var) (uvar? var))
            (errorf who "invalid variable ~s" var))
          (when (uvar? var)
            (unless (memq var uvar*)
              (errorf who "unbound uvar ~s" var)))
          var)))
    (define Triv
      (lambda (label* uvar*)
        (lambda (t)
          (unless (or (register? t) (frame-var? t) (label? t) (uvar? t)
                      (and (integer? t) (exact? t)))
            (errorf who "invalid Triv ~s" t))
          (when (and (integer? t) (exact? t))
            (unless (int64? t)
              (errorf who "integer out of 64-bit range ~s" t)))
          (when (uvar? t)
            (unless (memq t uvar*)
              (errorf who "unbound uvar ~s" t)))
          (when (label? t)
            (unless (memq t label*)
              (errorf who "unbound label ~s" t)))
          t)))
    (define Pred
      (lambda (label* uvar*)
        (lambda (pr)
          (match pr
            [(true) (void)]
            [(false) (void)]
            [(begin ,[(Effect label* uvar*) -> ef*] ... ,[pr]) (void)]
            [(if ,[test] ,[conseq] ,[altern]) (void)]
            [(,relop ,[(Triv label* uvar*) -> x]
               ,[(Triv label* uvar*) -> y])
             (unless (memq relop '(= < <= > >=))
               (errorf who "invalid predicate operator ~s" relop))]
            [,pr (errorf who "invalid Pred ~s" pr)]))))
    (define Effect
      (lambda (label* uvar*)
        (lambda (ef)
          (match ef
            [(nop) (void)]
            [(set! ,[(Var uvar*) -> x]
               (sra ,[(Triv label* uvar*) -> y]
                 ,[(Triv label* uvar*) -> z]))
             (unless (uint6? z)
               (errorf who "invalid attempt to sra by ~s" z))]
            [(set! ,[(Var uvar*) -> x]
               (,binop ,[(Triv label* uvar*) -> y]
                 ,[(Triv label* uvar*) -> z]))
             (unless (memq binop '(+ - logand logor * sra))
               (errorf who "invalid effect operator ~s" binop))]
            [(set! ,[(Var uvar*) -> x] ,[(Triv label* uvar*) -> y]) (void)]
            [(begin ,[ef] ,[ef*] ...) (void)]
            [(if ,[(Pred label* uvar*) -> test] ,[conseq] ,[altern])
             (void)]
            [,ef (errorf who "invalid Effect ~s" ef)]))))
    (define Tail
      (lambda (label* uvar*)
        (lambda (tail)
          (match tail
            [(begin ,[(Effect label* uvar*) -> ef*] ... ,[tail]) (void)]
            [(if ,[(Pred label* uvar*) -> test] ,[conseq] ,[altern])
             (void)]
            [(,[(Triv label* uvar*) -> t] ,[(Var uvar*)-> live-out*] ...)
             (unless (andmap
                       (lambda (x)
                         (or (frame-var? x) (register? x))) live-out*)
               (errorf who
                 "live out list contains invalid variable ~s" live-out*))
             (when (integer? t)
               (errorf who "~s attempt to apply integer" `(,t)))]
            [,tail (errorf who "invalid Tail ~s" tail)]))))
    (define Body
      (lambda (label*)
        (lambda (bd)
          (match bd
            [(locals (,uvar* ...) ,tail)
             (verify-x-list `(,uvar* ...) uvar? 'uvar)
             ((Tail label* uvar*) tail)]
            [,bd (errorf who "invalid Body ~s" bd)]))))
    (lambda (x)
      (match x
        [(letrec ([,label* (lambda () ,bd*)] ...) ,bd)
         (verify-x-list label* label? 'label)
         (for-each (Body label*) bd*)
         ((Body label*) bd)]
        [,x (errorf who "invalid Program ~s" x)])
      x))

  )