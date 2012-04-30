;; normalize-context.ss
;;
;; part of p423-sp12/srwaggon-p423
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in A11
;; 2012 / 4 / 29
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; revised in A11
;; 2012 / 4 / 29

#!chezscheme
(library (compiler normalize-context)
  (export normalize-context)
  (import
   ;; Load Chez Scheme primitives:
   (chezscheme)
   ;; Load compiler framework:
   (framework match)
   (framework helpers)
   (compiler helpers)
   )

#| normalize-context : program --> program
|| The goal of normalize-context is to recognize the three distinct contexts below and to
|| rewrite the code so that each kind of expression appears only in contexts that are
|| appropriate for that expression.
||
|| On input to this pass, any expression can appear in any of three contexts:
||
|| o effect, where the resulting value is not needed, e.g., all but the last
||   subexpression of a begin;
||
|| o predicate, where the expression determines flow of control, e.g., the test part of an if;
||
|| o value, where the value is needed, e.g., the right-hand side of a let.
|#
(define-who (normalize-context program)

  #|
  ||
  |#
  (define (Value value)
    (match value
      [(alloc ,[Value -> vl]) `(alloc ,vl)]
      [(begin ,[Effect -> ef*] ... ,[vl]) (make-begin `(,ef* ... ,vl))]
      [(if ,[Pred -> pr] ,[c] ,[a]) `(if ,pr ,c ,a)]
      [(let ([,uv* ,[Value -> vl*]] ...) ,[vl]) `(let ([,uv* ,vl*] ...) ,vl)]
      [(,binop ,[Value -> vl] ,[Value -> vl^]) (guard (binop? binop)) `(,binop ,vl ,vl^)]
      [(,[rator] ,[rand*] ...) `(,rator ,rand* ...)]
      [,tr (guard (triv? tr)) tr]
      [,else (invalid who 'Value else)]
      ))

  #|
  ||
  |#
  (define (Effect effect)
    (match effect
      [(nop) '(nop)]
      [(begin ,[Effect -> ef*] ... ,[ef]) (make-begin `(,ef* ... ,ef))]
      [(if ,[Pred -> pr] ,[c] ,[a]) `(if ,pr ,c ,a)]
      [(let ([,uv* ,[Value -> vl*]] ...) ,[ef]) `(let ([,uv* ,vl*] ...) ,ef)]
      [(mset! ,[Value -> base] ,[Value -> offset] ,[Value -> vl]) `(mset! ,base ,offset ,vl)]
      [(,[Value -> rator] ,[Value -> rand*] ...) `(,rator ,rand* ...)]
      [,else (invalid who 'Effect else)]
      ))

  #|
  ||
  |#
  (define (Pred pred)
    (match pred
      [(true) '(true)]
      [(false) '(false)]
      [(begin ,[Effect -> ef*] ... ,[pr]) (make-begin `(,ef* ... ,pr))]
      [(if ,[Pred -> pr] ,[c] ,[a]) `(if ,pr ,c ,a)]
      [(let ([,uv* ,[Value -> vl*]] ...) ,[pr]) `(let ([,uv* ,vl*] ...) ,pr)]
      [(,relop ,[Value -> vl] ,[Value -> vl^]) (guard (relop? relop)) `(,relop ,vl ,vl^)]
      [,else (invalid who 'Pred else)]
      ))

  #|
  ||
  |#
  (define (Program p)
    (match p
      [(letrec ([,label (lambda (,uvar* ...) ,[Value -> vl*])] ...) ,[Value -> vl])
       `(letrec ([,label (lambda (,uvar* ...) ,vl*)] ...) ,vl)]
      [,else (invalid who 'Program else)]
      ))

  (Program program)

)) ;; end library
