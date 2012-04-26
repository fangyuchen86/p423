;; uncover-locals.ss
;;
;; part of p423-sp12/srwaggon-p423
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in A9
;; 2012 / 4 / 25
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; revised in A9
;; 2012 / 4 / 25

#!chezscheme
(library (compiler uncover-locals)
  (export uncover-locals)
  (import
   ;; Load Chez Scheme primitives:
   (chezscheme)
   ;; Load compiler framework:
   (framework match)
   (framework helpers)
   (compiler helpers)
   )

#| uncover-locals : program -> program
|| uncover-locals scans through each lambda or letrec body to
|| find all variables bound by let expressions within the body
|| and records those variables in a locals form wrapped around
|| the body
|#
(define-who (uncover-locals program)
  
  (define (Triv triv)
    (match triv
      [,tr (guard triv?) '()]
      [,else (invalid who 'Triv else)]
      ))

  #|
  ||
  |#
  (define (Value value)
    (match value
      [(alloc ,[Value -> vl]) vl]
      [(begin ,[Effect -> ef*] ... ,[vl]) `(,ef* ... ... ,vl ...)]
      [(if ,[Pred -> pr] ,[c] ,[a]) `(,pr ... ,c ... ,a ...)]
      [(let ([,uv* ,[Value -> vl*]] ...) ,[vl]) `(,uv* ... ,vl* ... ... ,vl ...)]
      [(,binop ,[Value -> vl] ,[Value -> vl^]) (guard binop?) `(,vl ... ,vl^ ...)]
      [(,[rator] ,[rand*] ...) `(,rator ... ,rand* ... ...)]
      [,tr (guard triv?) '()]
      [,else (invalid who 'Valid else)]
      ))

  #|
  ||
  |#
  (define (Effect effect)
    (match effect
      [(nop) '()]
      [(begin ,[Effect -> ef*] ... ,[ef]) `(,ef* ... ... ,ef ...)]
      [(if ,[Pred -> pr] ,[c] ,[a]) `(,pr ... ,c ... ,a ...)]
      [(let ([,uv* ,[Value -> vl*]] ...) ,[ef]) `(,uv* ... ,vl* ... ... ,ef ...)]
      [(mset! ,[Value -> base] ,[Value -> offset] ,[Value -> vl]) `(,base ... ,offset ... ,vl ...)]
      [(set! ,uvar (,binop ,[Triv -> tr] ,[Triv -> tr^])) (guard (binop? binop))
       `(,tr ... ,tr^ ...)]
      [(set! ,uvar ,[Triv -> tr]) tr]
      [(,[Value -> rator] ,[Value -> rand*] ...) `(,rator ... ,rand* ... ...)]
      [,else (invalid who 'Effect else)]
      ))

  #|
  ||
  |#
  (define (Pred pred)
    (match pred
      [(true) '()]
      [(false) '()]
      [(begin ,[Effect -> ef*] ... ,[pr]) `(,ef* ... ... ,pr ...)]
      [(if ,[Pred -> pr] ,[c] ,[a]) `(,pr ... ,c ... ,a ...)]
      [(let ([,uv* ,[Value -> vl*]] ...) ,[pr]) `(,uv* ... ,vl* ... ... ,pr ...)]
      [(,relop ,[Triv -> tr] ,[Triv -> tr^]) (guard (relop? relop)) `(,tr ... ,tr^ ...)]
      [,else (invalid who 'Pred else)]
      ))

  #|
  ||
  |#
  (define (Tail tail)
    (match tail
      [(alloc ,[Value -> vl]) vl]
      [(begin ,[Effect -> ef*] ... ,[tl]) `(,ef* ... ... ,tl ...)]
      [(if ,[Pred -> pr] ,[c] ,[a]) `(,pr ... ,c ... ,a ...)]
      [(let ([,uv* ,[Value -> vl*]] ...) ,[tl]) `(,uv* ... ,vl* ... ... ,tl ...)]
      [(,binop ,[Triv -> tr] ,[Triv -> tr^]) (guard (binop? binop)) `(,tr ... ,tr^ ...)]
      [,tr (guard (triv? tr)) '()]
      [(,[Value -> rator] ,[Value -> rand*] ...) `(,rator ... ,rand* ... ...)]
      [,else (invalid who 'Tail else)]
      ))

  #|
  ||
  |#
  (define (Program p)
    (match p
      [(letrec ([,label (lambda (,uvar* ...) ,tl*)] ...) ,tl)
       `(letrec ([,label (lambda (,uvar* ...) (locals ,(map Tail tl*) ,tl*))] ...) (locals ,(Tail tl) ,tl))]
      [,else (invalid who 'Program else)]
      ))

  (Program program)

)) ;; end library
