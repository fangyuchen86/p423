;; FILENAME.ss
;;
;; part of p423-sp12/srwaggon-p423
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in A
;; 2012 /  /
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; revised in A
;; 2012 /  /

#!chezscheme
(library (compiler )
  (export )
  (import
   ;; Load Chez Scheme primitives:
   (chezscheme)
   ;; Load compiler framework:
   (framework match)
   (framework helpers)
   (compiler helpers)
   )

#|
||
|#
(define-who ( program)

  #|
  ||
  |#
  (define (Triv triv)
    (match triv
      [,tr (guard (triv? tr)) tr]
      [,else (invalid who 'Triv else)]
      ))

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
  (define (Tail tail)
    (match tail
      [(alloc ,[Value -> vl]) `(alloc ,vl)]
      [(begin ,[Effect -> ef*] ... ,[tl^]) (make-begin `(,ef* ... ,tl^))]
      [(if ,[Pred -> pr] ,[c] ,[a]) `(if ,pr ,c ,a)]
      [(let ([,uv* ,[Value -> vl*]] ...) ,[tl]) `(let ([,uv* ,vl*] ...) ,tl)]
      [(,binop ,[Value -> vl] ,[Value -> vl^]) (guard (binop? binop)) `(,binop ,vl ,vl^)]
      [,tr (guard (triv? tr)) tr]
      [(,[Value -> rator] ,[Value -> rand*] ...) `(,rator ,rand* ...)]
      [,else (invalid who 'Tail else)]
      ))

  #|
  ||
  |#
  (define (Program p)
    (match p
      [(letrec ([,label (lambda (,uvar* ...) ,[Tail -> tl*])] ...) ,[Tail -> tl])
       `(letrec ([,label (lambda (,uvar* ...) ,tl*)] ...) ,tl)]
      [,else (invalid who 'Program else)]
      ))

  (Program program)

)) ;; end library
