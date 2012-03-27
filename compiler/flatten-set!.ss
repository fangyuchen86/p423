;; flatten-set!.ss
;;
;; part of p423-sp12/srwaggon-p423 A6
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in A6
;; 2012 / 3 / 26
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; 2012 / 3 / 26

#!chezscheme
(library (compiler flatten-set!)
  (export 
   flatten-set!
   )
  (import
   ;; Load Chez Scheme primitives:
   (chezscheme)
   ;; Load compiler framework:
   (framework match)
   (framework helpers)
   (compiler helpers)
  )

  (define-who (flatten-set! program)
    
    (define (flatten uvar value)
      (match value
        [(begin ,[Effect -> e*] ... ,[v]) (make-begin `(,e* ... ,v))]
        [(if ,[Pred -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
        [(,binop ,t ,t^) (guard (binop? binop)) `(,binop ,t ,t^)]
        [,t (guard (triv? t)) `(set! ,uvar ,t)]
        [,else (invalid who 'Value else)]
        ))

    (define (Triv t)
      (match t
        [,t^ (guard (triv? t^)) t^]
        [,else (invalid who 'Triv else)]
        ))

    (define (Value v)
      (match v
        [(begin ,[Effect -> e*] ... ,[v^]) (make-begin `(,e* ... ,v^))]
        [(if ,[Pred -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
        [(,binop ,t ,t^) (guard (binop? binop)) `(,binop ,t ,t^)]
        [,t (guard (triv? t)) t]
        [,else (invalid who 'Value else)]
        ))
        
    (define (Effect e)
      (match e
        [(begin ,[Effect -> e*] ... ,[e^]) (make-begin `(,e* ... ,e^))]
        [(if ,[Pred -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
        [(nop) '(nop)]
        [(set! ,uvar ,v) (flatten uvar v)]
        [,else (invalid who 'Effect else)]
        ))
    
    (define (Pred p)
      (match p
        [(true) '(true)]
        [(false) '(false)]
        [(begin ,[Effect -> e*] ... ,[p^]) (make-begin `(,e* ... ,p^))]
        [(if ,[p^] ,[c] ,[a]) `(if ,p^ ,c ,a)]
        [(,relop ,t ,t^) (guard (relop? relop)) `(,relop ,t ,t^)]
        ))
     
    (define (Tail t)
      (match t
        [(begin ,[Effect -> e*] ... ,[t]) (make-begin (,e* .. ,t))]
        [(if ,[Pred -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
        [(,binop ,t ,t^) (guard (binop? binop)) `(,binop ,t ,t^)]
        [(,[Triv -> t] ,[Triv -> ,t*] ...) `(,t ,t* ...)]
        [,t (guard (triv? t)) t]
        [,else (invalid who 'Tail else)]
        ))

    (define (Body b)
      (match b
        [(locals (,uvar* ...) ,[Tail -> t]) `(locals (,uvar* ...) ,t)]
        [,else (invalid who 'Body else)]
        ))
    
    (define (Program p)
      (match p
        [(letrec ([,label* (lambda (,uvar* ...) ,[Body -> b*])] ...) ,[Body -> b])
         `(letrec ([,label* (lambda (,uvar* ...)) ] ...) ,b)]
        [,else (invalid who 'Program else)]
        ))
      
    (Program program)
  )

) ;; end library