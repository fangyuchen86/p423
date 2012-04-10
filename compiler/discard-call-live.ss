;; discard-call-live.ss
;;
;; part of p423-sp12/srwaggon-p423
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in A4
;; 
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; revised in A7
;; 2012 / 4 / 10

#!chezscheme
(library (compiler discard-call-live)
  (export discard-call-live)  
  (import
   (chezscheme)
   (framework match)
   (framework helpers)
   (compiler helpers))
  

(define-who (discard-call-live program)


  (define (Effect e)
    (match e
      [(begin ,[e*] ... ,[e^]) `(begin ,e* ... ,e^)]
      [(if ,[Pred -> t] ,[c] ,[a]) `(if ,t ,c ,a)]
      [(nop) `(nop)]
      [(return-point ,label ,[Tail -> t]) `(return-point ,label ,t)]
      [(set! ,x (,binop ,x ,y)) (guard (binop? binop))
       `(set! ,x (,binop ,x ,y))]
      [(set! ,x ,y) `(set! ,x ,y)]
      [,else (invalid who 'Effect else)]
      ))

  (define (Pred p)
    (match p
      [(begin ,[Effect -> e*] ... ,[p^]) `(begin ,e* ... ,p^)]
      [(false) `(false)]
      [(if ,[t] ,[c] ,[a]) `(if ,t ,c ,a)]
      [(,relop ,t^ ,t&) (guard (relop? relop) (triv? t^) (triv? t&))
       `(,relop ,t^ ,t&)]
      [(true) `(true)]
      ))
  
  (define (Tail t)
    (match t
      [(if ,[Pred -> pred] ,[tail^] ,[tail&])
       `(if ,pred ,tail^ ,tail&)]
      [(begin ,[Effect -> effect*] ... ,[tail^])
       `(begin ,effect* ... ,tail^)]
      [(,triv ,loc* ...) `(,triv)]
      [,else (invalid who 'Tail else)]
      ))

  (define (Body b)
    (match b
      [(locate ,uvar-env ,[Tail -> tail]) `(locate ,uvar-env ,tail)]
      [,else (invalid who 'Body else)]
      ))

  (define (Block b)
    (match b
      [(,label (lambda () ,[Body -> body]))
       `(,label (lambda () ,body))]
      [,else (invalid who 'Block else)]))

  (define (Program p)
    (match p
      [(letrec (,[Block -> block*] ...) ,[Body -> body])
       `(letrec ,block* ,body)]
      [,else (invalid who 'Program else)]))

  (Program program)

))