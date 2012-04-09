;; discard-call-live.ss
;;
;; part of p423-sp12/srwaggon-p423 a4
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in a4
;; 
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; 2012 / 2 / 19

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
      ;;[]
      ;;[]
      [,else (invalid who 'Effect else)]
      ))
  
  (define (Tail t)
    (match t
      [(if ,pred ,[tail^] ,[tail&]) `(if ,pred ,tail^ ,tail&)]
      [(begin ,[Effect -> effect*] ... ,[tail^]) `(begin ,effect* ... ,tail^)]
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
      [(,label (lambda () ,[Body -> body])) `(,label (lambda () ,body))]
      [,else (invalid who 'Block else)]))

  (define (Program p)
    (match p
      [(letrec (,[Block -> block*] ...) ,[Body -> body]) `(letrec ,block* ,body)]
      [,else (invalid who 'Program else)]))

  (Program program)

))