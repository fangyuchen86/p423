;; select-instructions.ss
;;
;; part of p423-sp12/srwaggon-p423 A5
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in A5
;; 2012 / 3 / 19
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; 2012 / 3 / 19

#!chezscheme
(library (compiler select-instructions)
  (export 
   select-instructions
   )
  (import
   ;; Load Chez Scheme primitives:
   (chezscheme)
   ;; Load compiler framework:
   (framework match)
   (framework helpers)
   (compiler helpers)
  )

(define-who (select-instructions program)

  (define (Effect e)

  (define (Pred p)
    (match p
      [(true) p]
      [(false) p]
      [(relop triv triv) `(relop ,triv ,triv)]
      [(if ,[p] ,[c] ,[a]) `(if ,p ,c ,a)]
      [(begin ,[Effect -> e*] ... ,[p]) `(begin ,e* ... ,p)]
      ))

  (define (Tail t)
    (match t
      [(if ,[Pred -> p] ,[Pred -> c] ,[Pred -> a]) ]
      [(begin ,[Effect -> e*] ... ,[t]) `(begin ,e* ... ,t)]
      [(,[Triv -> t] ,loc* ...) `(,t ,loc* ...)]
      [,else (invalid who 'Tail else)]
      ))

  (define (Body b)
    (match b
      [(locals (,loc* ...)
         (ulocals (,uloc* ...)
           (locate (,home* ...)
             (frame-conflict ,fgraph ,[Tail -> tail]))))
       `(locals (,loc* ...)
          (ulocals (,uloc* ...)
            (locate (,home* ...)
              (frame-conflict ,fgraph ,tail))))]
      [(locate (,home* ...) ,[Tail -> tail])
       `(locate (,home* ...) ,tail)]
      [,else (invalid who 'Body else)]
      ))
  
  (define (Program p)
    (match p
      [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
      [,else (invalid who 'Program else)]
      ))
  
  (Program program)

)) ;; end library