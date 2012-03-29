;; FILENAME.ss
;;
;; part of p423-sp12/srwaggon-p423 A6
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in A2?
;; 2012 / 3 / 28
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; 2012 / 3 / 28

#!chezscheme
(library (compiler expose-frame-var)
  (export 
   expose-frame-var
   )
  (import
   ;; Load Chez Scheme primitives:
   (chezscheme)
   ;; Load compiler framework:
   (framework match)
   (framework helpers)
   (compiler helpers)
  )


#| expose-frame-var : exp --> exp
 | expose-frame-var takes an expression and tree-walks it
 | converting any occurrences of frame variables into 
 | displacement mode operands,
 | with rbp as the base register and an offset based on 
 | the frame variable's index.  Since our words are 8-bits
 | in length, the following pattern emerges:
 |
 | fv0 --> #<disp rbp 0>
 | fv1 --> #<disp rbp 8>
 | fv[i] --> #<disp rbp 8i>
 |#
(define-who (expose-frame-var program)

  (define (Triv t)
    (match t
      [,t^ (guard (triv? t^))
           (if (frame-var? t^)
               (make-disp-opnd frame-pointer-register
                               (ash (frame-var->index t^) word-shift))
               t^)]
      [,else (invalid who 'Triv else)]
      ))

  (define (Effect e)
    (match e
      [(nop) '(nop)]
      [(begin ,[Effect -> e*] ... ,[e^]) (make-begin `(,e* ... ,e^))]
      [(if ,[Pred -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
      [(set! ,uvar (,binop ,[Triv -> t] ,[Triv -> t^])) (guard (binop? binop))
       `(set! ,uvar (,binop ,t ,t^))]
      [(set! ,uvar ,[Triv -> t]) `(set! ,uvar ,t)]
      [,else (invalid who 'Effect else)]
      ))

  (define (Pred p)
    (match p
      [(true) '(true)]
      [(false) '(false)]
      [(begin ,[Effect -> e*] ... ,[p^]) (make-begin `(,e* ... ,p^))]
      [(if ,[Pred -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
      [(,relop ,[Triv -> t] ,[Triv -> t^]) (guard (relop? relop)) `(,relop ,t ,t^)]
      [,else (invalid who 'Pred else)]
      ))
  
  (define (Tail t)
    (match t
      [(,[Triv -> t^]) `(,t^)]
      [(begin ,[Effect -> e*] ... ,[t^]) (make-begin `(,e* ... ,t^))]
      [(if ,[Pred -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
      [,t^ (guard (triv? t^)) t^]
      [,else (invalid who 'Tail else)]
      ))
    
  (define (Program p)
    (match p
      [(letrec ([,label (lambda (,uvar* ...) ,[Tail -> t*])] ...) ,[Tail -> t])
       `(letrec ([,label (lambda (,uvar* ...) ,t*)] ...) ,t)]
      [,else (invalid who 'Program else)]
      ))
  
  (Program program)
  
)) ;; end library
