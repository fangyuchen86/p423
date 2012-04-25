;; expose-frame-var.ss
;;
;; part of p423-sp12/srwaggon-p423
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in A2
;; 2012 / 3 / 28
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; revised in A7
;; 2012 / 4 / 11

#!chezscheme
(library (compiler expose-frame-var)
  (export expose-frame-var)
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

(define fp-offset 0)

  (define (Triv t)
    (match t
      [,t^ (guard (frame-var? t^))
           (make-disp-opnd frame-pointer-register
                           (- (ash (frame-var->index t^) word-shift) fp-offset))]
      [,t^ (guard (triv? t^)) t^]
      [,else (invalid who 'Triv else)]
      ))

  (define (Effect e)
    (match e
      [(nop) '(nop)]
      [(begin ,[ef*] ... ,[ef]) (make-begin `(,ef* ... ,ef))]
      [(if ,[Pred -> pr] ,[c] ,[a]) `(if ,pr ,c ,a)]
      [(mset! ,base ,offset ,val) `(mset! ,base ,offset ,val)]
      [(return-point ,label ,[Tail -> tl]) `(return-point ,label ,tl)]
      [(set! ,fp (- ,fp ,nb)) (guard (eq? fp frame-pointer-register))
       (set! fp-offset (+ fp-offset nb)) 
       `(set! ,fp (- ,fp ,nb))]
      [(set! ,fp (+ ,fp ,nb)) (guard (eq? fp frame-pointer-register))
       (set! fp-offset (- fp-offset nb))
       `(set! ,fp (+ ,fp ,nb))]
      [(set! ,var (mref ,base ,offset)) `(set! ,var (mref ,base ,offset))]
      [(set! ,[Triv -> uv] (,binop ,[Triv -> tr^] ,[Triv -> tr&]))
       (guard (binop? binop)) `(set! ,uv (,binop ,tr^ ,tr&))]
      [(set! ,[Triv -> uv] ,[Triv -> tr]) `(set! ,uv ,tr)]
      [,else (invalid who 'Effect else)]
      ))
  
  (define (Pred p)
    (match p
      [(true)  '(true)]
      [(false) '(false)]
      [(begin ,[Effect -> ef*] ... ,[pr])
       (make-begin `(,ef* ... ,pr))]
      [(if ,[Pred -> pr] ,[c] ,[a]) `(if ,pr ,c ,a)]
      [(,relop ,[Triv -> tr] ,[Triv -> tr^]) (guard (relop? relop)) `(,relop ,tr ,tr^)]
      [,else (invalid who 'Pred else)]
      ))
  
  (define (Tail t)
    (match t
      [(,[Triv -> tr]) `(,tr)]
      [(begin ,[Effect -> ef*] ... ,[tl]) (make-begin `(,ef* ... ,tl))]
      [(if ,[Pred -> pr] ,[c] ,[a]) `(if ,pr ,c ,a)]
      [,tr (guard (triv? tr)) (Triv tr)]
      [,else (invalid who 'Tail else)]
      ))

  (define (Program p)
    (match p
      [(letrec ([,label (lambda (,uvar* ...) ,[Tail -> tl*])] ...) ,[Tail -> tl])
       `(letrec ([,label (lambda (,uvar* ...) ,tl*)] ...) ,tl)]
      [,else (invalid who 'Program else)]
      ))
  
  (Program program)
  
)) ;; end library
