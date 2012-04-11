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

  (define (Triv offset)
    (lambda (t)
      (match t
        [,t^ (guard (frame-var? t^))
             (values (make-disp-opnd frame-pointer-register
                                     (- (ash (frame-var->index t^) word-shift) offset)) offset)]
        [,t^ (guard (triv? t^)) (values t^ offset)]
        [,else (invalid who 'Triv else)]
        )))

  (define (Effect offset)
    (lambda (e)
      (match e
        [(nop) (values '(nop) offset)]
        [(begin ,[(Effect offset) -> e* e*offset] ... ,[e^ e^offset]) (values (make-begin `(,e* ... ,e^)) offset)]
        [(if ,t ,c ,a)
         (let*-values ([(p^ poffset) ((Pred    offset) t)]
                       [(c^ coffset) ((Effect poffset) c)]
                       [(a^ aoffset) ((Effect poffset) a)])
           (if (= coffset aoffset) (values `(if ,p^ ,c^ ,a^) coffset)
               (errorf who "conseq and altern return with differing offsets" )))]
        [(set! ,fp (- ,fp ,nb)) (guard (eq? fp frame-pointer-register))
         (values '(nop) (- offset nb))]
        [(set! ,fp (+ ,fp ,nb)) (guard (eq? fp frame-pointer-register))
         (values '(nop) (+ offset nb))]
        [(set! ,[(Triv offset) -> uvar uvaroffset] (,binop ,[(Triv offset) -> t toffset] ,[(Triv offset) -> t^]))
         (guard (binop? binop)) (values `(set! ,uvar (,binop ,t ,t^)) offset)]
        [(set! ,[(Triv offset) -> uvar uvaroffset] ,[(Triv offset) -> t toffset]) (values `(set! ,uvar ,t) offset)]
        [,else (invalid who 'Effect else)]
        )))
    
  (define (Pred offset)
    (lambda (p)
      (match p
        [(true)  (values '(true)  offset)]
        [(false) (values '(false) offset)]
        [(begin ,[(Effect offset) -> e* e*offset] ... ,[p^ p^offset]) (values (make-begin `(,e* ... ,p^)) offset)]
        [(if ,t ,c ,a)
         (let*-values ([(p^ poffset) ((Pred  offset) t)]
                       [(c^ coffset) ((Pred poffset) c)]
                       [(a^ aoffset) ((Pred poffset) a)])
           (if (= coffset aoffset) (values `(if ,p^ ,c^ ,a^) coffset)
               (errorf who "conseq and altern return with differing offsets" )))]
        [(,relop ,[(Triv offset) -> t toffset] ,[(Triv offset) -> t^ t^offset])
         (guard (relop? relop)) (values `(,relop ,t ,t^) toffset)]
        [,else (invalid who 'Pred else)]
      )))
  
  (define (Tail offset)
    (lambda (t)
      (match t
        [(,[(Triv offset) -> t^ offset]) (values `(,t^) offset)]
        [(begin ,[(Effect offset) -> e* offset] ... ,[t^]) (values (make-begin `(,e* ... ,t^)) offset)]
        [(if ,t ,c ,a)
         (let*-values ([(p^ poffset) ((Pred  offset) t)]
                       [(c^ coffset) ((Tail poffset) c)]
                       [(a^ aoffset) ((Tail poffset) a)])
           (if (= coffset aoffset) (values `(if ,p^ ,c^ ,a^) coffset)
               (errorf who "conseq and altern return with differing offsets" )))]
        [,t^ (guard (triv? t^)) (let-values ([(t^ t^offset) ((Triv offset) t^)])
                                  t^)]
        [,else (invalid who 'Tail else)]
        )))
  
  (define (Program p)
    (match p
      [(letrec ([,label (lambda (,uvar* ...) ,[(Tail 0) -> t* offset])] ...) ,[(Tail 0) -> t offset])
       `(letrec ([,label (lambda (,uvar* ...) ,t*)] ...) ,t)]
      [,else (invalid who 'Program else)]
      ))
  
  (Program program)
  
)) ;; end library
