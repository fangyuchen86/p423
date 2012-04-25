;; expose-memory-operands.ss
;;
;; part of p423-sp12/srwaggon-p423
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in A8
;; 2012 / 4 / 24
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; revised in A8
;; 2012 / 4 /24

#!chezscheme
(library (compiler expose-memory-operands)
  (export expose-memory-operands)
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
(define-who (expose-memory-operands program)

    #|
    ||
    |#
    (define (Effect e)
      (match e
        [(nop) '(nop)]
        [(begin ,[Effect -> e*] ... ,[e^]) (make-begin `(,e* ... ,e^))]
        [(if ,[Pred -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
        [(mset! ,base ,offset ,val) `(set! ,(make-index-opnd base offset) ,val)]
        [(return-point ,lbl ,[Tail -> tl]) `(return-point ,lbl ,tl)]
        [(set! ,var (mref ,base ,offset)) `(set! ,var ,(make-index-opnd base offset))]
        [(set! ,uvar (,binop ,tr ,tr^)) (guard (binop? binop))
         `(set! ,uvar (,binop ,tr ,tr^))]
        [(set! ,uvar ,tr) `(set! ,uvar ,tr)]
        [,else (invalid who 'Effect else)]
        ))

    #|
    ||
    |#
    (define (Pred p)
      (match p
        [(true) '(true)]
        [(false) '(false)]
        [(begin ,[Effect -> e*] ... ,[p^]) (make-begin `(,e* ... ,p^))]
        [(if ,[Pred -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
        [(,relop ,tr ,tr^) (guard (relop? relop)) `(,relop ,tr ,tr^)]
        [,else (invalid who 'Pred else)]
        ))

    #|
    ||
    |#
    (define (Tail t)
      (match t
        [(begin ,[Effect -> e*] ... ,[t^]) (make-begin `(,e* ... ,t^))]
        [(if ,[Pred -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
        [(,binop ,tr^ ,tr&) (guard (binop? binop)) `(,binop ,tr^ ,tr&)]
        ;;[(,tr^ ,tr* ...) `(,tr^ ,tr* ...)]
        ;;[,tr^ tr^]
        [(,triv) `(,triv)]
        [,else (invalid who 'Tail else)]
        ))

    #|
    ||
    |#
    (define (Program p)
      (match p
        [(letrec ([,label (lambda (,uvar* ...) ,[Tail -> t*])] ...) ,[Tail -> t])
         `(letrec ([,label (lambda (,uvar* ...) ,t*)] ...) ,t)]
        [,else (invalid who 'Program else)]
        ))

    (Program program)

    )) ;; end library
