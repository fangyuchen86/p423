;; expose-allocation-pointer.ss
;;
;; part of p423-sp12/srwaggon-p423
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in A8
;; 2012 / 4 / 14
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; revised in A8
;; 2012 / 4 / 14

#!chezscheme
(library (compiler expose-allocation-pointer)
  (export expose-allocation-pointer)
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
  (define-who (expose-allocation-pointer program)

    #|
    ||
    |#
    (define (Triv t)
      (match t
        [,t^ (guard (triv? t^)) t^]
        [,else (invalid who 'Triv else)]
        ))

    #|
    ||
    |#
    (define (Effect e)
      (match e
        [(nop) '(nop)]
        [(begin ,[Effect -> e*] ... ,[e^]) (make-begin `(,e* ... ,e^))]
        [(if ,[Pred -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
        [(mset! ,[Triv -> tr] ,[Triv -> tr^] ,[Triv -> tr&]) `(mset! ,tr ,tr^ ,tr&)]
        [(return-point ,label ,[Tail -> tl]) `(return-point ,label ,tl)]
        [(set! ,uvar (alloc ,[Triv -> tr]))
         (let ([apr allocation-pointer-register])
           (make-begin `((set! ,uvar ,apr)
                         (set! ,apr (+ ,apr ,tr)))))]
        [(set! ,uvar (,binop ,[Triv -> t] ,[Triv -> t^])) (guard (binop? binop))
         `(set! ,uvar (,binop ,t ,t^))]
        [(set! ,uvar ,[Triv -> t]) `(set! ,uvar ,t)]
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
        [(,relop ,[Triv -> t] ,[Triv -> t^]) (guard (relop? relop)) `(,relop ,t ,t^)]
        [,else (invalid who 'Pred else)]
        ))

    #|
    ||
    |#
    (define (Tail t)
      (match t
        [(alloc ,[Triv -> tr]) `(alloc ,tr)]
        [(begin ,[Effect -> e*] ... ,[t^]) (make-begin `(,e* ... ,t^))]
        [(if ,[Pred -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
        [(mref ,[Triv -> tr] ,[Triv -> tr^]) `(mref ,tr ,tr^)]
        [(,binop ,[Triv -> t^] ,[Triv -> t&]) (guard (binop? binop)) `(,binop ,t^ ,t&)]
        [(,[Triv -> t^] ,[Triv -> t*] ...) `(,t^ ,t* ...)]
        [,t^ (guard (triv? t^)) t^]
        [,else (invalid who 'Tail else)]
        ))

    #|
    ||
    |#
    (define (Body b)
      (match b
        [(locals (,uvar* ...)
           (new-frames (,frame* ...)
             ,[Tail -> t]))
         `(locals (,uvar* ...) (new-frames (,frame* ...) ,t))]
        [,else (invalid who 'Body else)]
        ))

    #|
    ||
    |#
    (define (Program p)
      (match p
        [(letrec ([,label (lambda (,uvar* ...) ,[Body -> b*])] ...) ,[Body -> b])
         `(letrec ([,label (lambda (,uvar* ...) ,b*)] ...) ,b)]
        [,else (invalid who 'Program else)]
        ))

    (Program program)

    )) ;; end library
