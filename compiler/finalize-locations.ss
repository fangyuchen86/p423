 ;; finalize-locations.ss
;;
;; part of p423-sp12/srwaggon-p423
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in A2
;; 2012 /  /
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; revised in A7
;; 2012 / 4 / 10

#!chezscheme
(library (compiler finalize-locations)
  (export finalize-locations)
  (import
   ;; Load Chez Scheme primitives:
   (chezscheme)
   ;; Load compiler framework:
   (framework match)
   (framework helpers)
   (compiler helpers)
   )

  
#| finalize-locations : exp --> exp
|| finalize-locations takes a valid Scheme expression
|| and replaces each occurrence of a uvar in the body
|| of each locate form with the corresponding Loc.
|| As it does so, it removes useless assignments, i.e.,
|| converts any assignments (set! ,x ,y) to (nop)
|| if x and y resolve to the same location.  It
|| also discareds the locate form.
|#
(define-who (finalize-locations program)

  #|
  ||
  |#
  (define (Tail env)
    (lambda (t)

      (define (deref uvar)
        (cadr (assq uvar env)))

      #|
      ||
      |#
      (define (Triv t)
        (match t
          [,t^ (guard (uvar? t^)) (deref t^)]
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
          [(mset! ,base ,offset ,val) `(mset! ,base ,offset ,val)]
          [(return-point ,label ,[(Tail env) -> t]) `(return-point ,label ,t)]
          [(set! ,uvar (,binop ,[Triv -> t] ,[Triv -> t^]))
           (guard (binop? binop))
           `(set! ,(Triv uvar) (,binop ,t ,t^))]
          [(set! ,[Triv -> uvar] ,[Triv -> t])
           (if (equal? uvar t) '(nop) `(set! ,(Triv uvar) ,t))]
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
          [(if ,[p^] ,[c] ,[a]) `(if ,p^ ,c ,a)]
          [(,relop ,[Triv -> t^] ,[Triv -> t&])
           (guard (relop? relop))
           `(,relop ,t^ ,t&)]
          [,else (invalid who 'Pred else)]
          ))

      (match t
        [(begin ,[Effect -> e*] ... ,[t^]) (make-begin `(,e* ... ,t^))]
        [(if ,[Pred -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
        [(,binop ,[Triv -> t^] ,[(Triv env) -> t&])
         (guard (binop? binop))
         `(,binop ,t^ ,t&)]
        [(,[Triv -> t^] ,[Triv -> t*] ...) `(,t^ ,t* ...)]
        [,t^ (guard (triv? t^)) (Triv t^)]
        [,else (invalid who 'Tail else)]
        )))

  #|
  ||
  |#
  (define (Body b)
    (match b
      [(locate (,home* ...) ,tail) ((Tail home*) tail)]
      [,else (invalid who 'Body else)]
      ))
  
  #|
  ||
  |#
  (define (Program p)
    (match p
      [(letrec ([,label (lambda (,uvar* ...) ,[Body -> b*])] ...)
         ,[Body -> b])
       `(letrec ([,label (lambda (,uvar* ...) ,b*)] ...) ,b)]
      [,else (invalid who 'Program else)]
      ))
  
  (Program program)
  
)) ;; end library
