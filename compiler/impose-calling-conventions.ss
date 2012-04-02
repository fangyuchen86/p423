;; impose-calling-conventions.ss
;;
;; part of p423-sp12/srwaggon-p423
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in A6
;; 2012 / 3 / 26
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; revised in A7
;; 2012 / 3 / 31

#!chezscheme
(library (compiler impose-calling-conventions)
  (export 
   impose-calling-conventions
   )
  (import
   ;; Load Chez Scheme primitives:
   (chezscheme)
   ;; Load compiler framework:
   (framework match)
   (framework helpers)
   (compiler helpers)
  )

(define-who (impose-calling-conventions program)
  (define (Body b param*)

    (define frame* '())
    (define frame-vars '())

    #|
    || find-homes : param* -> `(loc* ...)
    || find-homes takes a list of parameters and returns a list
    || of determined locations for the parameters where register
    || locations are given priority (specifically parameter-registers)
    || before parameters are given stack-frame memory locations.
    |#
    (define (find-homes param* home-gen)
      (set! fv-index 0)
      (let loop ([param* param*][reg* parameter-registers])
        (cond
          [(null? param*) '()]
          [(null? reg*) (cons (home-gen) (loop (cdr param*) reg*))]
          [else (cons (car reg*) (loop (cdr param*) (cdr reg*)))]
          )))

    (define fv-index 0)

    (define (new-fv)
      (let ([fv (index->frame-var fv-index)])
        (set! fv-index (add1 fv-index))
        fv))

    (define (new-uvar)
        (let ([nfv (unique-name 'nfv)])
          (begin (set! frame-vars (cons nfv frame-vars))
                 nfv)))

    (define (Triv t)
      (match t
        [,t^ (guard (triv? t^)) t^]
        [,else (invalid who 'Triv else)]
        ))

    (define (Effect e)
      (match e
        [(nop) '(nop)]
        [(begin ,[e*] ... ,[e^]) (make-begin `(,e* ... ,e^))]
        [(if ,[Pred -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
        [(set! ,uvar (,binop ,[Triv -> t] ,[Triv -> t^])) (guard (binop? binop))
         `(set! ,uvar (,binop ,t ,t^))]
        [(set! ,uvar (,rator ,rand* ...))
         (make-begin `(,(Effect `(,rator ,rand* ...))
           (set! ,uvar ,return-value-register)))]
        [(set! ,uvar ,[Triv -> t]) `(set! ,uvar ,t)]
        [(,rator ,rand* ...)
         (let ([rp (unique-name 'rp)][loc* (find-homes rand* new-uvar)])
           (let ([rand* (reverse rand*)][loc-rand* (reverse loc*)])
             (begin 
               (set! frame* (cons (reverse frame-vars) frame*))
               (set! frame-vars '())
               `(return-point ,rp
               ,(make-begin
                 `((set! ,loc-rand* ,rand*) ...
                   (set! ,return-address-register ,rp)
                   (,rator ,frame-pointer-register ,loc* ...)))))))]
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
    
    (define (Tail t rp)
      (match t
        [(begin ,[Effect -> e*] ... ,[t^]) (make-begin `(,e* ... ,t^))]
        [(if ,[Pred -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
        [(,binop ,[Triv -> t^] ,[Triv -> t&]) (guard (binop? binop))
         (make-begin `((set! ,return-value-register (,binop ,t^ ,t&))
                       (,rp ,frame-pointer-register ,return-value-register)))]
        [(,[Triv -> rator] ,[Triv -> rand*] ...)
         (let ([loc* (find-homes rand* new-fv)])
           (let ([rand* (reverse rand*)] [loc-rand* (reverse loc*)])
             (make-begin
              `((set! ,loc-rand* ,rand*) ...
                (set! ,return-address-register ,rp)
                (,rator ,return-address-register ,frame-pointer-register
                        ,loc* ...)))))]
        [,t^ (guard (triv? t^))
             (make-begin `((set! ,return-value-register ,t^)
                           (,rp ,frame-pointer-register ,return-value-register)))]
        [,else (invalid who 'Tail else)]
        ))
    
    ;; (define (Body b)
    (match b
      [(locals (,uvar* ...) ,t)
       (let* ([rp (unique-name 'rp)] [tail (Tail t rp)]
              [loc* (find-homes param* new-fv)])
         `(locals (,rp ,param* ... ,uvar* ... ,frame* ... ...)
            (new-frames ,frame*
              ,(make-begin
                `((set! ,rp ,return-address-register)
                  (set! ,param* ,loc*) ...
                  ,tail)))))]
      [,else (invalid who 'Body else)]
      ))
  
  (define (Program p)
    (match p
      [(letrec ([,label (lambda (,uvar* ...) ,b*)] ...) ,b)
       `(letrec ([,label (lambda () ,(map Body b* uvar*))] ...) ,(Body b '()))]
      [,else (invalid who 'Program else)]
      ))

  (Program program)
  
)) ;; end library
