;; remove-complex-opera*.ss
;;
;; part of p423-sp12/srwaggon-p423 A6
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in A6
;; 2012 / 3 / 21
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; 2012 / 3 / 23

#!chezscheme
(library (compiler remove-complex-opera*)
  (export 
   remove-complex-opera*
   )
  (import
   ;; Load Chez Scheme primitives:
   (chezscheme)
   ;; Load compiler framework:
   (framework match)
   (framework helpers)
   (compiler helpers)
  )

#|
|| This pass removes nested primitive calls from within procedure
|| calls and other primitive calls, making the argument values
|| "trivial." Programs produced by this pass are in the language
|| described by the grammar below.
||
|| Program  --->  (letrec ([<label> (lambda (<uvar>*) <Body>)]*) <Body>)
|| Body     --->  (locals (<uvar>*) <Tail>)
|| Tail     --->  <Triv>
||           |    (alloc <Value>)
||           |    (<binop> <Triv> <Triv>)
||           |    (<Triv> <Triv>*)
||           |    (if <Pred> <Tail> <Tail>)
||           |    (mref <Value> <Value>)
||           |    (begin <Effect>* <Tail>)
|| Pred     --->  (true)
||           |    (false)
||           |    (<relop> <Triv> <Triv>)
||           |    (if <Pred> <Pred> <Pred>)
||           |    (begin <Effect>* <Pred>)
|| Effect   --->  (nop)
||           |    (set! <uvar> <Value>)
||           |    (if <Pred> <Effect> <Effect>)
||           |    (begin <Effect>+)
|| Value    --->  <Triv>
||           |    (<binop> <Triv> <Triv>)
||           |    (if <Pred> <Value> <Value>)
||           |    (begin <Effect>* <Value>)
|| Triv     --->  <uvar> | <int64> | <label>
||
|#

(define-who (remove-complex-opera* program)
  

  (define (Body b)

    (define new-local* '())
    
    (define (new-t)
      (let ([t (unique-name 't)])
        (set! new-local* (cons t new-local*))
        t))

    (define (simple? xpr)
      (or (and (integer? xpr) (exact? xpr))
          (label? xpr)
          (uvar? xpr)
          (binop? xpr)
          (relop? xpr)
          (eq? 'alloc xpr)
          (eq? 'mref xpr)
          (eq? 'mset! xpr)
          ))

    (define (trivialize xpr)
      (let-values ([(code set!*) (simplify xpr)])
        (make-begin `(,@set!* ,code))))

    (define (simplify xpr)
      (match xpr
        [()  (values '() '())]
        [(,simple . ,[rem set!*])
         (guard (simple? simple))
         (values `(,simple ,rem ...) set!*)]
        [(,[Value -> v] . ,[rem set!*])
         (let ([t (new-t)])
           (values `(,t ,rem ...) `((set! ,t ,v) ,set!* ...)))]
        [,x (invalid who 'Complex-Opera* x)]
        ))
  
    (define (Value v)
      (match v
        [(alloc ,v^) (trivialize `(alloc ,v^))]
        [(if ,[Pred -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
        [(begin ,[Effect -> e*] ... ,[v^]) (make-begin `(,e* ... ,v^))]
        [(mref ,v^ ,v&) (trivialize `(mref ,v^ ,v&))]
        [(,binop ,v^ ,v&) (guard (binop? binop))
         (trivialize `(,binop ,v^ ,v&))]
        [(,v^ ,v* ...) (trivialize `(,v^ ,v* ...))]
        [,t (guard (triv? t)) t]
        [,else (invalid who 'Value else)]
        ))
    
    (define (Effect e)
      (match e
        [(mset! ,v ,v^ ,v&) (trivialize `(mset! ,v ,v^ ,v&))]
        [(nop) '(nop)]
        [(set! ,uvar ,[Value -> v]) `(set! ,uvar ,v)]
        [(if ,[Pred -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
        [(begin ,[e*] ... ,[e]) (make-begin `(,e* ... ,e))]
        [(,v^ ,v* ...) (trivialize `(,v^ ,v* ...))]
        [,else (invalid who 'Effect else)]
        ))
    
    (define (Pred p)
      (match p
        [(true) '(true)]
        [(false) '(false)]
        [(if ,[t] ,[c] ,[a]) `(if ,t ,c ,a)]
        [(begin ,[Effect -> e*] ... ,[p]) (make-begin `(,e* ... ,p))]
        [(,relop ,v ,v^) (guard (relop? relop)) (trivialize `(,relop ,v ,v^))]
        [,else (invalid who 'Pred else)]
        ))
    
    (define (Tail t)
      (match t
        [(alloc ,v) (trivialize `(alloc ,v))]
        [(if ,[Pred -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
        [(begin ,[Effect -> e*] ... ,[t]) (make-begin `(,e* ... ,t))]
        [(mref ,v ,v^) (trivialize `(mref ,v ,v^))]
        [(,binop ,v ,v^) (guard (binop? binop)) (trivialize `(,binop ,v ,v^))]
        [(,v ,v* ...) (trivialize `(,v ,v* ...))]
        [,t (guard (triv? t)) t]
        [,else (invalid who 'Tail else)]
        ))
    
    (match b
      [(locals (,local* ...) ,[Tail -> t])
       `(locals (,local* ... ,new-local* ...) ,t)]
      [,else (invalid who 'Body else)]
      ))

  (define (Program p)
    (match p
      [(letrec ([,label (lambda (,uvar* ...) ,[Body -> b*])] ...) ,[Body -> b])
       `(letrec ([,label (lambda (,uvar* ...) ,b*)] ...) ,b)]
      [,else (invalid who 'Program else)]
      ))

  (begin
    (unique-name-count 10000)
    (Program program)
    )

)) ;; end library