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

  (define (Body b)
    
    (define new-ulocal* '())

    (define (new-u)
      (let ([u (unique-name 't)])
        (set! new-ulocal* (cons u new-ulocal*))
        u))


    (define (select-binop x op y z)
      (define (commutative? op)
        (define commutative-ops '(+ * logor logand))
        (and #t (memq op commutative-ops)))
      (cond
        [(eq? x y) (select-binop-2 op y z)]
        [(commutative? op) (select-binop2 op z y)]
        [else (let ([u (new-u)]
                    (make-begin `((set! ,u ,y)
                                  ,(select-binop-2 ,op ,u ,z)
                                  (set! ,x ,u)))))]))

    (define (select-binop-2 op y z) ---)

    (define (select-move lhs rhs)
      (cond
        [(and (frame-var? lhs)
              (or (frame-var? rhs) (int64? rhs) (label? rhs)))
         (let ([u (new-u)])
           (make-begin `((set! ,u ,rhs) `(set! ,lhs ,u))))]
        [else `(set! ,lhs ,rhs)]))

    (define (select-relop op x y) ---)

    (define (select-relop-2 op x y) ---)

    (define (loc? l)
      (or (register? l) (frame-var? l)))
    (define (Loc l)
      (match l
        [l (guard (loc? l)) l]
        [,else (invalid who 'Loc else)]
        ))
    
    (define (var? v)
      (or (uvar? v) (loc? v)))
    (define (Var v)
      (match v
        [,v (guard (var? v)) v]
        [,else (invalid who 'Var else)]
        ))
    
    (define (triv? t)
      (or (label? t) (int64? t) (var? t)))
    (define (Triv t)
      (match t
        [,t (guard (triv? t)) t]
        [,else (invalid who 'Triv else)]
        ))
    
    (define (Effect e)
      (match e
        [(nop) e]
        [(if ,[Pred -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
        [(begin ,[e] ,[e*] ...)]
        [(set! ,v (,binop ,t0 ,t1))
         (select-binop v binop t0 t1)]
        [(set! ,v ,t)
         (select-move v t)
        [,else (invalid who 'Effect else)]
        ))
    
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