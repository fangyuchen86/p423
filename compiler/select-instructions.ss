;; select-instructions.ss
;;
;; part of p423-sp12/srwaggon-p423 A5
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in A5
;; 2012 / 3 / 19
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; 2012 / 3 / 22

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

  (define (Body body)
    
    (define new-ulocal* '())

    (define (new-u)
      (let ([u (unique-name 'u)])
        (set! new-ulocal* (cons u new-ulocal*))
        u))

    (define (select-binop x op y z)
      (define (commutative? op)
        (define commutative-ops '(+ * logor logand))
        (and #t (memq op commutative-ops)))
      (cond
        [(eq? x y) (select-binop-2 op y z)]
        [(and (eq? x z) (commutative? op)) (select-binop-2 op z y)]
        [else (let ([u (new-u)])
                    (make-begin `((set! ,u ,y)
                                  ,(select-binop-2 op u z)
                                  (set! ,x ,u))))]))

    (define (select-binop-2 op y z)
      (case op
        [(- + logor logand) (cond
                              [(or (and (frame-var? y) (frame-var? z))
                                   (or (int64!32? z) (label? z)))
                               (let ([u (new-u)])
                                 (make-begin `((set! ,u ,z) (set! ,y (,op ,y ,u)))))]
                              [else `(set! ,y (,op ,y ,z))])]
        [(*) (cond
               [(frame-var? y) (let ([u (new-u)])
                                 (make-begin `((set! ,u ,y)
                                               ,(select-binop-2 op u z)
                                               (set! ,y ,u))))]
               [(or (label? z) (int64!32? z)) (let ([u (new-u)])
                                             (make-begin `((set! ,u ,z) (set! ,y (,op ,y ,u)))))]
               [else `(set! ,y (,op ,y ,z))])]
        [(sra) `(set! ,y (,op ,y ,z))]
        [else (invalid who 'binop op)]
        ))

    (define (select-move lhs rhs)
      (cond
        [(and (frame-var? lhs)
              (or (frame-var? rhs) (int64!32? rhs) (label? rhs)))
         (let ([u (new-u)])
           (make-begin `((set! ,u ,rhs) (set! ,lhs ,u))))]
        [else `(set! ,lhs ,rhs)]
        ))

    (define (select-relop op x y)
      (define (swap-relop op)
        (cdr (assq op `((< . >) (> . <) (<= . >=) (>= . <=) (= . =)))))
      (cond
        [(or (uvar? x) (register? x) (frame-var? x)) (select-relop-2 op x y)]
        [(or (uvar? y) (register? y) (frame-var? y)) (select-relop-2 (swap-relop op) y x)]
        [else (let ([u (new-u)]) (make-begin `((set! ,u ,x) ,(select-relop-2 op u y))))]
        ))

    (define (select-relop-2 op x y)
      (cond
        [(or (or (int64!32? y) (label? y))
             (and (frame-var? x) (frame-var? y)))
         (let ([u (new-u)])
           (make-begin `((set! ,u ,y) (,op ,x ,u))))]
        [else `(,op ,x ,y)]
        ))
    
    (define (Effect effect)
      (match effect
        [(nop) '(nop)]
        [(begin ,[e*] ... ,[e]) `(begin ,e* ... ,e)]
        [(if ,[Pred -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
        [(set! ,v (,binop ,t ,t^)) (select-binop v binop t t^)]
        [(set! ,v ,t) (select-move v t)]
        [,else (invalid who 'Effect else)]
        ))
    
    (define (Pred pred)
      (match pred
        [(true) '(true)]
        [(false) '(false)]
        [(begin ,[Effect -> e*] ... ,[p]) `(begin ,e* ... ,p)]
        [(if ,[p] ,[c] ,[a]) `(if ,p ,c ,a)]
        [(,relop ,triv0 ,triv1) (guard (relop? relop)) (select-relop relop triv0 triv1)]
        [,else (invalid who 'Pred else)]
        ))
    
    (define (Tail tail)
      (match tail
        [(begin ,[Effect -> e*] ... ,[t]) (make-begin `(,e* ... ,t))]
        [(if ,[Pred -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
        [(,t ,loc* ...) `(,t ,loc* ...)]
        [,else (invalid who 'Tail else)]
        ))
    
    (match body
      [(locals (,loc* ...)
         (ulocals (,uloc* ...)
           (locate (,home* ...)
             (frame-conflict ,fgraph ,[Tail -> tail]))))
       `(locals (,loc* ...)
          (ulocals (,uloc* ... ,new-ulocal* ...)
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