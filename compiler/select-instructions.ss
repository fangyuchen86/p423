;; select-instructions.ss
;;
;; part of p423-sp12/srwaggon-p423 A5
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in A5
;; 2012 / 3 / 5
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; 2012 / 3 / 5
;; borrowed from solution set.

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

  (define-who select-instructions
    (define (ur? x) (or (register? x) (uvar? x)))
    (define (Body x)
      (define new-ulocal* '())
      (define (new-u)
        (let ([u (unique-name 'u)])
          (set! new-ulocal* (cons u new-ulocal*))
          u))
      (define (select-binop-1 x op y z)
        (cond
          [(eq? y x) (select-binop-2 x op z)]
          [(and (eq? z x) (memq op '(* + logor logand))) (select-binop-2 x op y)]
          [else (let ([u (new-u)])
                  (make-begin `((set! ,u ,y)
                                ,(select-binop-2 u op z)
                                (set! ,x ,u))))]))
      (define (select-binop-2 x op y)
        (case op
          [(- + logand logor)
           (if (or (and (ur? x) (or (ur? y) (frame-var? y) (int32? y)))
                   (and (frame-var? x) (or (ur? y) (int32? y))))
               `(set! ,x (,op ,x ,y))
               (let ([u (new-u)])
                 (make-begin `((set! ,u ,y) (set! ,x (,op ,x ,u))))))]
          [(*)
           (if (ur? x)
               (if (or (ur? y) (frame-var? y) (int32? y))
                   `(set! ,x (,op ,x ,y))
                   (let ([u (new-u)])
                     (make-begin `((set! ,u ,y) (set! ,x (,op ,x ,u))))))
               (let ([u (new-u)])
                 (make-begin
                   `((set! ,u ,x)
                     ,(select-binop-2 u op y)
                     (set! ,x ,u)))))]
          [(sra) `(set! ,x (,op ,x ,y))]
          [else (error who "unrecognized binop ~s" op)]))
      (define (select-move lhs rhs)
        (if (or (ur? lhs) (and (frame-var? lhs) (or (ur? rhs) (int32? rhs))))
            `(set! ,lhs ,rhs)
            (let ([u (new-u)])
              (make-begin `((set! ,u ,rhs) (set! ,lhs ,u))))))

      ;; original formulation
      ;; (define (select-relop op x y)
      ;; (define (swap-relop op)
      ;;   (cdr (assq op '((= . =) (< . >) (<= . >=) (> . <) (>= . <=)))))
      ;; (cond
      ;;   [(or (and (ur? x) (or (ur? y) (frame-var? y) (int32? y)))
      ;;        (and (frame-var? x) (or (ur? y) (int32? y))))
      ;;    `(,op ,x ,y)]
      ;;   [(and (int32? x) (or (ur? y) (frame-var? y)))
      ;;    `(,(swap-relop op) ,y ,x)]
      ;;   [(or (and (frame-var? x) (frame-var? y))
      ;;        (and (int32? x) (int32? y))
      ;;        (and (or (and (int64? x) (not (int32? x))) (label? x))
      ;;             (or (ur? y) (frame-var? y) (int32? y))))
      ;;    (let ([u (new-u)])
      ;;      (make-begin `((set! ,u ,x) (,op ,u ,y))))]
      ;;   [(and (or (ur? x) (frame-var? x) (int32? x))
      ;;         (or (and (int64? y) (not (int32? y))) (label? y)))
      ;;    (let ([u (new-u)])
      ;;      (make-begin `((set! ,u ,y) (,(swap-relop op) ,u ,x))))]
      ;;   [else
      ;;     (let ([u1 (new-u)] [u2 (new-u)])
      ;;       (make-begin `((set! ,u1 ,x) (set! ,u2 ,y) (,op ,u1 ,u2))))]))

      ;; alternate formulation
      (define (select-relop-1 op x y)
        (define (swap-relop op)
          (cdr (assq op '((= . =) (< . >) (<= . >=) (> . <) (>= . <=)))))
        (cond
          [(or (ur? x) (frame-var? x)) (select-relop-2 op x y)]
          [(or (ur? y) (frame-var? y)) (select-relop-2 (swap-relop op) y x)]
          [else (let ([u (new-u)])
                  (make-begin `((set! ,u ,x)
                                ,(select-relop-2 op u y))))]))
      (define (select-relop-2 op x y)
        (if (if (ur? x)
                (or (ur? y) (frame-var? y) (int32? y))
                (or (ur? y) (int32? y)))
            `(,op ,x ,y)
            (let ([u (new-u)])
              (make-begin `((set! ,u ,y) (,op ,x ,u))))))
      (define (Effect x)
        (match x
          [(nop) '(nop)]
          [(begin ,[ef*] ... ,[ef]) (make-begin `(,ef* ... ,ef))]
          [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
          [(set! ,lhs (,binop ,x ,y)) (select-binop-1 lhs binop x y)]
          [(set! ,lhs ,rhs) (select-move lhs rhs)]
          [,x (error who "invalid Effect ~s" x)]))
      (define (Pred x)
        (match x
          [(true) '(true)]
          [(false) '(false)]
          [(begin ,[Effect -> ef*] ... ,[test]) (make-begin `(,ef* ... ,test))]
          [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
          [(,relop ,x ,y) (select-relop-1 relop x y)]
          [,x (error who "invalid Pred ~s" x)]))
      (define (Tail x)
        (match x
          [(begin ,[Effect -> ef*] ... ,[tail]) (make-begin `(,ef* ... ,tail))]
          [(if ,[Pred -> test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
          [(,triv ,live* ...) `(,triv ,live* ...)]
          [,x (error who "invalid Tail ~s" x)]))
      (match x
        [(locals (,local* ...)
           (ulocals (,ulocal* ...)
             (locate (,home* ...) (frame-conflict ,ct ,[Tail -> tail]))))
         `(locals (,local* ...)
            (ulocals (,ulocal* ... ,new-ulocal* ...)
              (locate (,home* ...)
                (frame-conflict ,ct ,tail))))]
        [(locate (,home* ...) ,tail) `(locate (,home* ...) ,tail)]
        [,x (error who "invalid Body ~s" x)]))
    (lambda (x)
      (match x
        [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
         `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
        [,x (error who "invalid Program ~s" x)])))


) ;; close library