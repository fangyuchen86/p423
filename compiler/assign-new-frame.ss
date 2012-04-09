;; assign-new-frame.ss
;;
;; part of p423-sp12/srwaggon-p423
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in A7
;; 2012 / 4 / 8
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; revised in A7
;; 2012 / 4 / 8

#!chezscheme
(library (compiler assign-new-frame)
  (export assign-new-frame)
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
  (define-who (assign-new-frame program)

    #|
    ||
    |#
    (define (Body b)

      (define frame-size 0)

      #| find-homes
      || : uvar* conflict-graph home* frame-index
      || --> home* :: `((uvar register) ...)
      ||
      || Finds and binds each uvar to a
      || non-conflicting frame-var.
      |#
      (define (find-homes uvar* cgraph home* frame-index)
        (if (null? uvar*) home*
            (let* ([uvar (car uvar*)]
                   [conflict* (assq uvar cgraph)]
                   [used (let find-used ([conflict* (cdr conflict*)])
                           (cond
                             [(null? conflict*) '()]
                             [(frame-var? (car conflict*))
                              (set-cons (car conflict*) (find-used (cdr conflict*)))]
                             [(assq (car conflict*) home*) =>
                              (lambda (x) (set-cons (cadr x) (find-used (cdr conflict*))))]
                             [else (find-used (cdr conflict*))]))]
                   [home (let find-home ([index frame-index])
                           (let ([fv (index->frame-var index)])
                             (if (memq fv used) (find-home (add1 index)) fv)))])
              (find-homes (remove uvar uvar*) cgraph `((,uvar ,home) . ,home*) frame-index)
              )))


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
          [(return-point ,label ,[Tail -> tail])
           (make-begin `((set! ,frame-pointer-register (+ ,frame-pointer-register ,frame-size))
                         (return-point ,label ,tail)
                         (set! ,frame-pointer-register (- ,frame-pointer-register ,frame-size))))]
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
          [(begin ,[Effect -> e*] ... ,[t^]) (make-begin `(,e* ... ,t^))]
          [(if ,[Pred -> p] ,[c] ,[a]) `(if ,p ,c ,a)]
          [(,binop ,[Triv -> t^] ,[Triv -> t&]) (guard (binop? binop)) `(,binop ,t^ ,t&)]
          [(,[Triv -> t^] ,[Triv -> t*] ...) `(,t^ ,t* ...)]
          [,t^ (guard (triv? t^)) t^]
          [,else (invalid who 'Tail else)]
          ))

      (match b
        [(locals (,loc* ...)
           (new-frames (,frame* ...)
             (locate (,home* ...)
               (frame-conflict ,fgraph
                 (call-live (,call-live ...)
                   ,[Tail -> t])))))

         (let* ([dereference (lambda (v)
                               (let ([home (assq v home*)])
                                 (cond
                                   [(frame-var? v) (frame-var->index v)]
                                   [home (frame-var->index (cadr home))]
                                   [else (errorf who "var ~s not recorded in locate form ~s" v home*)]
                                   )))]
                [dereffed (map dereference call-live)])
           (unless (null? dereffed) (set! frame-size (apply max dereffed)))
           (let ([bind* (find-homes call-live fgraph home* frame-size)])
             `(locals (,loc* ...)
                (ulocals ()
                  (locate (,home* ... ,bind* ...)
                    (frame-conflict ,fgraph ,t))))))]
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
