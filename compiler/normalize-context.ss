;; normalize-context.ss
;;
;; part of p423-sp12/srwaggon-p423
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in A11
;; 2012 / 4 / 29
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; revised in A11
;; 2012 / 4 / 29

#!chezscheme
(library (compiler normalize-context)
  (export normalize-context)
  (import
   ;; Load Chez Scheme primitives:
   (chezscheme)
   ;; Load compiler framework:
   (framework match)
   (framework helpers)
   (compiler helpers)
   )

#| normalize-context : program --> program
|| The goal of normalize-context is to recognize the three distinct contexts below and to
|| rewrite the code so that each kind of expression appears only in contexts that are
|| appropriate for that expression.
||
|| On input to this pass, any expression can appear in any of three contexts:
||
|| o effect, where the resulting value is not needed, e.g., all but the last
||   subexpression of a begin;
||
|| o predicate, where the expression determines flow of control, e.g., the test part of an if;
||
|| o value, where the value is needed, e.g., the right-hand side of a let.
|#
(define-who (normalize-context program)
  (define (make-nopless-begin x*)
    (let ([x* (remove '(nop) x*)])
      (if (null? x*)
          '(nop)
          (make-begin x*))))

  #|
  ||
  |#
  (define (Program p)
    (match p
      [(letrec ([,label (lambda (,uvar* ...) ,[Value -> vl*])] ...) ,[Value -> vl])
       `(letrec ([,label (lambda (,uvar* ...) ,vl*)] ...) ,vl)]
      [,else (invalid who 'Program else)]
      ))

  #|
  ||
  |#
  (define (Value value)
    (define (handle-prim prim vl*)
      (match prim
        [,ef-prim (guard (effect-prim? ef-prim)) (make-nopless-begin `((,prim ,vl* ...) (void)))] ;; nopless-begin
        [,pr-prim (guard (pred-prim?   pr-prim)) `(if (,prim ,vl* ...) '#t '#f)]
        [,vl-prim (guard (value-prim?  vl-prim)) `(,prim ,vl* ...)]
        [,else (invalid who 'Value-prim-context else)]
      ))
    (match value
      [(quote ,[Immediate -> im]) `(quote ,im)]
      [(begin ,[Effect -> ef*] ... ,[vl]) (make-begin `(,ef* ... ,vl))]
      [(if ,[Pred -> pr] ,[c] ,[a]) `(if ,pr ,c ,a)]
      [(let ([,uv* ,[Value -> vl*]] ...) ,[vl]) `(let ([,uv* ,vl*] ...) ,vl)]
      [(,prim ,[vl*] ...) (guard (prim? prim)) (handle-prim prim vl*)]
      [(,[rator] ,[rand*] ...) `(,rator ,rand* ...)]
      [,lbl (guard (label? lbl)) lbl]
      [,uv (guard (uvar? uv)) uv]
      [,else (invalid who 'Value-Context else)]
      ))

  #|
  ||
  |#
  (define (Effect effect)
    (define (handle-prim prim vl*)
      (match prim
        [,ef-prim (guard (effect-prim? ef-prim)) `(,prim ,vl* ...)]
        [,else `(nop)]
      ))
    (match effect
      [,lbl (guard (label? lbl))  `(nop)]
      [,uv  (guard (uvar? uv))    `(nop)]
      [(quote ,[Immediate -> im]) `(nop)]
      [(begin ,[Effect -> ef*] ... ,[ef]) (make-nopless-begin `(,ef* ... ,ef))] ;; nopless-begin
      [(if ,[Pred -> pr] ,[c] ,[a]) `(if ,pr ,c ,a)]
      [(let ([,uv* ,[Value -> vl*]] ...) ,[ef]) `(let ([,uv* ,vl*] ...) ,ef)]
      [(,prim ,[Value -> vl*] ...) (guard (prim? prim)) (handle-prim prim vl*)]
      [(,[Value -> rator] ,[Value -> rand*] ...) `(,rator ,rand* ...)]
      [,else (invalid who 'Effect-Context else)]
      ))

  #|
  ||
  |#
  (define (Pred pred)
    (define (handle-prim prim vl*)
      (match prim
        [,pr-prim (guard (pred-prim?   pr-prim)) `(,prim ,vl* ...)]
        [,ef-prim (guard (effect-prim? ef-prim)) (make-nopless-begin `((,prim ,vl* ...) (true)))] ;; ehhh.
        [,else `(if (eq? (,prim ,vl* ...) '#f) (false) (true))]
        ))
    (match pred
      [void `(nop)]
      [(begin ,[Effect -> ef*] ... ,[pr]) (make-begin `(,ef* ... ,pr))]
      [(if ,[Pred -> pr] ,[c] ,[a]) `(if ,pr ,c ,a)]
      [(let ([,uv* ,[Value -> vl*]] ...) ,[pr]) `(let ([,uv* ,vl*] ...) ,pr)]
      [(quote ,[Immediate -> im]) (if (eq? im #f) '(false) '(true))]
      [(,prim ,vl* ...) (guard (prim? prim)) (handle-prim prim vl*)]
      [,const (guard (or (uvar? const) (label? const)))
              `(if (eq? ,const '#f) (false) (true))]
      [,else (invalid who 'Pred-Context else)]
      ))

  
  (define (Immediate immediate)
    (match immediate
      [() '()]
      [#t '#t]
      [#f '#f]
      [,fixnum (guard (fixnum? fixnum)) fixnum]
      [,else (invalid who 'Immediate else)]
      ))

  (Program program)

)) ;; end library
