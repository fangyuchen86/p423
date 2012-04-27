;; specify-representation.ss
;;
;; part of p423-sp12/srwaggon-p423
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in A10
;; 2012 / 4 / 26
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; revised in A10
;; 2012 / 4 / 26

#!chezscheme
(library (compiler specify-representation)
  (export specify-representation)
  (import
   ;; Load Chez Scheme primitives:
   (chezscheme)
   ;; Load compiler framework:
   (framework match)
   (framework helpers)
   (compiler helpers)
   )

#| specify-representation : program --> program
|| specify-representation converts all Scheme datatypes
|| to their ptr equivalents so they are represented as
|| integers in the output language, and at the same time,
|| translate calls to Scheme primitives into calls to UIL
|| primitives.
|#
(define-who (specify-representation program)

  #|
  ||
  |#
  (define (Immediate immediate)
    (match immediate
      [() '()]
      [#t #t]
      [#f #f]
      [,fixnum (guard (fixnum? fixnum)) fixnum]
      [,else (invalid who 'Immediate else)]
      ))

  #|
  ||
  |#
  (define (Effect effect)
    (match effect
      [(nop) '(nop)]
      [(begin ,[Effect -> ef*] ... ,[ef]) (make-begin `(,ef* ... ,ef))]
      [(if ,[Pred -> pr] ,[c] ,[a]) `(if ,pr ,c ,a)]
      [(let ([,uv* ,[Value -> vl*]] ...) ,[ef]) `(let ([,uv* ,vl*] ...) ,ef)]
      [(,ef-prim ,[Value -> vl*] ...) (guard (effect-prim? ef-prim)) `(,ef-prim ,vl* ...)]
      [(,[Value -> rator] ,[Value -> rand*] ...) `(,rator ,rand* ...)]
      [,else (invalid who 'Effect else)]
      ))

  #|
  ||
  |#
  (define (Pred pred)
    (match pred
      [(true) '(true)]
      [(false) '(false)]
      [(begin ,[Effect -> ef*] ... ,[pr]) (make-begin `(,ef* ... ,pr))]
      [(if ,[pr] ,[c] ,[a]) `(if ,pr ,c ,a)]
      [(let ([,uv* ,[Value -> vl*]] ...) ,[pr]) `(let ([,uv* ,vl*] ...) ,pr)]
      [(,pr-prim ,[Value -> vl*] ...) (guard (pred-prim? pr-prim)) `(,pr-prim ,vl)]
      [,else (invalid who 'Pred else)]
      ))

  #|
  ||
  |#
  (define (Value value)
    (match value
      [(begin ,[Effect -> ef*] ... ,[vl]) (make-begin `(,ef* ... ,vl))]
      [(if ,[Pred -> pr] ,[c] ,[a]) `(if ,pr ,c ,a)]
      [(let ([,uv* ,[vl*]] ...) ,[vl]) `(let ([,uv* ,vl*] ...) ,vl)]
      [(quote ,[Immediate -> im]) `(quote ,im)]
      [(,vl-prim ,[vl*] ...) (guard (value-prim? vl-prim) `(,vl-prim ,vl* ...)]
      [(,[rator] ,[rand*] ...) `(,rator ,rand* ...)]
      [,uv (guard (uvar? uv)) uv]
      [,lbl (guard (label? lbl)) lbl]
      [,else (invalid who 'Value else)]
      ))

  #|
  ||
  |#
  (define (Program p)
    (match p
      [(letrec ([,label (lambda (,uvar* ...) ,[Value -> vl*])] ...) ,[Value -> vl])
       `(letrec ([,label (lambda (,uvar* ...) ,vl*)] ...) ,vl)]
      [,else (invalid who 'Program else)]
      ))

  (Program program)

)) ;; end library
