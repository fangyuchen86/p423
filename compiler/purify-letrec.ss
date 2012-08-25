;; purify-letrec.ss
;;
;; part of p423-sp12/srwaggon-p423
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in A14
;; 2012 / 8 / 24
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; revised in A14
;; 2012 / 8 / 24

#!chezscheme
(library (compiler purify-letrec)
  (export purify-letrec)
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
(define-who (purify-letrec program)

  ;; bind* expr* --> simple* lambda* complex*
  (define (partition bind* expr*)
    (if (null? bind*)
        (values '() '() '())
        

  (define (Expr expr)
    (match expr
      [,uvar (guard (uvar? uvar)) uvar]
      [(quote ,immediate) (guard (immediate? immediate))
       `(quote ,immediate)]
      [(begin ,[e*] ... ,[e])
       `(begin ,e* ... ,e)]
      [(if ,[t] ,[c] ,[a])
       `(if ,t ,c ,a)]

      [(letrec ([,uvar* ,[e*]] ...) (assigned ,assigned* ,[b]))
       (let*-values ([(simple* lambda* complex*) (partition uvar* e*)])


       `(letrec ([,uvar* ,e*] ...) ,b)]


      [(let ([,uvar* ,[e*]] ...) (assigned ,assigned* ,[b]))
       `(let ([,uvar* ,e*] ...) (assigned ,assigned* ,b))]
      [(lambda ,uvar* (assigned ,assigned* ,[b]))
       `(lambda ,uvar* (assigned ,assigned* ,b))]
      [(set! ,uvar ,[e]) `(set! ,uvar ,e)]
      [(,prim ,[e*] ...) (guard (prim? prim))
       `(,prim ,e* ...)]
      [(,[rator] ,[rand*] ...)
       `(,rator ,rand* ...)]
      [,else (invalid who 'expression else)]
      ))

  (Expr program)

)) ;; end library
