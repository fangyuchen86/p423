;; uncover-assigned.ss
;;
;; part of p423-sp12/srwaggon-p423
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in A14
;; 2012 / 8 / 17
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; revised in A14
;; 2012 / 8 / 24

#!chezscheme
(library (compiler uncover-assigned)
  (export uncover-assigned)
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
(define-who (uncover-assigned program)

  (define (Expr program)
    (match program
      [,uvar (guard (uvar? uvar)) (values uvar `())]
      [(quote ,immediate) (guard (immediate? immediate))
       (values `(quote ,immediate) `())]
      [(if ,[t tv*] ,[c cv*] ,[a av*])
       (values `(if ,t ,c ,a) (union tv* cv* av*))]
      [(begin ,[e* e*v] ... ,[e ev])
       (values `(begin ,e* ... ,e) (apply union ev e*v))]
      [(letrec ([,uvar* ,[e* e*v]] ...) ,[b bv])
       (let ([assign (intersection uvar* (apply union bv e*v))])
         (values `(letrec ([,uvar* ,e*] ...)
                    (assigned ,assign ,b))
                 (difference (apply union e*v bv) uvar*)))]
      [(let ([,uvar* ,[e* e*v]] ...) ,[b bv])
       (let ([assign (intersection uvar* (apply union e*v bv))])
         (values `(let ([,uvar* ,e*] ...)
                    (assigned ,assign ,b))
                 (difference (apply union e*v bv) uvar*)))]
      [(lambda ,uvar* ,[b bv])
       (let ([assign (intersection uvar* bv)])
         (values `(lambda ,uvar* (assigned ,assign ,b))
                 (difference bv uvar*)))]
      [(set! ,uvar ,[e ev]) (values `(set! ,uvar ,e) (union `(,uvar) ev))]
      [(,prim ,[e* ev*] ...) (guard (prim? prim))
       (values `(,prim ,e* ...) (apply union ev*))]
      [(,[rator r*v] ,[rand* r**v] ...)
       (values `(,rator ,rand* ...) (apply union r*v r**v))]
      [,else (invalid who 'expression else)]
      ))

  (match program
    [,[Expr -> expr assign] expr]
    )

)) ;; end library
