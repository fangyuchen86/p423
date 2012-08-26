;; convert-assignments.ss
;;
;; part of p423-sp12/srwaggon-p423
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in A14
;; 2012 / 8 / 26
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; revised in A14
;; 2012 / 8 / 26

#!chezscheme
(library (compiler convert-assignments)
  (export convert-assignments)
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
(define-who (convert-assignments expr)
  
  (define (replace expr)
    (match expr
      [(assigned (,x ...) ,e)
       (let ([tmp (unique-name 'convert-assignments)])
         (values
           `(let ((,x (cons ,tmp (void))) ...) ,e)
           tmp)]
      [,else (invalid who 'assign-form else)]
      )))

  (define (Expr expr)
    (match expr

      [,uvar (guard (uvar? uvar))
        uvar]

      [(quote ,immediate) (guard (immediate? immediate))
       `(quote ,immediate)]

      [(begin ,[e*] ... ,[e])
       `(begin ,e* ... ,e)]

      [(if ,[t] ,[c] ,[a])
       `(if ,t ,c ,a)]

      [(letrec ([,uvar* ,[e*]] ...) ,[b])
       `(letrec ([,uvar* ,e*] ...) ,b)]

      [(let ([,uvar* ,[e*]] ...) (assigned ,assigned* ,[b]))
       ]

      [(lambda ,uvar* (assigned ,assigned* ,[b]))
       `(lambda ,uvar* (assigned ,assigned* ,b))]

      [(set! ,uvar ,[e])
       `(set-car! ,uvar ,e)]

      [(,prim ,[e*] ...) (guard (prim? prim))
       `(,prim ,e* ...)]

      [(,[rator] ,[rand*] ...)
       `(,rator ,rand* ...)]

      [,else
       (invalid who 'expression else)]

      ))

  (Expr expr)

)) ;; end library
