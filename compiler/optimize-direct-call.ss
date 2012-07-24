;; optimize-direct-call.ss
;;
;; part of p423-sp12/srwaggon-p423
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in A13
;; 2012 / 5 / 20
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; revised in A13
;; 2012 / 5 / 20

#!chezscheme
(library (compiler optimize-direct-call)
  (export optimize-direct-call)
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
(define-who (optimize-direct-call program)
  
  (define (Expr expr)
    (match expr
      [,uvar (guard (uvar? uvar)) uvar]
      [(quote ,im) (guard (immediate? im)) `(quote ,im)]
      [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq, altern)]
      [(begin ,[expr*] ... ,[expr^]) `(begin ,expr* ... ,expr^)]
      [((lambda (,uvar* ...) ,[body]) ,param* ...)
       `(let ([,uvar* ,param*] ...))]
      [(lambda (,uvar* ...) ,[expr^]) `(lambda (,uvar* ...) ,expr^)]
      [(let ([,uvar ,[expr*]] ...) ,[expr^])
       `(let ([,uvar ,expr*]) ... ,expr^)]
      [(letrec ([,uvar (lambda (,uvar** ...) ,[body])] ...) ,[expr^])
       `(letrec ([,uvar (lambda (,uvar** ...) ,body) ...]) ,expr^)]
      [(,prim ,[expr*] ...) (guard (prim? prim)) `(,prim ,expr* ...)]
      [(,[rator] ,[rand*] ...) `(,rator ,rand* ...)]
      [,else (invalid who 'Expr else)]
      ))
    
  ;;(Expr program)
  program

)) ;; end library
