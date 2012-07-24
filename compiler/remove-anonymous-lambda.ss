;; remove-anonymous-lambda.ss
;;
;; part of p423-sp12/srwaggon-p423
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in A13
;; 2012 / 7 / 6
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; revised in A13
;; 2012 / 7 / 6

#!chezscheme
(library (compiler remove-anonymous-lambda)
  (export remove-anonymous-lambda)
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
(define-who (remove-anonymous-lambda program)
  (match program
    [,uvar (guard (uvar? uvar)) uvar]
    [(quote ,immediate) (guard (immediate? immediate)) `(quote ,immediate)]
    [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
    [(begin ,[e*] ... ,[e]) `(begin ,e* ... ,e)]
    [(lambda (,uvar* ...) ,[exp])
     (let ([anon (unique-name 'anon)])
       `(letrec ([,anon (lambda (,uvar* ...) ,exp)]) ,anon))]
    [(let ([,uvar (lambda (,uvar* ...) ,[e*])] ...) ,[e])
     `(let ([,uvar (lambda (,uvar* ...) ,e*)] ...) ,e)]
    [(let ([,uvar ,[exp*]] ...) ,[exp])
     `(let ([,uvar ,exp*] ...) ,exp)]
    [(letrec ([,uvar (lambda (,uvar* ...) ,[exp*])] ...) ,[body])
     `(letrec ([,uvar (lambda (,uvar* ...) ,exp*)] ...) ,body)]
    [(,prim ,[exp] ...) (guard (prim? prim))
     `(,prim ,exp ...)]
    [(,[rator] ,[rand] ...) `(,rator ,rand ...)]
    [,else (invalid who 'Program else)])

)) ;; end library
