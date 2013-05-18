;; convert-complex-datum.ss
;;
;; part of p423-sp12/srwaggon-p423
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in A14
;; 2012 / 8 / 17
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; revised in A14
;; 2012 / 8 / 17

#!chezscheme
(library (compiler convert-complex-datum)
  (export convert-complex-datum)
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
(define-who (convert-complex-datum program)

  (define let-binds '())
  (define (convert datum)
    (match datum
      [,x (guard (immediate? x)) `(quote ,x)]
      [(,[x] . ,[y]) `(cons ,x ,y)]
      [#(,[x*] ...)
       (let ([tmp (unique-name 'tmp)]
             [len (length x*)])
       `(let ([,tmp (make-vector ',len)])
          (begin
            ,(map (lambda (i v) `(vector-set! ,tmp  ',i ,v)) (iota len) x*) ... ,tmp)))]
      ))
  
  (define (expr program)
    (match program
      [,uvar (guard (uvar? uvar)) uvar]
      [(quote ,immediate) (guard (immediate? immediate)) `(quote ,immediate)]
      [(quote ,datum)
       (let ([tmp (unique-name 'tmp)])
         (set! let-binds (cons `[,tmp ,(convert datum)] let-binds))
         tmp)]
      [(if ,[expr0]  ,[expr1] ,[expr2])
       `(if ,expr0 ,expr1 ,expr2)]
      [(begin ,[expr*] ... ,[expr])
       `(begin ,expr* ... ,expr)]
      [(lambda (,uvar* ...) ,[expr])
       `(lambda (,uvar* ...) ,expr)]
      [(let ([,uvar* ,[expr*]] ...) ,[expr])
       `(let ([,uvar* ,expr*] ...) ,expr)]
      [(letrec ([,uvar* ,[expr*]] ...) ,[expr])
       `(letrec ([,uvar* ,expr*] ...) ,expr]
      [(set! ,uvar ,[expr]) (guard (uvar? uvar))
       `(set! ,uvar ,expr)]
      [(,prim ,[expr*] ...) (guard (prim? prim)) `(,prim ,expr* ...)]
      [(,[expr] ,[expr*] ...) `(,expr ,expr* ...)]
      [,else (invalid who 'expression else)]
      ))

  `(let ,let-binds ,(expr program))

)) ;; end library
