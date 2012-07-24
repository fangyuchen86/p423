;; sanitize-binding-forms.ss
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
(library (compiler sanitize-binding-forms)
  (export sanitize-binding-forms)
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
(define-who (sanitize-binding-forms program)
  (define (Expr expr)
    (match expr
      [,uvar (guard (uvar? uvar)) uvar]
      [(quote ,Immed) `(quote ,Immed)]
      [(if ,[test] ,[consq] ,[alt]) `(if ,test ,consq ,alt)]
      [(begin ,[e*] ... ,[e]) (make-begin `(,e* ... ,e))]
      [(lambda (,uvar* ...) ,[e]) `(lambda (,uvar* ...) ,e)]
      [(let () ,[e]) e]
      [(letrec () ,[e]) e]
      [(let ([,uvar ,[e*]] ...) ,[e])
       (let-values ([(rec reg) (sort-lets uvar e* '() '())])
                   (ret-lets rec reg e))]
      [(letrec ([,uvar (lambda (,uvar* ...) ,[e*])] ...) ,[e])
       `(letrec ([,uvar (lambda (,uvar* ...) ,e*)] ...) ,e)]
      [(,prim ,[e*] ...) (guard (prim? prim)) `(,prim ,e* ...)]
      [(,[e] ,[e*] ...) `(,e ,e* ...)]
      [,else (errorf who "Error in Expr: ~s" else)]))
  (define (sort-lets uvars expers rec reg)
    (if (null? uvars)
        (values rec reg)
        (match (car expers)
          [(lambda (,uvar* ...) ,Expr)
           (sort-lets (cdr uvars)
                      (cdr expers)
                      (cons `(,(car uvars) ,(car expers)) rec)
                      reg)]
          [,x (sort-lets (cdr uvars)
                         (cdr expers)
                         rec
                         (cons `(,(car uvars) ,(car expers)) reg))])))
  (define (ret-lets rec reg Expr)
    (cond
     [(and (null? rec) (null? reg)) Expr]
     [(null? rec) `(let ,reg ,Expr)]
     [(null? reg) `(letrec ,rec ,Expr)]
     [else `(let ,reg
              (letrec ,rec ,Expr))]))
  (Expr program)  

)) ;; end library
