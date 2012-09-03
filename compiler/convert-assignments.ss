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
  

  (define (convert-binding uvar* expr* assign*)
    (cond
      [(null? uvar*) (values '() '())]
      [else (let-values ([(bind* tmp*)
                          (convert-binding (cdr uvar*) (cdr expr*) assign*)])
              (let ([x (car uvar*)]
                    [e (car expr*)])
                (if (memq x assign*)
                    (let ([tmp (unique-name who)])
                      (values `((,tmp ,e) . ,bind*) `((,x . ,tmp) . ,tmp*)))
                    (values `((,x ,e) . ,bind*) tmp*))))]
  ))


  (define (convert-formal uvar* assign*)
    (cond
      [(null? uvar*) (values '() '())]
      [else (let-values ([(formal* tmp*) (convert-formal (cdr uvar*) assign*)])
              (let ([x (car uvar*)])
                (if (memq x assign*)
                    (let ([tmp (unique-name who)])
                      (values `(,tmp . ,formal*) `((,x . ,tmp) . ,tmp*)))
                    (values `(,x . ,formal*) tmp*))))]
  ))


  (define (zip-bind* assign* tmp*)
    (map (lambda (x)
           `(,x (cons ,(cdr (assq x tmp*)) (void))))
         assign*)
  )


  (define (Expr new*)
    (lambda (expr)
      (match expr
        
        [,uvar (guard (uvar? uvar))
          (if (memq uvar new*) `(car ,uvar) uvar)]

        [(quote ,immediate) (guard (immediate? immediate))
         `(quote ,immediate)]
        
        [(begin ,[e*] ... ,[e])
         `(begin ,e* ... ,e)]
        
        [(if ,[t] ,[c] ,[a])
         `(if ,t ,c ,a)]
        
        [(letrec ([,uvar* ,[e*]] ...) ,[b])
         `(letrec ([,uvar* ,e*] ...) ,b)]
        
        [(let ([,uvar* ,[e*]] ...) (assigned ,assigned* ,b))
         (let*-values ([(body) ((Expr (append new* assigned*)) b)]
                       [(bind* tmp*) (convert-binding uvar* e* assigned*)]
                       [(assign-bind*) (zip-bind* assigned* tmp*)])
           `(let ,bind*
              (let ,assign-bind* ,body)))]
        
        [(lambda ,uvar* (assigned ,assigned* ,b))
         (let*-values ([(body) ((Expr (append new* assigned*)) b)]
                       [(formal* tmp*) (convert-formal uvar* assigned*)]
                       [(assign-bind*) (zip-bind* assigned* tmp*)])
         `(lambda ,formal*
            (let ,assign-bind* ,body)))]

        [(set! ,uvar ,[e])
         `(set-car! ,uvar ,e)]
        
        [(,prim ,[e*] ...) (guard (prim? prim))
         `(,prim ,e* ...)]
        
        [(,[rator] ,[rand*] ...)
         `(,rator ,rand* ...)]
        
        [,else
         (invalid who 'expression else)]
        
  )))

  ((Expr '()) expr)

)) ;; end library
