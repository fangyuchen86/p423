;; optimize-known-call.ss
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
(library (compiler optimize-known-call)
  (export optimize-known-call)
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
(define-who (optimize-known-call program)
  (define (Expr env)
    (lambda (program)
      (match program
        [,uvar (guard (uvar? uvar)) uvar]
        [,label (guard (label? label)) label]
        [(quote ,immediate) 
         `(quote ,immediate)]
        [(if ,[test] ,[conseq] ,[altern]) `(if ,test ,conseq ,altern)]
        [(begin ,[e*] ... ,[e]) (make-begin `(,e* ... ,e))]
        [(let ([,uvar* ,[e*]] ...) ,[e])
         `(let ([,uvar* ,e*] ...) ,e)]
        [(letrec ([,label (lambda (,cp* ,uvar** ...)
                            (bind-free (,cp* ,free** ...) ,[e*]))] ...)
           (closures ([,lab-name* ,label* ,free** ...] ...) ,[e]))
         (let* ([env (map list lab-name* label*)]
                [proc (Expr env)]
                [e* (map proc e*)]
                [e (proc e)])
           `(letrec ([,label (lambda (,cp* ,uvar** ...)
                               (bind-free (,cp* ,free** ...) ,e*))] ...)
              (closures ([,lab-name* ,label* ,free** ...] ...) ,e)))]
        [(,prim ,[e*] ...) (guard (prim? prim)) `(,prim ,e* ...)]
        [(,[e] ,[e*] ...)
         (let ([tmp (assq e env)])
           (if tmp
               `(,(cadr tmp) ,e* ...)
               `(,e ,e* ...)))]
        [,else (errorf who "Error in Expr:  ~s" else)])))
    (match program
      [,[(Expr '()) -> expr] expr]
      [,else (errorf who "Error in Program: ~s" else)])

)) ;; end library
