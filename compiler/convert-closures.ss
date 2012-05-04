;; convert-closures.ss
;;
;; part of p423-sp12/srwaggon-p423
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in A12
;; 2012 / 5 / 4
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; revised in A12
;; 2012 / 5 / 4

#!chezscheme
(library (compiler convert-closures)
  (export convert-closures)
  (import
   ;; Load Chez Scheme primitives:
   (chezscheme)
   ;; Load compiler framework:
   (framework match)
   (framework helpers)
   (compiler helpers)
   )

#| This pass converts lambda expressions with free variables into lamba
|| expressions without free variables and makes explicit the representation
|| of each procedure as a closure that encapsulates a pointer to the
|| procedure's code and the value of its free variables. It does so by:
||
|| o adding a new formal parameter, cp, to each lambda expression, for which
||   the corresponding actual parameter is the procedure's own closure, through
||   which it can access the values of its free variables;
||
|| o replacing the variables that the input-language letrec expression
||   binds with labels;
||
|| o introducing a closures form within each letrec expression that creates a
||   closure for each lambda expression, implicity storing within the closure
||   a pointer to the code (obtained via the code's new label) and the values
||   of its free variables; and
||
|| o converting each procedure call to pass along the closure as an explicit
||   additional argument.
|#
(define-who (convert-closures program)
    
  (define (Immediate immediate)
    (match immediate
      [,im (guard (immediate? im)) im]
      [,else (invalid who 'Immediate else)]
      ))

  (define (Expr expr)
    (match expr
      [,uv (guard (uvar? uv)) uv]
      [(quote ,[Immediate -> im]) `(quote ,im)]
      [(if ,[t] ,[c] ,[a]) `(if ,t ,c ,a)]
      [(begin ,[ex*] ... ,[ex]) `(begin ,ex* ... ,ex)]
      [(let ([,uv* ,[ex*]] ...) ,[ex]) `(let ([,uv* ,ex*] ...) ,ex)]
      [(letrec ([,uv* (lambda (,uv** ...) (free (,free* ...) ,[ex*]))] ...) ,[ex])
       (let ([cp*  (map unique-name (make-list (length uv*) 'cp))]
             [lbl* (map unique-label uv*)])
         `(letrec ([,lbl* (lambda (,cp* ,uv** ...)
                            (bind-free (,cp* ,free* ...) ,ex*))] ...)
            (closures ([,uv* ,lbl* ,free* ...] ...) ,ex))
         )]
      [(,prim ,[ex*] ...) (guard (prim? prim)) `(,prim ,ex* ...)]
      [(,[rator] ,[rand*] ...)
       (if (uvar? rator)
           `(,rator ,rator ,rand* ...)
           (let ([tmp (unique-name 'tmp)])
             `(let ([,tmp ,rator])
                (,tmp ,tmp ,rand* ...))))]
      [,else (invalid who 'Expr else)]
      ))

  (Expr program)

)) ;; end library