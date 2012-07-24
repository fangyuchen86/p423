;; introduce-procedure-primitives.ss
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
(library (compiler introduce-procedure-primitives)
  (export introduce-procedure-primitives)
  (import
   ;; Load Chez Scheme primitives:
   (chezscheme)
   ;; Load compiler framework:
   (framework match)
   (framework helpers)
   (compiler helpers)
   )

#| This pass completes closure conversion by introducing concrete procedure
|| manipulation primitives and eliminating the bind-free and closure
|| forms. The new procedure-manipulation primitives, described below,
|| do explicitly what bind-free and closures do implicitly.
||
|| o (make-procedure code size) allocates a new procedure (closure) with code
||   as its code pointer and size
||
|| o (procedure-code proc) extracts the code pointer from proc.
||
|| o (procedure-ref proc i) extracts the value from the ith free-variable slot of proc.
||
|| o (procedure-set! proc i val) stores val in the ith free-variable slot of proc.
|#
(define-who (introduce-procedure-primitives program)

  (define (index var ls)
    (letrec ([help
              (lambda (var ls count)
                (cond
                  [(null? ls) #f]
                  [(eq? (car ls) var) count]
                  [else (help var (cdr ls) (add1 count))]))])
      (help var ls 0)))

  (define (call-proc-ref body cp free)
    (match body
      [,x (guard (member x free))
          `(procedure-ref ,cp (quote ,(index x free)))]
      [(,[x] . ,[y]) `(,x . ,y)]
      [,else else]))
  
  (define (call-make-proc name lbl free)
    `[,name (make-procedure ,lbl (quote ,(length free)))])

  (define (call-proc-set! name free)
    (letrec ([help
              (lambda (name ls)
                (cond
                  [(null? ls) '()]
                  [else
                   (cons `(procedure-set! ,name (quote ,(index (car ls) free)) ,(car ls))
                         (help name (cdr ls)))]))])
      (help name free)))
    
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

      [(letrec ([,uv* (lambda (,cp* ,uv** ...) (bind-free (,cp* ,free* ...) ,[ex*]))] ...)
         (closures ([,name* ,lbl* ,free* ...] ...) ,[ex]))

       (let* ([ex* (map call-proc-ref ex* cp* free*)]
              [bind* (map call-make-proc name* lbl* free*)]
              [begins* (map call-proc-set! name* free*)])
       `(letrec ([,lbl* (lambda (,cp* ,uv** ...)
                          ,ex*)] ...)
          (let ,bind*
            ,(make-begin `(,begins* ... ... ,ex)))))]
      [(,prim ,[ex*] ...) (guard (prim? prim)) `(,prim ,ex* ...)]
      [(,label ,[rand*] ...) (guard (label? label)) `(,label ,rand* ...)]
      [(,[rator] ,[rand*] ...) `((procedure-code ,rator) ,rand* ...)]
      [,else (invalid who 'Expr else)]
      ))

  (Expr program)


)) ;; end library