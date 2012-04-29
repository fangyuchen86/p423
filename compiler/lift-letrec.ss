;; lift-letrec.ss
;;
;; part of p423-sp12/srwaggon-p423
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in A11
;; 2012 / 4 / 29
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; revised in A11
;; 2012 / 4 / 29

#!chezscheme
(library (compiler lift-letrec)
  (export lift-letrec)
  (import
   ;; Load Chez Scheme primitives:
   (chezscheme)
   ;; Load compiler framework:
   (framework match)
   (framework helpers)
   (compiler helpers)
   )

;; lift-letrec : program --> program
;; This pass simply moves all of the letrec bindings from where they
;; appear into a letrec expression wrapped around the outermost
;; expression, removing all of the internal letrec expressions in the
;; process.
;; 
;; 
;; input grammar:
;; Program    -->  <Expr>
;; Expr       -->  <label>
;;             |   <uvar>
;;             |   (quote <Immediate>)
;;             |   (if <Expr> <Expr> <Expr>)
;;             |   (begin <Expr>* <Expr>)
;;             |   (let ([<uvar> <Expr>]*) <Expr)
;;             |   (letrec ([<label> (lambda (<uvar>*) <Expr>)]*) <Expr>)
;;             |   (<prim> <Expr>*)
;;             |   (<Expr> <Expr>*)
;; Immediate  -->  <fixnum> | () | #t | #f
;;
;;
;; output grammar:
;; Program    -->  (letrec ([<label> (lambda (<uvar>*) <Expr>)]*) <Expr>)
;; Expr       -->  <label>
;;             |   <uvar>
;;             |   (quote <Immediate>)
;;             |   (if <Expr> <Expr> <Expr>)
;;             |   (begin <Expr>* <Expr>)
;;             |   (let ([<uvar> <Expr>]*) <Expr)
;;             |   (<prim> <Expr>*)
;;             |   (<Expr> <Expr>*)
;; Immediate  -->  <fixnum> | () | #t | #f
;;
;; The output grammar differs only insofar as the lifting of letrec forms.

(define-who (lift-letrec program)

  (define bindings '())
  (define (add-bindings bind*)
    (if (null? bind*)
        '()
        (begin (set! bindings (cons (car bind*) bindings))
              (add-bindings (cdr bind*)))))

  #|
  ||
  |#
  (define (Program program)
    (match program
      [,[Expr -> ex] `(letrec ,bindings ,ex)]
      [,else (invalid who 'Program else)]
      ))

  #|
  ||
  |#
  (define (Expr expr)
    (match expr
      [(quote ,[Immediate -> im]) `(quote ,im)]
      [(if ,[e0] ,[e1] ,[e2]) `(if ,e0 ,e1 ,e2)]
      [(begin ,[ex*] ... ,[ex]) `(begin ,ex* ... ,ex)]
      [(let ([,uv ,[ex*]] ...) ,[ex]) `(let ([,uv ,ex*] ...) ,ex)]
      [(letrec ,[Bind* -> bind*] ,[ex])
       (add-bindings bind*)
       ex]
      [(,prim ,[ex*] ...) (guard (prim? prim)) `(,prim ,ex* ...)]
      [(,[ex] ,[ex*] ...) `(,ex ,ex* ...)]      
      [,lbl (guard (label? lbl)) lbl]
      [,uv (guard (uvar? uv)) uv]
      [,else (invalid who 'Expression else)]
      ))

  (define (Bind* bind*)
    (match bind*
      [() '()]
      [([,lbl* (lambda (,uv* ...) ,[Expr -> ex*])] ...)
       `([,lbl* (lambda (,uv* ...) ,ex*)] ...)]
      [,else (invalid who 'letrec-binding else)]
    ))

  #|
  ||
  |#
  (define (Immediate immediate)
    (match immediate
      [() '()]
      [#t '#t]
      [#f '#f]
      [,fixnum (guard (fixnum? fixnum)) fixnum]
      [,else (invalid who 'Immediate else)]
      ))

  (Program program)

)) ;; end library
