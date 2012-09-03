;; parse-scheme.ss
;;
;; part of p423-sp12/srwaggon-p423
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in A15
;; 2012 / 9 / 2
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; revised in A15
;; 2012 / 9 / 2

#!chezscheme
(library (compiler parse-scheme)
  (export parse-scheme)
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
(define-who (parse-scheme program)



;;; verify-scheme accept a single value and verifies that the value
;;; is a valid program in the current source language.
;;;
;;; The grammar changes from Assignment 13 in that letrec right-hand
;;; sides can now be arbitrary expressions and in the reintroduction
;;; of set! expressions.
;;;
;;; Grammar for verify-scheme (assignment 14):
;;;
;;;  Program --> <Expr>
;;;  Expr    --> <uvar>
;;;           |  (quote <Immediate>)
;;;           |  (if <Expr> <Expr> <Expr>)
;;;           |  (begin <Expr>* <Expr>)
;;;           |  (lambda (<uvar>*) <Expr>)
;;;           |  (let ([<uvar> <Expr>]*) <Expr>)
;;;           |  (letrec ([<uvar> <Expr>]*) <Expr>)
;;;           |  (set! <uvar> <Expr>)
;;;           |  (<primitive> <Expr>*)
;;;           |  (<Expr> <Expr>*)
;;;  Immediate -> <fixnum> | () | #t | #f
;;;
;;; Where uvar is symbol.n, n >= 0
;;;       fixnum is an exact integer
;;;       primitives are void (zero arguments); car, cdr, vector-length,
;;;         make-vector, boolean?, fixnum?, null?, pair?, procedure?,
;;;         vector? (one argument); *, +, -, cons, vector-ref, <, <=, =,
;;;         >=, >, eq?, set-car!, set-cdr! (two arguments); and vector-set!
;;;         (three arguments).
;;;
;;; Within the same Program, each uvar bound by a lambda, let, or letrec
;;; expression must have a unique suffix.
;;;
;;; Machine constraints:
;;;   - each fixnum must be an exact integer n, -2^(k-1) <= n <= 2^(k-1)-1,
;;;     where k is the value of the helpers.ss variable fixnum-bits
;;;
;;; If the value is a valid program, verify-scheme returns the value
;;; unchanged; otherwise it signals an error.

(define-who parse-scheme
  (define primitives
    '((+ . 2) (- . 2) (* . 2) (<= . 2) (< . 2) (= . 2)
      (>= . 2) (> . 2) (boolean? . 1) (car . 1) (cdr . 1)
      (cons . 2) (eq? . 2) (fixnum? . 1) (make-vector . 1)
      (null? . 1) (pair? . 1) (procedure? . 1) (set-car! . 2)
      (set-cdr! . 2) (vector? . 1) (vector-length . 1)
      (vector-ref . 2) (vector-set! . 3) (void . 0)))
  (define (datum? x)
    (define (constant? x)
      (or (memq x '(#t #f ()))
          (and (and (integer? x) (exact? x))
               (or (fixnum-range? x)
                   (error who "integer ~s is out of fixnum range" x)))))
    (or (constant? x)
        (if (pair? x)
            (and (datum? (car x)) (datum? (cdr x)))
            (and (vector? x) (andmap datum? (vector->list x))))))
  (define verify-x-list
    (lambda (x* x? what)
      (let loop ([x* x*] [idx* '()])
        (unless (null? x*)
          (let ([x (car x*)] [x* (cdr x*)])
            (unless (x? x)
              (error who "invalid ~s ~s found" what x))
            (let ([idx (extract-suffix x)])
              (when (member idx idx*)
                (error who "non-unique ~s suffix ~s found" what idx))
              (loop x* (cons idx idx*))))))))
  (define Program
    (lambda (x)
      (define all-uvar* '())
      (define Expr
        (lambda (uvar*)
          (lambda (x)
            (match x
              [,uvar (guard (uvar? uvar))
               (if (memq uvar uvar*)
                   (values)
                   (error who "unbound uvar ~s" uvar))]
              [(quote ,x)
               (unless (datum? x) (error who "invalid datum ~s" x))
               (values)]
              [(if ,[] ,[] ,[]) (values)]
              [(begin ,[] ... ,[]) (values)]
              [(lambda (,fml* ...) ,x)
               (set! all-uvar* (append fml* all-uvar*))
               ((Expr (append fml* uvar*)) x)]
              [(let ([,new-uvar* ,[]] ...) ,x)
               (set! all-uvar* (append new-uvar* all-uvar*))
               ((Expr (append new-uvar* uvar*)) x)]
              [(letrec ([,new-uvar* ,rhs*] ...) ,x)
               (set! all-uvar* (append new-uvar* all-uvar*))
               (let ([p (Expr (append new-uvar* uvar*))])
                 (for-each p rhs*)
                 (p x))]
              [(set! ,uvar ,[])
               (unless (uvar? uvar) (error who "invalid set! lhs ~s" uvar))
               (if (memq uvar uvar*)
                   (values)
                   (error who "unbound uvar ~s" uvar))]
              [(,prim ,x* ...)
               (guard (assq prim primitives))
               (unless (= (length x*) (cdr (assq prim primitives)))
                 (error who "too many or few arguments ~s for ~s" (length x*) prim))
               (for-each (Expr uvar*) x*)
               (values)]
              [(,x ,y ...)
               (guard (and (symbol? x) (not (uvar? x))))
               (error who "invalid Expr ~s" `(,x ,y ...))]
              [(,[] ,[] ...) (values)]
              [,x (error who "invalid Expr ~s" x)]))))
      ((Expr '()) x)
      (verify-x-list all-uvar* uvar? 'uvar)))
  (lambda (x) (Program x) x))

) ;; end library
