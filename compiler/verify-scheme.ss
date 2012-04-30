;;; Official Verify Scheme

(library (compiler verify-scheme)
  (export verify-scheme)
  (import
    (chezscheme)
    (framework match)
    (framework helpers))

;;; Andy Keep, Kent Dybvig
;;; P423/P523
;;; Spring 2010

;;; verify-scheme accept a single value and verifies that the value
;;; is a valid program in the current source language.
;;;
;;; Grammar for verify-scheme (assignment 11):
;;;
;;;  Program --> <Expr>
;;;  Expr   --> <label>
;;;          |  <uvar>
;;;          |  (quote <Immediate>)
;;;          |  (if <Expr> <Expr> <Expr>)
;;;          |  (begin <Expr>* <Expr>)
;;;          |  (let ([<uvar> <Expr>]*) <Expr>)
;;;          |  (letrec ([<label> (lambda (<uvar>*) <Expr>)]*) <Expr>)
;;;          |  (<primitive> <Expr>*)
;;;          |  (<Expr> <Expr>*)
;;;  Immediate -> <fixnum> | () | #t | #f
;;;
;;; Where uvar is symbol.n, n >= 0
;;;       label is symbol$n, n >= 0
;;;       fixnum is an exact integer
;;;       primitives are void (zero arguments); car, cdr, vector-length,
;;;         make-vector, boolean?, fixnum?, null?, pair?, vector? (one
;;;         argument); *, +, -, cons, vector-ref, <, <=, =, >=, >, eq?,
;;;         set-car!, set-cdr!  (two arguments); and vector-set! (three
;;;         arguments).
;;;
;;; Within the same Program, each label bound by a letrec expression
;;; must have a unique suffix, and each uvar bound by a lambda or let
;;; expression must have a unique suffix.
;;;
;;; Machine constraints:
;;;   - each fixnum must be an exact integer n, -2^(k-1) <= n <= 2^(k-1)-1,
;;;     where k is the value of the helpers.ss variable fixnum-bits
;;;
;;; If the value is a valid program, verify-scheme returns the value
;;; unchanged; otherwise it signals an error.

(define-who verify-scheme
  (define primitives
    '((+ . 2) (- . 2) (* . 2) (<= . 2) (< . 2) (= . 2)
      (>= . 2) (> . 2) (boolean? . 1) (car . 1) (cdr . 1)
      (cons . 2) (eq? . 2) (fixnum? . 1) (make-vector . 1)
      (null? . 1) (pair? . 1) (set-car! . 2) (set-cdr! . 2)
      (vector? . 1) (vector-length . 1) (vector-ref . 2)
      (vector-set! . 3) (void . 0)))
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
      (define all-label* '())
      (define all-uvar* '())
      (define Expr
        (lambda (label* uvar*)
          (lambda (x)
            (match x
              [,label (guard (label? label))
               (if (memq label label*)
                   (values)
                   (error who "unbound label ~s" label))]
              [,uvar (guard (uvar? uvar))
               (if (memq uvar uvar*)
                   (values)
                   (error who "unbound uvar ~s" uvar))]
              [(quote ,[Immediate ->]) (values)]
              [(if ,[] ,[] ,[]) (values)]
              [(begin ,[] ... ,[]) (values)]
              [(let ([,new-uvar* ,[]] ...) ,x)
               (set! all-uvar* (append new-uvar* all-uvar*))
               ((Expr label* (append new-uvar* uvar*)) x)]
              [(letrec ([,new-label* (lambda (,fml** ...) ,x*)] ...) ,x)
               (set! all-label* (append new-label* all-label*))
               (let ([label* (append new-label* label*)])
                 (for-each
                   (lambda (fml* x)
                     (set! all-uvar* (append fml* all-uvar*))
                    ; pass along fml* only---no free variables yet!
                     ((Expr label* fml*) x))
                   fml**
                   x*)
                 ((Expr label* uvar*) x))]
              [(,prim ,x* ...)
               (guard (assq prim primitives))
               (unless (= (length x*) (cdr (assq prim primitives)))
                 (error who "too many or few arguments ~s for ~s" (length x*) prim))
               (for-each (Expr label* uvar*) x*)
               (values)]
              [(,x ,y ...)
               (guard (and (symbol? x) (not (or (uvar? x) (label? x)))))
               (error who "invalid Expr ~s" `(,x ,y ...))]
              [(,[] ,[] ...) (values)]
              [,x (error who "invalid Expr ~s" x)]))))
      (define (Immediate imm)
        (cond
          [(memq imm '(#t #f ())) (values)]
          [(and (integer? imm) (exact? imm))
           (unless (fixnum-range? imm)
             (error who "integer ~s is out of fixnum range" imm))
           (values)]
          [else (error who "invalid Immediate ~s" imm)]))
      ((Expr '() '()) x)
      (verify-x-list all-label* label? 'label)
      (verify-x-list all-uvar* uvar? 'uvar)))
  (lambda (x) (Program x) x))

)

