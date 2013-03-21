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
;; 2013 / 3 / 21

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

;;; parse-scheme accept a single value and verifies that the value
;;; is a valid program in the current source language.
;;;
;;;
;;; Grammar for parse-scheme (assignment 15):
;;;
;;;  Program --> <Expr>
;;;  Expr    --> 
;;;           |  <uvar>
;;;           |  <Immediate>
;;;           |  (quote <Datum>)
;;;           |  (if <Expr> <Expr>)
;;;           |  (if <Expr> <Expr> <Expr>)
;;;           |  (and <Expr>*) 
;;;           |  (or <Expr>*
;;;           |  (begin <Expr>* <Expr>)
;;;           |  (lambda (<uvar>*) <Expr>)
;;;           |  (let ([<uvar> <Expr>]*) <Expr>*)
;;;           |  (letrec ([<uvar> <Expr>]*) <Expr>*)
;;;           |  (set! <uvar> <Expr>)
;;;           |  (<primitive> <Expr>*)
;;;           |  (<Expr> <Expr>*)
;;;  Immediate-> <fixnum> | () | #t | #f
;;;
;;;
;;; Where uvar is symbol.n, n >= 0
;;;   fixnum is an exact integer
;;;   primitives are void (zero arguments); car, cdr, vector-length,
;;;     make-vector, boolean?, fixnum?, null?, pair?, procedure?,
;;;     vector? (one argument); *, +, -, cons, vector-ref, <, <=, =,
;;;     >=, >, eq?, set-car!, set-cdr! (two arguments); and vector-set!
;;;     (three arguments),
;;;  datum is a constant, a pair of datums, or a vector of datums.
;;;
;;;
;;; Within the same Program, each uvar bound by a lambda, let, or letrec
;;; expression must have a unique suffix.
;;;
;;; Machine constraints:
;;;   - each fixnum must be an exact integer n, -2^(k-1) <= n <= 2^(k-1)-1,
;;;     where k is the value of the helpers.ss variable fixnum-bits
;;;
;;; If the value is a valid program, parse-scheme returns the value
;;; unchanged; otherwise it signals an error.
;;;
;;;
;;; This pass has several tasks to perform:
;;;
;;;  o  verify that the syntax of the input program is correct;
;;;  o  verify that there are no unbound variables;
;;;  o  convert all variables to unique variables, handling the shadowing of
;;;     identiers (other variables, keyword names, and primitive names) correctly;
;;;  o  convert unquoted constants into quoted constants;
;;;  o  verify that each constant and quoted datum is well formed, with 
;;;     each xnum in the xnum range;
;;;  o  rewrite not calls, and expressions, or expressions, and
;;;     one-armed if expressions in terms of the other language expressions.







(define-who (parse-scheme program)

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
            (and (vector? x) (and (map datum? (vector->list x)))))))

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

  (define (replace-params params env)
    (match params
      [() '()]
      [,x (guard (assq x env)) (cdr (assq x env))]
      [(,[x] . ,[y]) `(,x . ,y)]
      [,x (error who "not replaced: ~s" x )]
      ))



  (define (Program x)
    
    (define all-uvar* '())
    
    (define (Expr env uvar*)
      (lambda (x)
        (match x
          [,k (guard (or (immediate? k) (fixnum? k) (integer? k)))
              `(quote ,k)]
          
          [,id (guard (symbol? id))
               (if (assq id env)
                   (cdr (assq id env))
                   (error who "unbound variable ~s" id))]
          
          [(quote ,x)
           (unless (datum? x) (error who "invalid datum ~s" x))
           `(quote ,x)]
         
          [(not ,[(Expr env uvar*) -> e]) `(if ,e '#f '#t)]
          [(and) '#t]
          [(and ,[(Expr env uvar*) -> e]) e]
          [(and ,[(Expr env uvar*) -> e] ,[(Expr env uvar*) -> e*] ...)
           `(if ,e ,((Expr env uvar*) `(and ,e* ...)) '#f)]
          [(or) '#f]
          [(or ,[(Expr env uvar*) -> e]) e]
          [(or ,[(Expr env uvar*) -> e] ,[(Expr env uvar*) -> e*] ...)
           (let ([tmp (unique-name who)])
             `(let ([,tmp ,e])
                (if ,tmp ,tmp ,((Expr env uvar*) `(or ,e* ...)))))]

 
          [(if ,[(Expr env uvar*) -> t] ,[(Expr env uvar*) -> c] ,[(Expr env uvar*) -> a])
           `(if ,t ,c ,a)]
          
          [(begin ,[(Expr env uvar*) -> e*] ... ,[(Expr env uvar*) -> e])
           `(begin ,e* ... ,e)]
          
          [(lambda (,fml* ...) ,x)
           (let* ([env+ (map (lambda (fml)
                              (cons fml (unique-name fml))) fml*)]
                  [rep (replace-params fml* env+)])
             (set! all-uvar* (append rep all-uvar*))
             `(lambda (,rep ...)
                ,((Expr `(,(append env+ env) ...) (append rep uvar*)) x)))]

          [(let ([,new-uvar* ,[(Expr env uvar*) -> x*]] ...) ,e ,e+ ...)
           (let* ([env+ (map (lambda (var)
                               (cons var (unique-name var))) new-uvar*)]
                  [rep (replace-params new-uvar* env+)])
             (set! all-uvar* (append rep all-uvar*))
             `(let ([,rep ,x*] ...)
                ,(make-begin
                  (if (null? e+)
                      `(,((Expr (append env+ env) (append rep uvar*)) e))          
                      `(,((Expr (append env+ env) (append rep uvar*)) e)
                        ,((Expr (append env+ env) (append rep uvar*)) e+) ...)))))]

          [(letrec () ,[(Expr env uvar*) -> e] ,[(Expr env uvar*) -> e+] ...)
           (if (null? e+)
               `(letrec () ,e)
               `(letrec () ,(make-begin `(,e ,e+ ...))))]
          
          [(letrec ([,new-uvar* ,e*] ...) ,e ,e+ ...)
           (let* ([env+ (map (lambda (var)
                               (cons var (unique-name var))) new-uvar*)]
                  [rep (replace-params new-uvar* env+)])
             (set! all-uvar* (append rep all-uvar*))
             (if (null? e+)
                 `(letrec ([,rep ,(map (Expr (append env+ env) (append rep uvar*)) e*)] ...)
                    ,((Expr (append env+ env) (append rep uvar*)) e))
                 `(letrec ([,rep ,((Expr (append env+ env) (append rep uvar*)) e*)] ...)
                    ,((Expr (append env+ env) (append rep uvar*)) e)
                    ,((Expr (append env+ env) (append rep uvar*)) e+) ...)
                 ))]
          
          [(set! ,uvar ,[(Expr env uvar*) -> x])
           (unless (uvar? uvar) (error who "invalid set! lhs ~s" uvar))
           (if (assq uvar env)
               `(set! ,(assq uvar env) ,x)
               (error who "unbound uvar ~s" uvar))]
          
          [(,prim ,[(Expr env uvar*) -> x*] ...)
           (guard (assq prim primitives))
           (unless (= (length x*) (cdr (assq prim primitives)))
             (error who "too many or few arguments ~s for ~s" (length x*) prim))
           ;(for-each (Expr uvar*) x*)
           `(,prim ,x* ...)]
          
          ;[(,x ,y ...)
           ;(guard (and (symbol? x) (not (uvar? x))))
           ;(error who "invalid Expr ~s" `(,x ,y ...))]
          
          [(,[(Expr env uvar*) -> rator] ,[(Expr env uvar*) -> rand*] ...)
           `(,rator ,rand* ...)]
          
          [,x (error who "invalid Expr ~s" x)])))
    
    (let ([x ((Expr '() '()) x)])
      (verify-x-list all-uvar* uvar? 'uvar)
      x))
  
  (Program program)
  
  )) ;; end library
