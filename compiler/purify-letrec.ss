;; purify-letrec.ss
;;
;; part of p423-sp12/srwaggon-p423
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in A14
;; 2012 / 8 / 24
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; revised in A14
;; 2012 / 8 / 24

#!chezscheme
(library (compiler purify-letrec)
  (export purify-letrec)
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
(define-who (purify-letrec program)

  ;; bind* expr* --> simple* lambda* complex*
  ;; Separates bindings according to whether they are simple, lambda,
  ;; or complex as according to the follows:
  ;; simple if x is not assigned (not in the list (x! ...)) and e is
  ;; a simple expression.
  ;; lambda if x is not assigned and e is a lambda expression
  ;; complex otherwise
  (define (partition bind* expr* lhs*)
    (if (null? bind*)
        (values '() '() '())
        (let ([uvar (caar bind*)]
              [expr (cadar bind*)]
              [rest (cdr bind*)])
          (let-values ([(simple* lambda* complex*)
                        (partition rest expr* lhs*)])
            (cond
              [(and (not (memq uvar expr*)) ((simple? #f lhs*) expr))
               (values `((,uvar ,expr) . ,simple*) lambda* complex*)]
              [(and (not (memq uvar expr*)) (eq? (car expr) 'lambda))
               (values simple* `((,uvar ,expr) . ,lambda*) complex*)]
              [else (values simple* lambda* `((,uvar ,expr) . ,complex*))]
              )))))

  (define (split bind*)
    (match bind*
      [((,x* ,e*) ...) (values x* e*)]
      ))

  (define (Expr expr)
    (match expr
      [,uvar (guard (uvar? uvar)) uvar]
      [(quote ,immediate) (guard (immediate? immediate))
       `(quote ,immediate)]
      [(begin ,[e*] ... ,[e])
       `(begin ,e* ... ,e)]
      [(if ,[t] ,[c] ,[a])
       `(if ,t ,c ,a)]

      [(letrec ([,uvar* ,[e*]] ...) (assigned ,assigned* ,[b]))
       (let*-values
           ([(simple* lambda* complex*)
             (partition `((,uvar* ,e*) ...) assigned* uvar*)]
            [(xc* ec*) (split complex*)])
         (if (null? complex*)
           `(let ,simple*
              (assigned ()
                (letrec ,lambda* ,b)))
           (let ([tmp* (map (lambda (x) (unique-name 'purify-letrec))
                            xc*)])
             `(let ,simple*
                (assigned ()
                  (let ((,xc* (void)) ...)
                    (assigned ,xc*
                      (letrec ,lambda*
                        (let ((,tmp* ,ec*) ...)
                          (assigned ()
                            ,(make-begin
                              `((set! ,xc* ,tmp*) ... ,b))))))))))))]
                    

      [(let ([,uvar* ,[e*]] ...) (assigned ,assigned* ,[b]))
       `(let ([,uvar* ,e*] ...) (assigned ,assigned* ,b))]
      [(lambda ,uvar* (assigned ,assigned* ,[b]))
       `(lambda ,uvar* (assigned ,assigned* ,b))]
      [(set! ,uvar ,[e]) `(set! ,uvar ,e)]
      [(,prim ,[e*] ...) (guard (prim? prim))
       `(,prim ,e* ...)]
      [(,[rator] ,[rand*] ...)
       `(,rator ,rand* ...)]
      [,else (invalid who 'expression else)]
      ))

  (Expr program)

)) ;; end library
