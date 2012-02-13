;; expose-basic-blocks.ss
;;
;; part of p423-sp12/srwaggon-p423 assign3
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; 2012/2/11

#!chezscheme
(library (compiler expose-basic-blocks)
  (export expose-basic-blocks)
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (framework match)
    (framework helpers)
    (compiler helpers)
  )

#| expose-basic-blocks : exp --> pain and suffering
 | expose-basic-blocks takes an expression and a programmer's
 | soul and converts the programmer's free time into work
 | as well as life into one of unhappiness.
 |#
(define-who (expose-basic-blocks program)

  (define (expose-tail tail)
    (match tail
      [(if ,pred ,tail0 ,tail1)
       (let ([clbl0 (unique-label 'c)]
             [albl0 (unique-label 'a)])
         (let-values ([(pexpr pbinds) (expose-pred pred clbl0 albl0)]
                      [(cexpr cbinds) (expose-tail tail0)]
                      [(aexpr abinds) (expose-tail tail1)])
           (values pexpr `(,pbinds ...
                           [,clbl0 (lambda () ,cexpr)]
                           ,cbinds ...
                           [,albl0 (lambda () ,aexpr)]
                           ,abinds ...
                           ))))]
      [(begin ,effect* ... ,tail)
       (let*-values ([(texpr tbinds) (expose-tail tail)]
                     [(eexpr ebinds) (expose-effect* effect* `(,texpr))])
         (values eexpr `(,ebinds ... ,tbinds ...)))]
      [(,triv)
       (values `(,triv) '())]
      [,x (errorf who "unexpected tail: ~s" x) x]
    )
  )

  (define (expose-pred pred clbl albl)
    (match pred
      [(true)
       (values `(,clbl) '())]
      [(false)
       (values `(,albl) '())]
      [(,relop ,r0 ,r1) (guard (relop? relop))
       (values `(if (,relop ,r0 ,r1) (,clbl) (,albl)) '())]
      [(if ,pred ,conseq ,altern)
       (let ([clbl0 (unique-label 'c)]
             [albl0 (unique-label 'a)])
         (let-values ([(pexpr pbinds) (expose-pred pred clbl0 albl0)]
                      [(cexpr cbinds) (expose-pred conseq clbl albl)]
                      [(aexpr abinds) (expose-pred altern clbl albl)])
           (values pexpr
                   `(,pbinds ...
                    [,clbl0 (lambda () ,cexpr)]
                    ,cbinds ...
                    [,albl0 (lambda () ,aexpr)]
                    ,abinds ...
                    ))))]
      [(begin ,effect* ... ,pred)
       (let*-values ([(pexp pbinds) (expose-pred pred clbl albl)]
                     [(eexp ebinds) (expose-effect* effect* `(,pexp))])
         (values eexp `(,ebinds ... ,pbinds ...)))]
      [,x (errorf who "unexpected pred: ~s" x) x]
    )
  )

  (define (expose-effect* effect* acc)
    (match effect*
      [() (values (make-begin acc) '())]
      [(,e* ... ,e) (expose-effect e* e acc)]
      [,x (errorf who "unexpected effect*: ~s" x) x]
    )
  )

  (define (expose-effect effect* effect acc)
    (match effect
      [(nop) (expose-effect* effect* acc)]
      [(set! . ,x) (expose-effect* effect* `((set! . ,x) ,acc ...))]
      [(if ,pred ,conseq ,altern)
       (let ([clbl (unique-label 'c)]
             [albl (unique-label 'a)]
             [jlbl (unique-label 'j)])
         (let*-values ([(pexpr pbinds) (expose-pred pred clbl albl)]
                       [(cexpr cbinds) (expose-effect '() conseq `((,jlbl)))]
                       [(aexpr abinds) (expose-effect '() altern `((,jlbl)))]
                       [(eexpr ebinds) (expose-effect* effect* `(,pexpr))])
           (values eexpr
                   `(,ebinds ...
                     ,pbinds ...
                     [,clbl (lambda () ,cexpr)]
                     ,cbinds ...
                     [,albl (lambda () ,aexpr)]
                     ,abinds ...
                     [,jlbl (lambda () ,(make-begin acc))])
                   )))]
      [(begin ,e* ...) (expose-effect* (append effect* e*) acc)]
      [,x (errorf who "unexpected effect: ~s" x) x]
    )
  )

  (define (Program program)
    (match program
      [(letrec ([,label* (lambda () ,[expose-tail -> texp* tbinds*])] ...) ,[expose-tail -> texp tbinds])
       `(letrec ([,label* (lambda () ,texp*)] ... ,tbinds* ... ... ,tbinds ...) ,texp)]
      [,x (errorf who "unexpected program: ~s" x) x]
    )
  )
  (Program program)
)

)
;; End Library.  Please flip to side B.