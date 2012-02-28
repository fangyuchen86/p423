;; flatten-program.ss
;;
;; part of p423-sp12/srwaggon-p423 assign3
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in a2
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; 2012/2/11

#!chezscheme
(library (compiler flatten-program)
  (export flatten-program)
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (framework match)
    (framework helpers)
  )

#| flatten-program : scheme-exp --> pseudo-assmebly-exp
 | flatten-program takes a scheme expression and 'flattens' it,
 | converting it into a pseudo-assembly code expression without nesting
 | with which it is easier to convert directly into x86-64 assembly.
 |#
(define-who (flatten-program program)
  (define (Program program)
    (define (Tail tail nxtLbl)
      
      (define (Effect effect)
        (match effect
          [(set! . ,x) `(set! . ,x)]          
          [,x (errorf who "invalid effect: ~s" x) `(ERROR ,x)]
        )
      )
      (match tail
        [(begin ,effect* ... ,tail) `(,(map Effect effect*) ... ,(Tail tail nxtLbl) ...)]
        [(if ,pred (,conseq) (,altern))
         (cond 
          [(eq? conseq nxtLbl) `((if (not ,pred) (jump ,altern)))]
          [(eq? altern nxtLbl) `((if ,pred (jump ,conseq)))]
          [else `((if ,pred (jump ,conseq)) (jump ,altern))]
         )
        ]
        [(,triv) (if (eq? triv nxtLbl) '() `((jump ,triv)))]
        [,x (errorf who "invalid tail: ~s" x) `(ERROR ,x)]
      )
    )
    (match program
      [(letrec ([,lbl* (lambda () ,tail*)] ...) ,tail)
       `(code 
         ,@(let loop ([tail tail] [lbl* lbl*] [tail* tail*])
             (if (null? lbl*)
                 (Tail tail #f)
                 `(,(Tail tail (car lbl*)) ...
                   ,(car lbl*)
                   ,(loop (car tail*) (cdr lbl*) (cdr tail*)) ...)
             )
           )
         )
      ]
      [,x (errorf who "invalid program: ~s" x) `(ERROR ,x)]
    )
  )
  (Program program)
)
;; thank you again, kent.

) ;; End Library.