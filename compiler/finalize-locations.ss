;; finalize-locations.ss
;;
;; part of p423-sp12/srwaggon-p423 assign3
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in a2
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; 2012/2/11

#!chezscheme
(library (compiler finalize-locations)
  (export finalize-locations)
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (framework match)
    (framework helpers)
  )

#| finalize-locations : exp --> exp
 | finalize-locations takes a valid Scheme expression
 | and replaces each occurrence of a uvar in the body
 | of each locate form with the corresponding Loc.
 | As it does so, it removes useless assignments, i.e.,
 | converts any assignments (set! ,x ,y) to (nop)
 | if x and y resolve to the same location.  It
 | also discareds the locate form.
 |#
(define (finalize-locations program) 
  (define (finalize-loc env)
    (lambda (exp)
      (match exp
        [,uvar (guard (uvar? uvar)) (cadr (assq  uvar env))]
        [(set! ,[loc] (,binop ,[triv0] ,[triv1])) `(set! ,loc (,binop ,triv0 ,triv1))]
        [(set! ,[loc] ,[triv]) (if (eq? loc triv) `(nop) `(set! ,loc ,triv))]
        [(if ,[pred] ,[conseq] ,[altern]) `(if ,pred ,conseq ,altern)]
        [(begin ,[effect*] ... ,[effect]) `(begin ,effect* ... ,effect)]
        [(,relop ,[triv0] ,[triv1]) `(,relop ,triv0 ,triv1)]
        [(,[tail]) `(,tail)] 
        [,x x]
      )
    )
  )

  (define (Body body)
    (match body
      [(locate ,env ,tail)
       ((finalize-loc env) tail)
      ]
    )
  )

  (match program
    [ (letrec ([,lbl (lambda () ,[Body -> bn])] ...) ,[Body -> b0])
     `(letrec ([,lbl (lambda () ,bn)] ...) ,b0)
    ]
  )
)

)
;; End Library.  Please flip to side B.