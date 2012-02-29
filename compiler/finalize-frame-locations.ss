;; finalize-frame-locations.ss
;;
;; part of p423-sp12/srwaggon-p423 A5
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in A5
;; 2012 / 2 / 28
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; 2012 / 2 / 28

#!chezscheme
(library (compiler finalize-frame-locations)
  (export 
   finalize-frame-locations
   )
  (import
   ;; Load Chez Scheme primitives:
   (chezscheme)
   ;; Load compiler framework:
   (framework match)
   (framework helpers)
   (compiler helpers)
  )


  #|
  || Sure, a tree walk, why not?
  || I mean, people used to entertain themselves
  || by rolling a hoop down a hill with a stick.
  ||
  |#
  (define-who (finalize-frame-locations program)
    

    (define (Body body)
      (match body
        [(locals (,local* ...)
           (ulocals (,ulocal* ...)
             (locate (,home* ...)
               (frame-conflict ,fgraph ,[Tail -> tail]))))
        `(locals (,local* ...)
           (ulocals (,ulocal* ...)
             (locate (,home* ...)
               (frame-conflict ,fgraph ,tail))))]
        [(locate (,home* ...) ,[Tail -> tail])
        `(locate (,home* ...) ,tail)]
        [,else (invalid who ,Body else)]
    ))
    
    (define (Program program)
      (match program
        [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
         `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
        [,else (invalid who 'Program else)]
    ))
    
    (Program program)

)) ;; end library
