;; uncover-register-conflict.ss
;;
;; part of p423-sp12/srwaggon-p423 a5
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; since a4
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; 2012 / 2 / 21

#!chezscheme
(library (compiler uncover-register-conflict)
  (export uncover-register-conflict)
  (import
   ;; Load Chez Scheme primitives:
   (chezscheme)
   ;; Load compiler framework:
   (framework match)
   (framework helpers)
   (compiler helpers)
  )

(define-who  (uncover-register-conflict program)

  (define (Body body) ;; Body --> Body
    (match body
      [(locals (,local* ...)
         (ulocals (,ulocal* ...)
           (locate (,home* ...)
             (frame-conflict ,frame-conflict-graph ,tail))))
       `(locals (,local* ...)
          (ulocals (,ulocal* ...)
            (locate (,home* ...)
              (frame-conflict ,frame-conflict-graph
                (register-conflict ,(uncover-conflicts tail (union local* ulocal*)  who register?) ,tail)))))]
      [(locate (,home* ...) ,tail) `(locate (,home* ...) ,tail)]
      [,else (invalid who 'Body else)]))
  
  (define (Program program) ;; Program --> Program
    (match program
      [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
    [,else (invalid who 'Program else)]))
  
  (Program program)
)

  
) ;; End Library