;; uncover-frame-conflict.ss
;;
;; part of p423-sp12/srwaggon-p423 a5
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced a5
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; 2012 / 2 / 21

#!chezscheme
(library (compiler uncover-frame-conflict)
  (export uncover-frame-conflict)
  (import
   ;; Load Chez Scheme primitives:
   (chezscheme)
   ;; Load compiler framework:
   (framework match)
   (framework helpers)
   (compiler helpers)
   (compiler uncover-register-conflict)
  )

(define-who (uncover-frame-conflict program)

  (define (Body body) ;; Body --> Body
    (match body
      [(locals ,uvar* ,tail)
       `(locals ,uvar* (frame-conflict ,(uncover-conflict tail uvar* who frame-var?) ,tail))]
      [,else (invalid who 'Body else)]))
  
  (define (Program program) ;; Program --> Program
    (match program
      [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
      [,else (invalid who 'Program else)]))
  
  (Program program)
)

) ;; end library