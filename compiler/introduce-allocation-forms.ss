;; introduce-allocation-forms.ss
;;
;; part of p423-sp12/srwaggon-p423 A5
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in A5
;; 2012 / 2 / 22
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; 2012 / 2 / 21

#!chezscheme
(library (compiler )
  (export 

  )
  (import
   ;; Load Chez Scheme primitives:
   (chezscheme)
   ;; Load compiler framework:
   (framework match)
   (framework helpers)
   (compiler helpers)
)

(define-who (introduce-allocation-forms program)
  
  (define (Body body)
    (match body
      [(locals ,uvar* (frame-conflict ,conflict* ,tail))
       `(locals ,uvar* (ulocals () (locate () (frame-conflict ,conflict ,tail))))]
      [,else (invalid who 'Body)]
  ))

  (define (Program program)
    (match program
      [(letrec ([,label* (lambda () ,[body*])] ...) ,[body])
       `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
      [,else (invalid who 'Program)]
  ))

  (Program program)
)