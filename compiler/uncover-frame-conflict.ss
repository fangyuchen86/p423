;; uncover-frame-conflict.ss
;;
;; part of p423-sp12/srwaggon-p423
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced A5
;; 2012 / 2 / ??
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; revised in A7
;; 2012 / 4 / 4

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
  )

(define-who (uncover-frame-conflict program)

  (define (Body body) ;; Body --> Body
    (match body
      [(locals ,uvar* (new-frames ,frame*,tail))
       (let*-values ([(graph call-live) (uncover-conflicts tail uvar* who frame-var?)]
                     [(spills) (filter uvar? call-live)])
         `(locals ,uvar*
            (new-frames ,frame*
               (spills ,spills
                 (frame-conflict ,graph
                   (call-live ,call-live ,tail))))))]
      [,else (invalid who 'Body else)]))
  
  (define (Program program) ;; Program --> Program
    (match program
      [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
      [,else (invalid who 'Program else)]))
  
  (Program program)

)) ;; end library