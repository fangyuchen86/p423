;; impose-calling-conventions.ss
;;
;; part of p423-sp12/srwaggon-p423 A6
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in A6
;; 2012 / 3 / 26
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; 2012 / 3 / 26

#!chezscheme
(library (compiler impose-calling-conventions)
  (export 
   impose-calling-conventions
   )
  (import
   ;; Load Chez Scheme primitives:
   (chezscheme)
   ;; Load compiler framework:
   (framework match)
   (framework helpers)
   (compiler helpers)
  )

) ;; end library
