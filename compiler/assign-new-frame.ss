;; assign-new-frame.ss
;;
;; part of p423-sp12/srwaggon-p423 A7
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in A7
;; 2012 / 3 / 19
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; 2012 / 3 / 19

#!chezscheme
(library (compiler assign-new-frame)
  (export 
   assign-new-frame
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