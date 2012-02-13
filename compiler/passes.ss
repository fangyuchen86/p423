;; passes.ss
;;
;; part of p423-sp12/srwaggon-p423 assign3
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; 2012/2/11

#!chezscheme
(library (compiler passes)
  (export
    verify-scheme
    finalize-locations
    expose-frame-var
    expose-basic-blocks
    flatten-program
    generate-x86-64
  )
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (framework match)
    (framework helpers)
    ;; load compiler passes
    (compiler verify-scheme)
    (compiler finalize-locations)
    (compiler expose-frame-var)
    (compiler expose-basic-blocks)
    (compiler flatten-program)
    (compiler generate-x86-64)
  )
) ;; End Library