;; passes.ss
;;
;; part of p423-sp12/srwaggon-p423 a4
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in a1
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; 2012/2/15

#!chezscheme
(library (compiler passes)
  (export
    verify-scheme
    uncover-frame-conflict
    uncover-register-conflict
    introduce-allocation-forms
    select-instructions
    assign-registers
    assign-frame
    everybody-home?
    finalize-frame-locations
    discard-call-live
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
    (compiler uncover-frame-conflict)
    (compiler introduce-allocation-forms)
    (compiler select-instructions)
    (compiler uncover-register-conflict)
    (compiler assign-registers)
    (compiler assign-frame)
    (compiler everybody-home)
    (compiler finalize-frame-locations)
    (compiler discard-call-live)
    (compiler finalize-locations)
    (compiler expose-frame-var)
    (compiler expose-basic-blocks)
    (compiler flatten-program)
    (compiler generate-x86-64)
  )
) ;; End Library