;; compile.ss
;;
;; part of p423-sp12/srwaggon-p423 a4
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; 2012/2/15

(library (compiler compile)
  (export p423-compile p423-compile-passes)
  (import 
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load provided compiler framework:
    (framework driver)
    (framework wrappers)
    (framework match)
    (framework helpers)
    ;; My passes:
    (compiler passes))

;; Given a thunk that generates assembly code, this will compile the 
;; resulting assembly code and output it to a file named t.s
(define (assemble thunk)
  (with-output-to-file "t.s"
    thunk 
    'replace)
  (unless (zero? (system "cc -m64 -o t t.s framework/runtime.c"))
    (error 'assemble "assembly failed"))
  "./t")

;; Defines the compiler
(define-compiler (p423-compile p423-compile-passes pass->wrapper)
  (verify-scheme)
  (uncover-frame-conflict)
  (introduce-allocation-forms)
  (iterate
   (select-instructions)
   (uncover-register-conflict)
   (assign-registers)
   (break/when everybody-home?)
   (assign-frame)
   (finalize-frame-locations)
   )
  (discard-call-live)
  (finalize-locations)
  (expose-frame-var)
  (expose-basic-blocks)
  (flatten-program)
  (generate-x86-64 assemble)
  )

;; See the drivers.ss file for other options when defining a compiler

) ;; End library

