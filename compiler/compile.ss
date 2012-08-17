;; compile.ss
;;
;; part of p423-sp12/srwaggon-p423 a4
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; 2012/ 5 / 4

(library (compiler compile)
  (export p423-compile p423-step
          ;; My passes:
          verify-scheme              ;; a9
          optimize-direct-call       ;; a13
          remove-anonymous-lambda    ;; a13
          sanitize-binding-forms     ;; a13
          uncover-free               ;; a12
          convert-closures           ;; a12
          optimize-known-call        ;; a13
          introduce-procedure-primitives ;; a12
          lift-letrec                ;; a11
          normalize-context          ;; a11
          specify-representation     ;; a10
          uncover-locals             ;; a9
          remove-let                 ;; a9
          verify-uil                 ;; a1
          remove-complex-opera*      ;; a6
          flatten-set!               ;; a6
          impose-calling-conventions ;; a6
          expose-allocation-pointer  ;; a8
          uncover-frame-conflict     ;; a5
          pre-assign-frame           ;; a7
          select-instructions        ;; a5
          uncover-register-conflict  ;; a4
          assign-registers           ;; a4
          assign-frame               ;; a5
          finalize-frame-locations   ;; a5
          discard-call-live          ;; a4
          finalize-locations         ;; a5
          expose-frame-var           ;; a4
          expose-memory-operands     ;; a8
          expose-basic-blocks        ;; a3
          optimize-jumps             ;; a11
          flatten-program            ;; a2
          generate-x86-64            ;; a1
          )
  (import 
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load provided compiler framework:
    (framework driver)
    (framework wrappers)
    (framework match)
    (framework helpers)
    ;; My passes:
    (compiler verify-scheme)              ;; a9
    (compiler optimize-direct-call)       ;; a13
    (compiler remove-anonymous-lambda)    ;; a13
    (compiler sanitize-binding-forms)     ;; a13
    (compiler uncover-free)               ;; a12
    (compiler convert-closures)           ;; a12
    (compiler optimize-known-call)        ;; a13
    (compiler introduce-procedure-primitives) ;; a12
    (compiler lift-letrec)                ;; a11
    (compiler normalize-context)          ;; a11
    (compiler specify-representation)     ;; a10
    (compiler uncover-locals)             ;; a9
    (compiler remove-let)                 ;; a9
    (compiler verify-uil)                 ;; a1
    (compiler remove-complex-opera*)      ;; a6
    (compiler flatten-set!)               ;; a6
    (compiler impose-calling-conventions) ;; a6
    (compiler expose-allocation-pointer)  ;; a8
    (compiler uncover-frame-conflict)     ;; a5
    (compiler pre-assign-frame)           ;; a7
    (compiler assign-new-frame)           ;; a7
    (compiler select-instructions)        ;; a5
    (compiler uncover-register-conflict)  ;; a4
    (compiler assign-registers)           ;; a4
    (compiler assign-frame)               ;; a5
    (compiler everybody-home)             ;; a4
    (compiler finalize-frame-locations)   ;; a5
    (compiler discard-call-live)          ;; a4
    (compiler finalize-locations)         ;; a5
    (compiler expose-frame-var)           ;; a4
    (compiler expose-memory-operands)     ;; a8
    (compiler expose-basic-blocks)        ;; a3
    (compiler optimize-jumps)             ;; a11
    (compiler flatten-program)            ;; a2
    (compiler generate-x86-64)            ;; a1
    )
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
(define-compiler (p423-compile p423-step pass->wrapper)
  (verify-scheme)
  (optimize-direct-call)
  (remove-anonymous-lambda)
  (sanitize-binding-forms)
  (uncover-free)
  (convert-closures)
  (optimize-known-call)
  (introduce-procedure-primitives)
  (lift-letrec)
  (normalize-context)
  (specify-representation)
  (uncover-locals)
  (remove-let)
  (verify-uil)
  (remove-complex-opera*)
  (flatten-set!)
  (impose-calling-conventions)
  (expose-allocation-pointer)
  (uncover-frame-conflict)
  (pre-assign-frame)
  (assign-new-frame)
  (iterate
   (finalize-frame-locations)
   (select-instructions)
   (uncover-register-conflict)
   (assign-registers)
   (break/when everybody-home?)
   (assign-frame)
   )
   (discard-call-live)
   (finalize-locations)
   (expose-frame-var)
   (expose-memory-operands)
   (expose-basic-blocks)
   (optimize-jumps)
   (flatten-program)
   (generate-x86-64 assemble)
  )

;; See the drivers.ss file for other options when defining a compiler

) ;; End library

