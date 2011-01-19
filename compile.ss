(library (p423 compiler compile)
  (export
    p423-compile
    p423-compile-passes)
  (import
    (chezscheme)
    (p423 driver)
    (p423 compiler passes)
    (p423 compiler wrappers)
    (p423 compiler match)
    (p423 compiler helpers))

(define (assemble thunk)
  (with-output-to-file "t.s"
    thunk 
    'replace)
  (unless (zero? (system "cc -m64 -o t t.s runtime.c"))
    (error 'assemble "assembly failed"))
  "./t")

(define-compiler (p423-compile p423-compile-passes source/wrapper)
  (verify-scheme verify-scheme/wrapper)
  (generate-x86-64 generate-x86-64/wrapper assemble))

)
