(library (p423 compiler compile)
  (export p423-compile)
  (import
    (chezscheme)
    (p423 driver)
    (p423 compiler passes)
    (p423 compiler wrappers)
    (p423 compiler match)
    (p423 compiler helpers))

(define-compiler (p423-compile p423-compile-passes source/wrapper)
  (verify-scheme verify-scheme/wrapper))

)
