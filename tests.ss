(library (tests)
  (export
   tests)
  (import
   (chezscheme))

  (define tests '((letrec () (locals (curr.11 val.10)
                          (if (if (> curr.11 0)
                                  (true)
                                  (> (mref curr.11 0) val.10))
                              1
                              0)))))
)