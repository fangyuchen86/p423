(flatten-program
    (expose-basic-blocks
      '(letrec ([f$1 (lambda ()
                       (begin (nop) (if (if (= r8 1) (true) (> r9 1000))
                                        (begin (set! rax r9) (r15))
                                        (begin 
                                          (set! r9 (* r9 2))
                                          (set! rax r8)
                                          (set! rax (logand rax 1))
                                          (if (= rax 0) (set! r9 (+ r9 1)) (nop))
                                          (set! r8 (sra r8 1))
                                          (f$1)))))])
         (begin (set! r8 3) (set! r9 10) (f$1)))))