(uncover-register-conflict
 '(letrec ()
                   (locals
                     (m.2 t.10015 t.10014)
                     (ulocals
                       (u.10035 u.10034)
                       (locate
                         ((n.1 fv1) (rp.10021 fv0))
                         (frame-conflict
                          ((rp.10021 t.10015 t.10014 m.2 n.1)
                           (n.1 t.10015 t.10014 m.2 rp.10021)
                           (m.2 rp.10021 n.1)
                           (t.10015 rp.10021 n.1)
                           (t.10014 rp.10021 n.1))
                          (begin
                            (set! fv0 r15)
                            (set! fv1 r8)
                            (set! m.2 r9)
                            (if (= m.2 1)
                                (begin (set! rax fv1) (fv0 rbp rax))
                                (begin
                                  (set! u.10035 m.2)
                                  (set! u.10035 (- u.10035 1))
                                  (set! t.10014 u.10035)
                                  (set! rbp (+ rbp 16))
                                  (return-point
                                   rp$10022
                                   (begin
                                     (set! r9 t.10014)
                                     (set! r8 fv1)
                                     (set! r15 rp$10022)
                                     (expt$0 r15 rbp r8 r9)))
                                  (set! rbp (- rbp 16))
                                   (set! t.10015 rax)
                                   (set! u.10034 fv1)
                                   (set! u.10034 (* u.10034 t.10015))
                                   (set! rax u.10034)
                                   (fv0 rbp rax)))))))))
)