;; expose-frame-var.ss
;;
;; part of p423-sp12/srwaggon-p423 assign3
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in a2
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; 2012/2/11

#!chezscheme
(library (compiler expose-frame-var)
  (export expose-frame-var)
  (import
    ;; Load Chez Scheme primitives:
    (chezscheme)
    ;; Load compiler framework:
    (framework match)
    (framework helpers)
  )

#| expose-frame-var : exp --> exp
 | expose-frame-var takes an expression and tree-walks it
 | converting any occurrences of frame variables into 
 | displacement mode operands,
 | with rbp as the base register and an offset based on 
 | the frame variable's index.  Since our words are 8-bits
 | in length, the following pattern emerges:
 |
 | fv0 --> #<disp rbp 0>
 | fv1 --> #<disp rbp 8>
 | fv[i] --> #<disp rbp 8i>
 |#
(define (expose-frame-var sexp)
  (match sexp
    [,x (guard (frame-var? x)) (make-disp-opnd 'rbp (* 8 (frame-var->index x)))]
    [(,x . ,y) `(,(expose-frame-var x) . ,(expose-frame-var y))]
    [,else else]
  )
)

)
;; End Library.  Please flip to side B.