(library (tests)
  (export
   tests)
  (import
   (chezscheme))

  (define tests '(
(letrec ([vector-scale!$0 (lambda (vect.1 scale.2)
                            (locals
                             (size.3)
                             (begin
                               (set! size.3 (mref vect.1 0))
                               (vector-scale!$1 size.3 vect.1 scale.2))))]
         [vector-scale!$1 (lambda (offset.4 vect.5 scale.6)
                            (locals
                             (t.10010 t.10009 t.10008 t.10007 t.10006)
                             (if (< offset.4 1)
                                 0
                                 (begin
                                   (set! t.10010 (* offset.4 8))
                                   (set! t.10007 (* offset.4 8))
                                   (set! t.10008 (mref vect.5 t.10007))
                                   (set! t.10009 (* t.10008 scale.6))
                                   (mset! vect.5 t.10010 t.10009)
                                   (set! t.10006 (- offset.4 1))
                                   (vector-scale!$1
                                    t.10006
                                    vect.5
                                    scale.6)))))]
         [vector-sum$2 (lambda (vect.7)
                         (locals
                          (t.10005)
                          (begin
                            (set! t.10005 (mref vect.7 0))
                            (vector-sum$3 t.10005 vect.7))))]
         [vector-sum$3 (lambda (offset.9 vect.10)
                         (locals
                          (t.10004 t.10003 t.10002 t.10001)
                          (if (< offset.9 1)
                              0
                              (begin
                                (set! t.10003 (* offset.9 8))
                                (set! t.10004 (mref vect.10 t.10003))
                                (set! t.10001 (- offset.9 1))
                                (set! t.10002
                                      (vector-sum$3 t.10001 vect.10))
                                (+ t.10004 t.10002)))))])
  (locals
   (vect.11)
   (begin
     (set! vect.11 (alloc 48))
     (mset! vect.11 0 5)
     (mset! vect.11 8 123)
     (mset! vect.11 16 10)
     (mset! vect.11 24 7)
     (mset! vect.11 32 12)
     (mset! vect.11 40 57)
     (vector-scale!$0 vect.11 10)
     (vector-sum$2 vect.11))))

                  #;(letrec ([make-vector$0 (lambda (size.1)
                  (locals
                  (v.2)
                  (begin
                  (set! v.2 (alloc (+ (* size.1 8) 8)))
                  (mset! 0 v.2 size.1)
                  v.2)))]
                  [chained-vector-set!$1 (lambda (v.3 off.4 val.5)
                  (locals
                  ()
                  (begin
                  (mset! (* (+ off.4 1) 8) v.3 val.5)
                  v.3)))]
                  [vector-length$4 (lambda (v.8) (locals () (mref v.8 0)))]
                  [find-greatest-less-than$2 (lambda (v.6 val.7)
                  (locals
                  ()
                  (fglt-help$3
                  v.6
                  val.7
                  (+ v.6 8)
                  (vector-length$4 v.6))))]
                  [fglt-help$3 (lambda (v.9 val.10 curr.11 size.12)
                  (locals
                  ()
                  (if (if (> curr.11 (+ (+ v.9 (* size.12 8)) 8))
                  (true)
                  (> (mref curr.11 0) val.10))
                  (mref curr.11 -8)
                  (fglt-help$3
                  v.9
                  val.10
                  (+ curr.11 8)
                  size.12))))])
                  (locals
                  (v.13)
                  (begin
                  (set! v.13
                  (chained-vector-set!$1
                  (chained-vector-set!$1
                  (chained-vector-set!$1
                  (chained-vector-set!$1
                  (chained-vector-set!$1
                  (chained-vector-set!$1
                  (chained-vector-set!$1
                  (chained-vector-set!$1
                  (chained-vector-set!$1
                  (chained-vector-set!$1 (make-vector$0 10) 0 0)
                  1
                  10)
                  2
                  20)
                  3
                  30)
                  4
                  40)
                  5
                  50)
                  6
                  60)
                  7
                  70)
                  8
                  80)
                  9
                  90))
                  (find-greatest-less-than$2 v.13 76)))))
    ))