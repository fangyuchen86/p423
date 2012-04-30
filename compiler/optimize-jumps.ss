;; optimize-jumps.ss
;;
;; part of p423-sp12/srwaggon-p423
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in A11
;; 2012 / 4 / 29
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; revised in A11
;; 2012 / 4 / 29

#!chezscheme
(library (compiler optimize-jumps)
  (export optimize-jumps)
  (import
   ;; Load Chez Scheme primitives:
   (chezscheme)
   ;; Load compiler framework:
   (framework match)
   (framework helpers)
   (compiler helpers)
   )

#|
||
|#
(define-who (optimize-jumps program)
  
  (define bindings '())
  (define (associate-jumps lbl jmp)
    (let loop ([end jmp][nxt (cons '0 jmp)])
        (if nxt (loop (cdr nxt) (assoc (cdr nxt) bindings))
            (set! bindings (cons (cons lbl end) bindings)))))

  (define (resolve-jump lbl)
    (let loop ([last lbl][site (assq lbl bindings)])
      (if site (loop (cdr site) (assoc (cdr site) bindings))
          last)))
  
  #|
  ||
  |#
  (define (Effect effect)
    (match effect
      [(nop) '(nop)]
      [(begin ,[Effect -> ef*] ... ,[ef]) (make-begin `(,ef* ... ,ef))]
      [(if ,[Pred -> pr] (,cjmp) (,ajmp)) `(if ,pr (,(resolve-jump cjmp)) (,(resolve-jump ajmp)))]
      [(set! ,uv ,lbl) (guard (label? lbl)) `(set! ,uv ,(resolve-jump lbl))]
      [(set! ,uv . ,x) `(set! ,uv . ,x)]
      [,else (invalid who 'Effect else)]
      ))

  #|
  ||
  |#
  (define (Pred pred)
    (match pred
      [(true) '(true)]
      [(false) '(false)]
      [(begin ,[Effect -> ef*] ... ,pjmp) (make-begin `(,ef* ... ,(resolve-jump pjmp)))]
      [(if ,[Pred -> pr] (,cjmp) (,ajmp)) `(if ,pr (,(resolve-jump cjmp)) (,(resolve-jump ajmp)))]
      [(,relop ,tr ,tr^) (guard (relop? relop)) `(,relop ,tr ,tr^)]
      [,else (invalid who 'Pred else)]
      ))

  #|
  ||
  |#
  (define (Tail tail)
    (match tail
      [(begin ,[Effect -> ef*] ... ,[tl]) (make-begin `(,ef* ... ,tl))]
      [(if ,[Pred -> pr] (,cjmp) (,ajmp)) `(if ,pr (,(resolve-jump cjmp)) (,(resolve-jump ajmp)))]
      [(,jmp) (guard (triv? jmp)) `(,(resolve-jump jmp))]
      ;;[,else (invalid who 'Tail else)]
      [,else else] ;; whatever.
      ))


  (define (Bind bind)
    (match bind
      [[,lbl (lambda () (,jmp))] (guard (label? jmp))
       (associate-jumps lbl jmp) bind]
      [[,lbl (lambda () ,tl)]
       `[,lbl (lambda () ,(Tail tl))]]
      [,else (invalid who 'Binding else)]
      ))

  #|
  ||
  |#
  (define (Program p)
    (match p
      [(letrec (,[Bind -> bn*] ...) ,tl)
       `(letrec (,bn* ...) ,(Tail tl))]
      [,else (invalid who 'Program else)]
      ))

  (Program program)

)) ;; end library
