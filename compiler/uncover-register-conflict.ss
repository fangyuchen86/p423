;; uncover-register-conflict.ss
;;
;; part of p423-sp12/srwaggon-p423 a4
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; 2012 / 2 / 15

#!chezscheme
(library (compiler uncover-register-conflict)
  (export uncover-register-conflict)
  (import
   ;; Load Chez Scheme primitives:
   (chezscheme)
   ;; Load compiler framework:
   (framework match)
   (framework helpers)
   (compiler helpers))

#|
uncover-register-conflict takes the blankets away from
register conflicts so that they freeze to death and die.
|# 
(define-who (uncover-register-conflict program)
  
  (define (graph-add! s conflicts graph)
    (when (uvar? s)
      (let ([assoc (assq s graph)])
        (if assoc
            (set-cdr! assoc (union conflicts (cdr assoc)))
            (cons `(,s ,conflicts) graph))))
    graph)
                  
          

  ;; handle-var : symbol live-set --> live-set
  ;; iff the var is a uvar or a register it is added
  ;; to the live-set before the live-set is returned.
  (define (handle-var var ls)
    (if (or (uvar? var) (register? var)) (set-cons var ls) ls))

  (define (Effect effect* effect graph live)
    (match effect
      [(nop) (Effect* effect* graph live)]
      [(set! ,lhs (,binop ,rhs0 ,rhs1))
       (let ([ls (remove lhs (handle-var rhs0 (handle-var rhs1 live)))])
         (Effect* effect* (graph-add! lhs ls graph) ls))]
      [(set! ,lhs ,rhs)
       (let ([ls (remove lhs (handle-var rhs live))])
         (Effect* effect* (graph-add! lhs ls graph) ls))]
      [(if ,pred ,conseq ,altern)
       (let*-values ([(gp lsp) (Pred pred graph live)]
                     [(gc lsc) (Effect '() conseq graph live)]
                     [(ga lsa) (Effect '() altern graph live)]
                     [(ge lse) (Effect* effect* graph live)])
         (values graph (union lsp lsc lsa lse)))]
      [(begin ,e* ...) (Effect* (append e* effect*) graph live)]
      [,else (invalid who 'Effect else)]))

  ;; Effect* : Effect* conflict-graph live-set --> conflict-graph live-set
  (define (Effect* effect* graph live)
    (match effect*
      [() (values graph live)]
      [(,effect* ... ,effect) (Effect effect* effect graph live)]
      [,else (invalid who 'Effect* else)]))

  ;; Pred : Pred conflict-graph live-set --> conflict-graph live-set
  (define (Pred pred graph live)
    (match pred
      [(true) (values graph live)]
      [(false) (values graph live)]
      [(begin ,effect* ... ,pred)
       (let*-values ([(gp lsp) (Pred pred graph live)]
                     [(ge* lse*) (Effect* effect* gp lsp)])
         (values graph (union lsp lse*)))]
      [(if ,pred ,conseq ,altern)
       (let*-values ([(gp lsp) (Pred pred graph live)]
                     [(gc lsc) (Pred conseq graph live)]
                     [(ga lsa) (Pred altern graph live)])
         (values graph (union lsp lsc lsa)))]
      [(,relop ,triv0 ,triv1)
       (values graph (handle-var triv0 (handle-var triv1 live)))]
      [,else (invalid who 'Pred else)]))

  ;; Tail : Tail conflict-graph live-set --> conflict-graph live-set
  (define (Tail tail graph live) 
    (match tail
      [(begin ,effect* ... ,tail)
       (let*-values ([(gt lst) (Tail tail graph live)]
                     [(ge* lse*) (Effect* effect* gt lst)])
         (values graph (union lst lse*)))]
      [(if ,pred ,conseq ,altern)
       (let*-values ([(gp lsp) (Pred pred graph live)]
                     [(gc lsc) (Tail conseq graph live)]
                     [(ga lsa) (Tail altern graph live)])
                     (values graph (union lsp lsc lsa)))]
      [(,triv ,loc* ...) (values graph (union loc* live))]
      [,else (invalid who 'Tail else)])) ;; Tail

  (define (Body body) ;; Body --> Body
    (match body
      [(locals ,uvar* ,tail)
       (let*-values ([(empty-graph) (map (lambda (s) (cons s '())) uvar*)]
                     [(live-set) '(rax rbp)]
                     [(graph lives) (Tail tail empty-graph live-set)])
         `(locals ,uvar* (conflict-graph ,graph ,tail)))]
      [,else (invalid who 'Body else)])) ;; Body

  (define (Block block) ;; Block --> Block
    (match block
      [(,label (lambda () ,[Body -> body])) `(,label (lambda () ,body))]
      [,else (invalid who 'Block else)])) ;; Block

  (define (Program program) ;; Program --> Program
    (match program
      [(letrec (,[Block -> block*] ...) ,[Body -> body])
       `(letrec ,block* ,body)]
      [,else (invalid who 'Program else)])) ;; Program

  (Program program)) ;; uncover-register-conflict
  
) ;; End Library