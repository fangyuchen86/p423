;; assign-registers.ss
;;
;; part of p423-sp12/srwaggon-p423 a4
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in a4
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; 2012 / 2 / 20

#!chezscheme
(library (compiler assign-registers)
  (export assign-registers)
  (import
   ;; Load Chez Scheme primitives:
   (chezscheme)
   ;; Load compiler framework:
   (framework match)
   (framework helpers)
   (compiler helpers))



(define-who (assign-registers program)


  ;; graph-remove : symbol conflict-graph --> conflict-graph
  ;; graph-remove takes a symbol and an association list
  ;; and removes the given symbol from the value for each
  ;; key in the list.
  (define (graph-remove x graph)
    (let ([graph (map (lambda (y) (cons (car y) (remove x (cdr y)))) graph)])
      (remove (assq x graph) graph)))
  
  
  ;; low-degree : uvar uvar* --> conflict-graph
  ;; low-degree iterates across each uvar in uvar*
  ;; determining which is low-degree until it finds a low
  ;; degree variable (of which it will return).
  ;; if no low-degree variable is found, it will return the
  ;; last variable seen.
  (define (low-degree cur uvar* conflict*)
    (cond
      [(null? uvar*) cur]
      [(< (length registers) (length (assq cur conflict*)))
       (low-degree (car uvar*) (cdr uvar*) conflict*)]
      [else (low-degree cur (cdr uvar*) conflict*)]))
  
  
  ;; play-nice : uvar* conflict-graph --> `((uvar register) ...)
  ;; a list of uvars
  ;; a list of uvar-conflict associations (conflict graph)
  ;; and attempts to safely associate each uvar with a register
  ;; for which it does not conflict.
  (define (play-nice uvar* conflict*)
    (if (null? uvar*) '()
      (let* ([uvar (low-degree (car uvar*) (cdr uvar*) conflict*)] ;; pick low degree variable
             [flict (assq uvar conflict*)]
             [alist (play-nice (remove uvar uvar*) (graph-remove uvar conflict*))])
        `((,uvar ,(pick-reg uvar flict alist)) . ,alist))))
  
  
  ;; pick-reg : uvar conflict-list ((uvar reg) ...) --> reg
  ;; pick-reg takes a uvar and a list of conflicting variables and registers
  ;; and picks a non-conflicting register to associate with
  ;; the provided uvar.
  (define (pick-reg uvar conflicts alist)
    (let* ([conflicts (map (lambda (x) (if (assq x alist) (cadr (assq x alist)) x)) conflicts)]
           [avail (difference registers conflicts)])
      (if (null? avail)
          (errorf 'assign-registers "out of registers (~s) for ~s but built ~s" avail uvar alist)
          (car avail))))
  
  
  (define (Body body) ;; Body --> Body
    (match body
      [(locals ,uvar* (register-conflict ,graph ,tail))
       `(locate ,(play-nice uvar* graph) ,tail)]
      [,else (invalid who 'Body else)]))

  (define (Block block) ;; Block --> Block
    (match block
      [(,label (lambda () ,[Body -> body])) `(,label (lambda () ,body))]
      [,else (invalid who 'Block else)]))
  
  (define (Program program) ;; Program --> Program
    (match program
      [(letrec (,[Block -> block*] ...) ,[Body -> body])
       `(letrec ,block* ,body)]
      [,else (invalid who 'Program else)]))
  
  (Program program))

) ;; End Library