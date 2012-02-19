;; assign-registers.ss
;;
;; part of p423-sp12/srwaggon-p423 a4
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in a4
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; 2012 / 2 / 19

#!chezscheme
(library (compiler assign-registers)
  (export assign-registers play-nice graph-remove low-degree pick-reg)
  (import
   ;; Load Chez Scheme primitives:
   (chezscheme)
   ;; Load compiler framework:
   (framework match)
   (framework helpers)
   (compiler helpers))

;; graph-remove takes a symbol and an association list
;; and removes the given symbol from the value for each
;; key in the list.
(define (graph-remove x graph)
  (let ([graph (map (lambda (y) (cons (car y) (remove x (cdr y)))) graph)])
    (remove (assq x graph) graph)))

;; Unless uvar* is empty,
;; pick-low-degree will iterate across each uvar in uvar*
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

;; play-nice takes
;; a list of uvars
;; a list of registers
;; a list of uvar-conflict associations (conflict graph)
;; and attempts to safely associate each uvar with a register
;; for which it does not conflict.
(define (play-nice uvar* conflict*)
  (if (null? uvar*) '()
      (let* ([uvar (low-degree (car uvar*) (cdr uvar*) conflict*)] ;; pick low degree variable
             [flict (assq uvar conflict*)]
             [alist (play-nice (remove uvar uvar*) (graph-remove uvar conflict*))])
        `((,uvar ,(pick-reg uvar flict alist)) . ,alist))))

;; pick-reg takes a list of registers and a list of registers
;; and returns the car of the set-difference.
(define (pick-reg uvar conflicts alist)
  (let* ([conflicts (map (lambda (x) (if (assq x alist) (cadr (assq x alist)) x)) conflicts)] 
         [used (map (lambda (x) (cadr x)) alist)]
         [avail (difference registers conflicts)])
    (if (null? avail)
        (errorf 'assign-registers "out of registers (~s) for ~s but built ~s" avail uvar alist)
        (car avail))))

(define (taken-registers conflicts alist taken)
  (cond
    [(null? conflicts)
     taken]
    [(register? (car conflicts))
     (taken-registers
      (cdr conflicts) alist (cons (car conflicts) taken))]
    [(assq (car conflicts) alist)
     (taken-registers
      (cdr conflicts) alist (cons (cadr (assq (car conflicts) alist)) taken))]
    [else (taken-registers (cdr conflicts) alist taken)]))

(define-who (assign-registers program)
  
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