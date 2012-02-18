;; assign-registers.ss
;;
;; part of p423-sp12/srwaggon-p423 a4
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; 2012 / 2 / 18

#!chezscheme
(library (compiler assign-registers)
  (export assign-registers play-nice low-degree? graph-remove low-degree pick-reg)
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
  (map (lambda (y) (cons (car y) (remove x (cdr y)))) graph))


;; play-nice takes
;; a list of uvars
;; a list of registers
;; a list of uvar-conflict associations (conflict graph)
;; and attempts to safely associate each uvar with a register
;; for which it does not conflict.
(define (play-nice uvar* reg* conflict*)
  (if (null? uvar*) '()
      (let* ([uvar (low-degree uvar* reg* conflict*)] ;; pick low degree variableOA
             [flict (assq uvar conflict*)]
             [reg (pick-reg reg* flict)])
        (append `((,uvar ,reg)) (play-nice (remove uvar uvar*) (remove reg reg*) (graph-remove uvar conflict*))))))

;; pick-reg takes a list of registers and a list of registers
;; and returns the car of the set-difference.
(define (pick-reg reg* conflicts)
  (let ([leftovers (difference reg* conflicts)])
    (if (null? leftovers)
        (errorf 'assign-registers "out of registers")
        (car leftovers))))


;; Unless uvar* is empty,
;; pick-low-degree will iterate across each uvar in uvar*
;; determining which is low-degree until it finds a low
;; degree variable (of which it will return).
;; if no low-degree variable is found, it will return the
;; last variable seen.
(define (low-degree uvar* reg* conflict*)
  (cond
    [(null? uvar*) #f]
    [else (let loop ([mem (car uvar*)][rem (cdr uvar*)])
            (cond
              [(null? rem) mem]
              [(low-degree? mem reg* conflict*) mem]
              [else (loop (car rem) (cdr rem))]))]))

(define (low-degree? mem reg* conflict*)
  (let ([conflicts (assq mem conflict*)]) ;; either `(,x ...) or #f
    (if conflicts ;; aka, not false, as false implies no conflicts
        (< (length (cdr conflicts)) (length reg*))
        #t)))

(define-who (assign-registers program)
  
  (define (Body body) ;; Body --> Body
    (match body
      [(locals ,uvar* (register-conflict ,graph ,tail))
      `(locate ,(play-nice uvar* registers graph) ,tail)]
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