;; assign-registers.ss
;;
;; part of p423-sp12/srwaggon-p423 A5
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in A4
;; 2012/ 2/ 20
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; 2012/ 2/ 27

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

  #| graph-remove : symbol conflict-graph --> conflict-graph
  || graph-remove takes a symbol and an association list
  || and removes the given symbol from the value for each
  ||  key in the list.
  |#
  (define (graph-remove x graph)
    (let ([graph (map (lambda (y) (cons (car y) (remove x (cdr y)))) graph)])
      (remove (assq x graph) graph)))
  
  
  #| low-degree : uvar uvar* --> conflict-graph
  || low-degree iterates across each uvar in uvar*
  || determining which is low-degree until it finds a low
  || degree variable (of which it will return).
  || if no low-degree variable is found, it will return the
  || last variable seen.
  |#
  (define (low-degree cur uvar* conflict*)
    (cond
      [(null? uvar*) cur]
      [(< (length registers) (length (assq cur conflict*)))
       (low-degree (car uvar*) (cdr uvar*) conflict*)]
      [else (low-degree cur (cdr uvar*) conflict*)]
  ))
  
  
  #| play-nice : uvar* conflict-graph spills-list --> `((uvar register) ...) spills-list
  || play-nice takes
  || a list of uvars
  || a list of uvar-conflict associations (conflict graph)
  || a list of spilt uvars
  || and attempts to safely associate each uvar with a register
  || for which it does not conflict.
  |#
  (define (play-nice uvar* conflict* spills)
    (if (null? uvar*) (values '() spills)
        (let*-values
            ([(uvar) (low-degree (car uvar*) (cdr uvar*) conflict*)] ;; pick low degree variable
             [(alist spills) (play-nice (remove uvar uvar*) (graph-remove uvar conflict*) spills)]
             [(conflicts)
              (map (lambda (x)
                     (if (assq x alist) (cadr (assq x alist)) x))
                   (assq uvar conflict*))]
             [(avail) (difference registers conflicts)])
          (if (null? avail) (values alist (set-cons uvar spills))
              (values `((,uvar ,(car avail)) . ,alist) spills)))))
  

  #| Body : Body --> (complete / incomplete) Body
  || Body handles the Body part of our grammar.
  || If a variable is spilled (has no available registers)
  || then Body returns an incomplete Body requiring
  || the substitution of uvars for shorter-living spill-vars.
  || Otherwise, it returns a complete Body.
  |#
  (define (Body body)
    (match body
      [(locals (,locals ...)
         (ulocals (,ulocals ...)
           (locate ,locate
             (frame-conflict ,fgraph
               (register-conflict ,rgraph ,tail))))) ;; <--- LHS || RHS ---V
       (let-values ([(bind* spills) (play-nice (union locals ulocals) rgraph '())])
         (if (null? spills) `(locate ,bind* ,tail)
             `(locals ,(difference locals spills)
                (ulocals ,(difference ulocals spills)
                  (spills ,spills
                    (locate ,locate
                      (frame-conflict ,fgraph ,tail)))))))]
      [(locate (,home* ...) ,tail) `(locate (,home* ...) ,tail)]
      [,else (invalid who 'Body else)]
  ))
  
  (define (Program program) ;; Program --> Program
    (match program
      [(letrec ([,label* (lambda () ,[Body -> body*])] ...) ,[Body -> body])
       `(letrec ([,label* (lambda () ,body*)] ...) ,body)]
      [,else (invalid who 'Program else)]
  ))
  
  (Program program))

) ;; End Library