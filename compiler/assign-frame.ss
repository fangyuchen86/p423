;; assign-frame.ss
;;
;; part of p423-sp12/srwaggon-p423 A5
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in A5
;; 2012/ 2/ 28
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; 2012/ 2/ 28

#!chezscheme
(library (compiler assign-frame)
  (export assign-frame)
  (import
   ;; Load Chez Scheme primitives:
   (chezscheme)
   ;; Load compiler framework:
   (framework match)
   (framework helpers)
   (compiler helpers))


(define-who (assign-frame program)

  #| graph-remove
  || : symbol conflict-graph
  || --> conflict-graph
  || 
  || Removes the given symbol from the value for each
  || key in the list.
  |#
  (define (graph-remove x graph)
    (let ([graph (map (lambda (y) (cons (car y) (remove x (cdr y)))) graph)])
      (remove (assq x graph) graph)))  
  
  #| play-nice
  || : uvar* conflict-graph spills-list
  || --> `((uvar register) ...) spills-list
  ||
  || Finds and binds each uvar to a
  || non-conflicting frame-var.
  |#
  (define (play-nice uvar* cgraph home*)
    (if (null? uvar*) home*
        (let* ([uvar (car uvar*)]
               [conflict* (assq uvar cgraph)]
               [used (if conflict*
                         (let find-used ([conflict* (cdr conflict*)])
                           (cond
                             [(null? conflict*) '()]
                             [(frame-var? (car conflict*))
                              (set-cons (car conflict*) (find-used (cdr conflict*)))]
                             [(assq (car conflict*) home*) =>
                              (lambda (x) (set-cons (cadr x) (find-used (cdr conflict*))))]
                             [else (find-used (cdr conflict*))]))
                         '())]
               [home (let find-home ([index 0])
                       (let ([fv (index->frame-var index)])
                         (if (memq fv used) (find-home (add1 index)) fv)))])
          (play-nice (remove uvar uvar*) (graph-remove uvar cgraph) `((,uvar ,home) . ,home*))
  )))

  #| Body
  || : Body
  || --> Body
  ||
  || Body handles the Body part of our grammar.
  || If a variable is spilled (has no available registers)
  || then Body returns an incomplete Body requiring
  || the substitution of uvars for shorter-living spill-vars.
  || Otherwise, it returns a complete Body.
  |#
  (define (Body body)
    (match body
      [(locals (,local* ...)
         (ulocals (,ulocal* ...)
           (spills (,spill* ...)
             (locate (,home* ...)
               (frame-conflict ,fgraph ,tail)))))
       
       (let ([bind* (play-nice spill* fgraph home*)])
         `(locals ,(difference local* spill*)
            (ulocals ,(difference ulocal* spill*)
              (locate (,bind* ...)
                (frame-conflict ,fgraph ,tail)))))]
      [(locate (,home* ...) ,tail)  `(locate (,home* ...) ,tail)]
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