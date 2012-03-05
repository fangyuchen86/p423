;;; Official Verify Scheme

(library (compiler verify-scheme)
  (export verify-scheme)
  (import
    (chezscheme)
    (framework match)
    (framework helpers))

;;; Andy Keep, Kent Dybvig
;;; P423/P523
;;; Spring 2010

;;; verify-scheme accept a single value and verifies that the value
;;; is a valid program in the current source language.
;;;
;;; Grammar for verify-scheme (assignment 9):
;;;
;;;  Program --> (letrec ([<label> (lambda (<uvar>*) <Tail>)]*) <Tail>)
;;;  Tail    --> <Triv>
;;;           |  (binop <Value> <Value>)
;;;           |  (alloc <Value>)
;;;           |  (<Value> <Value>*)
;;;           |  (if <Pred> <Tail> <Tail>)
;;;           |  (begin <Effect>* <Tail>)
;;;           |  (let ([<uvar> <Value>]*) <Tail>)
;;;  Pred    --> (true)
;;;           |  (false)
;;;           |  (<relop> <Value> <Value>)
;;;           |  (if <Pred> <Pred> <Pred>)
;;;           |  (begin <Effect>* <Pred>)
;;;           |  (let ([<uvar> <Value>]*) <Pred>)
;;;  Effect  --> (nop)
;;;           |  (mset! <Value> <Value> <Value>)
;;;           |  (<Value> <Value>*)
;;;           |  (if <Pred> <Effect> <Effect>)
;;;           |  (begin <Effect>* <Effect>)
;;;           |  (let ([<uvar> <Value>]*) <Effect>)
;;;  Value   --> <Triv>
;;;           |  (<binop> <Value> <Value>)
;;;           |  (alloc <Value>)
;;;           |  (<Value> <Value>*)
;;;           |  (if <Pred> <Value> <Value>)
;;;           |  (begin <Effect>* <Value>)
;;;           |  (let ([<uvar> <Value>]*) <Value>)
;;;  Triv    --> <uvar> | <integer> | <label>
;;;
;;; Where uvar is symbol.n, n >= 0
;;;       binop is mref +, -, *, logand, logor, or sra
;;;       relop is <, <=, =, >=, >
;;;       label is symbol$n, n >= 0
;;;
;;; Each label bound by the letrec expression must have a unique suffix,
;;; and each uvar bound by a lambda or let expression must have a unique
;;; suffix, within the same Program.
;;;
;;; Machine constraints:
;;;   - sra's second operand must be an exact integer k, 0 <= k <= 63
;;;   - each other integer must be a exact integer n, -2^63 <= n <= 2^63-1
;;;
;;; If the value is a valid program, verify-scheme returns the value
;;; unchanged; otherwise it signals an error.

  (define-who verify-scheme
    (define binops '(mref + - * logand logor sra))
    (define relops '(< > <= >= =))
    (define verify-x-list
      (lambda (x* x? what)
        (let loop ([x* x*] [idx* '()])
          (unless (null? x*)
            (let ([x (car x*)] [x* (cdr x*)])
              (unless (x? x)
                (error who "invalid ~s ~s found" what x))
              (let ([idx (extract-suffix x)])
                (when (member idx idx*)
                  (error who "non-unique ~s suffix ~s found" what idx))
                (loop x* (cons idx idx*))))))))
    (define Program
      (lambda (x)
        (define all-uvar* '())
        (define Body
          (lambda (label* fml*)
            (define Triv
              (lambda (uvar*)
                (lambda (t)
                  (unless (or (label? t) (uvar? t) (and (integer? t) (exact? t)))
                    (error who "invalid Triv ~s" t))
                  (when (and (integer? t) (exact? t))
                    (unless (int64? t)
                      (error who "integer out of 64-bit range ~s" t)))
                  (when (uvar? t)
                    (unless (memq t uvar*)
                      (error who "reference to unbound uvar ~s" t)))
                  (when (label? t)
                    (unless (memq t label*)
                      (error who "unbound label ~s" t)))
                  (values))))
            (define Value
              (lambda (uvar*)
                (lambda (val)
                  (match val
                    [(let ([,new-uvar* ,[]] ...) ,val)
                     (set! all-uvar* (append new-uvar* all-uvar*))
                     ((Value (append new-uvar* uvar*)) val)]
                    [(if ,[(Pred uvar*) ->] ,[] ,[]) (values)]
                    [(begin ,[(Effect uvar*) ->] ... ,[]) (values)]
                    [(sra ,[] ,y)
                     (unless (uint6? y)
                       (error who "invalid sra operand ~s" y))
                     (values)]
                    [(,binop ,[] ,[])
                     (guard (memq binop binops))
                     (values)]
                    [(alloc ,[]) (values)]
                    [(,x ,y ...)
                     (guard (and (symbol? x) (not (or (uvar? x) (label? x)))))
                     (error who "invalid Value ~s" `(,x ,y ...))]
                    [(,[] ,[] ...) (values)]
                    [,[(Triv uvar*) ->] (values)]))))
            (define Effect
              (lambda (uvar*)
                (lambda (ef)
                  (match ef
                    [(nop) (values)]
                    [(let ([,new-uvar* ,[(Value uvar*) ->]] ...) ,ef)
                     (set! all-uvar* (append new-uvar* all-uvar*))
                     ((Effect (append new-uvar* uvar*)) ef)]
                    [(if ,[(Pred uvar*) ->] ,[] ,[]) (values)]
                    [(begin ,[] ... ,[]) (values)]
                    [(mset! ,[(Value uvar*) ->] 
                       ,[(Value uvar*) ->] 
                       ,[(Value uvar*) ->])
                     (values)]
                    [(,x ,y ...)
                     (guard (and (symbol? x) (not (or (uvar? x) (label? x)))))
                     (error who "invalid Effect ~s" `(,x ,y ...))]
                    [(,[(Value uvar*) ->] ,[(Value uvar*) ->] ...) (values)]
                    [,ef (error who "invalid Effect ~s" ef)]))))
            (define Pred
              (lambda (uvar*)
                (lambda (pr)
                  (match pr
                    [(true) (values)]
                    [(false) (values)]
                    [(let ([,new-uvar* ,[(Value uvar*) ->]] ...) ,pr)
                     (set! all-uvar* (append new-uvar* all-uvar*))
                     ((Pred (append new-uvar* uvar*)) pr)]
                    [(if ,[] ,[] ,[]) (values)]
                    [(begin ,[(Effect uvar*) ->] ... ,[]) (values)]
                    [(,relop ,[(Value uvar*) ->] ,[(Value uvar*) ->])
                     (guard (memq relop relops))
                     (values)]
                    [,pr (error who "invalid Pred ~s" pr)]))))
            (define Tail
              (lambda (uvar*)
                (lambda (tail)
                  (match tail
                    [(let ([,new-uvar* ,[(Value uvar*) ->]] ...) ,tail)
                     (set! all-uvar* (append new-uvar* all-uvar*))
                     ((Tail (append new-uvar* uvar*)) tail)]
                    [(if ,[(Pred uvar*) ->] ,[] ,[]) (values)]
                    [(begin ,[(Effect uvar*) ->] ... ,[]) (values)]
                    [(sra ,[(Value uvar*) ->] ,y)
                     (unless (uint6? y)
                       (error who "invalid sra operand ~s" y))
                     (values)]
                    [(,binop ,[(Value uvar*) ->] ,[(Value uvar*) ->])
                     (guard (memq binop binops))
                     (values)]
                    [(alloc ,[]) (values)]
                    [(,x ,y ...)
                     (guard (and (symbol? x) (not (or (uvar? x) (label? x)))))
                     (error who "invalid Tail ~s" `(,x ,y ...))]
                    [(,[(Value uvar*) ->] ,[(Value uvar*) ->] ...) (values)]
                    [,[(Triv uvar*) ->] (values)]))))
            (lambda (tail) ((Tail fml*) tail))))
        (define Lambda
          (lambda (label*)
            (lambda (x)
              (match x
                [(lambda (,fml* ...) ,[(Body label* fml*) ->])
                 (set! all-uvar* (append fml* all-uvar*))
                 (values)]
                [,x (error who "invalid Lambda ~a" x)]))))
        (match x
          [(letrec ([,label* ,[(Lambda label*) ->]] ...) ,[(Body label* '()) ->])
           (verify-x-list label* label? 'label)
           (verify-x-list all-uvar* uvar? 'uvar)]
          [,x (error who "invalid Program ~s" x)])))
    (lambda (x) (Program x) x)))

