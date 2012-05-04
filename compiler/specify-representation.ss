;; specify-representation.ss
;;
;; part of p423-sp12/srwaggon-p423
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in A10
;; 2012 / 4 / 26
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; revised in A10
;; 2012 / 4 / 29

#| Gotta be forward: Jelli Rohl gave me a ton of help on this one.
|| Justin Rohl (jrohl@indiana.edu).
|#

#!chezscheme
(library (compiler specify-representation)
  (export specify-representation)
  (import
   ;; Load Chez Scheme primitives:
   (chezscheme)
   ;; Load compiler framework:
   (framework match)
   (framework helpers)
   (compiler helpers)
   )


#| specify-representation : program --> program
|| specify-representation converts all Scheme datatypes
|| to their ptr equivalents so they are represented as
|| integers in the output language, and at the same time,
|| translate calls to Scheme primitives into calls to UIL
|| primitives.
|#

#| The following items summarize what must be done in this pass:
||
|| o Convert quoted fixnums and other immediates into their
||   unquoted ptr equivalents, using the values of the appropriate
||   helpers.ss variable.
||
|| o Adjust one multiplication operand, at compile time if possible,
||   otherwise at run time, using the helpers.ss variable shift-fixnum.
||
|| o Convert calls to cons and make-vector into calls to alloc. Take
||   advantage of constant lengths in make-vector, where possible.
||
|| o Convert calls to car, cdr, vector-length, and vector-ref into mref.
||   Take advantage of constant indices into vector-ref, where possible.
||
|| o Convert calls to set-car!, set-cdr!, and vector-set! into calls to
||   mset!. Take advantage of constant indices in vector-set!, where
||   possible.
||
|| o Expand the type predicates boolean?, fixnum?, pair?, and vector?
||   into calls to logand and =, inserting the appropriate values of
||   the symbolic mask and tag constants.
||
|| o Convert calls to eq? into calls to =. Treat calls to null? as
||   calls to eq? with the empty list as one argument, hence convert
||   them as well into calls to =.
|#
(define-who (specify-representation program)

  #|
  ||
  |#
  (define (Immediate immediate)
    (match immediate
      [() $nil]
      [#t $true]
      [#f $false]
      [,fixnum (guard (fixnum? fixnum)) (* fixnum (expt 2 shift-fixnum))]
      [,else (invalid who 'Immediate else)]
      ))

  #|
  ||
  |#
  (define (Effect effect)
    (define (effect-prim->ptr prim val)
      (match `(,prim . ,val)
        [(set-car! ,ls ,x) `(mset! ,ls ,(- disp-car tag-pair) ,x)]
        [(set-cdr! ,ls ,x) `(mset! ,ls ,(- disp-cdr tag-pair) ,x)]
        [(vector-set! ,vector ,i ,x)
         (let ([offset `(+ (- ,disp-vector-data ,tag-vector) ,i)])
           `(mset! ,vector
                   ,(if (integer? i)
                        (eval offset)
                        offset)
                   ,x))]
        [(procedure-set! ,proc ,i ,ef)
         (let ([offset `(+ (- ,disp-procedure-data ,tag-procedure) ,i)])
           `(mset! ,proc ,(eval offset) ,ef))]
        [,else (invalid who 'Effect-primitive else)]
        ))
    (match effect
      [(nop) '(nop)]
      [(begin ,[Effect -> ef*] ... ,[ef]) (make-begin `(,ef* ... ,ef))]
      [(if ,[Pred -> pr] ,[c] ,[a]) `(if ,pr ,c ,a)]
      [(let ([,uv* ,[Value -> vl*]] ...) ,[ef]) `(let ([,uv* ,vl*] ...) ,ef)]
      [(,ef-prim ,[Value -> vl*] ...) (guard (effect-prim? ef-prim)) (effect-prim->ptr ef-prim vl*)]
      [(,[Value -> rator] ,[Value -> rand*] ...) `(,rator ,rand* ...)]
      [,else (invalid who 'Effect else)]
      ))

  #|
  ||
  |#
  (define (Pred pred)
    (define (pred-prim->ptr prim val)
      (match `(,prim . ,val)
        [(,relop ,x ,y) (guard (relop? relop)) `(,relop ,x ,y)]
        [(null? ,x)    `(= ,x ,$nil)]
        [(eq? ,x ,y)   `(= ,x ,y)]
        [(procedure? ,x) `(= (logand ,x ,mask-procedure) ,tag-procedure)]
        [(boolean? ,x) `(= (logand ,x ,mask-boolean) ,tag-boolean)]
        [(fixnum?  ,x) `(= (logand ,x ,mask-fixnum ) ,tag-fixnum )]
        [(pair?    ,x) `(= (logand ,x ,mask-pair   ) ,tag-pair   )]
        [(vector?  ,x) `(= (logand ,x ,mask-vector ) ,tag-vector )]
        [,else (invalid who 'Pred-primitive else)]
        ))
    (match pred
      [(true) '(true)]
      [(false) '(false)]
      [(begin ,[Effect -> ef*] ... ,[pr]) (make-begin `(,ef* ... ,pr))]
      [(if ,[pr] ,[c] ,[a]) `(if ,pr ,c ,a)]
      [(let ([,uv* ,[Value -> vl*]] ...) ,[pr]) `(let ([,uv* ,vl*] ...) ,pr)]
      [(,pr-prim ,[Value -> vl*] ...) (guard (pred-prim? pr-prim)) (pred-prim->ptr pr-prim vl*)]
      [,else (invalid who 'Pred else)]
      ))

  #|
  ||
  |#
  (define (Value value)
    (define (value-prim->ptr prim val)
      (match `(,prim . ,val)
        [(,binop ,x ,y) (guard (binop? binop))
         (cond
           [(not (eq? binop '*)) `(,binop ,x ,y)]
           [(integer? x) `(* ,(/ x (expt 2 shift-fixnum)) ,y)]
           [(integer? y) `(* ,x ,(/ y (expt 2 shift-fixnum)))]
           [else `(* ,x (sra ,y ,shift-fixnum))])]
        [(car ,ls) `(mref ,ls ,(- disp-car tag-pair))]
        [(cdr ,ls) `(mref ,ls ,(- disp-cdr tag-pair))]
        [(cons ,a ,d)
         (let ([avar (unique-name 'car)]
               [dvar (unique-name 'cdr)]
               [ls (unique-name 'ls)])
           `(let ([,avar ,a]
                  [,dvar ,d])
              (let ([,ls (+ (alloc ,size-pair) ,tag-pair)])
                (begin
                  (mset! ,ls ,(- disp-car tag-pair) ,avar)
                  (mset! ,ls ,(- disp-cdr tag-pair) ,dvar)
                  ,ls))))]
        [(make-procedure ,lbl ,z)
         (let* ([offset `(+ ,disp-procedure-data ,z)]
                [v (unique-name 'v)])
           `(let ([,v (+ (alloc ,(eval offset))
                         ,tag-procedure)])
              (begin (mset! ,v ,(- disp-procedure-code tag-procedure) ,lbl)
                     ,v)))]
        [(make-vector ,e)
         (let* ([fixed (integer? e)]
                [v (unique-name 'v)]
                [len (if fixed e (unique-name 'e))]
                [offset `(+ ,disp-vector-data ,len)]
                [code `(let ([,v (+ (alloc ,(if fixed
                                                (eval offset)
                                                offset))
                                    ,tag-vector)])
                         (begin
                           (mset! ,v ,(- disp-vector-length tag-vector) ,len)
                           ,v))])
           (if fixed
               code
               `(let ([,len ,e]) ,code)))]
        [(procedure-code ,pr)
         `(mref ,pr ,(- disp-procedure-code tag-procedure))]
        [(procedure-ref ,cp ,i)
         (let ([offset (+ (- disp-procedure-data tag-procedure) i)])
           `(mref ,cp ,offset))]
        [(vector-length ,v) `(mref ,v ,(- disp-vector-length tag-vector))]
        [(vector-ref ,v ,i)
         (let ([offset `(+ (- ,disp-vector-data ,tag-vector) ,i)])
           `(mref ,v ,(if (integer? i) (eval offset) offset)))]
        [(void) $void]
        [,else (invalid who 'Value-primitive else)]
        ))
    (match value
      [(begin ,[Effect -> ef*] ... ,[vl]) (make-begin `(,ef* ... ,vl))]
      [(if ,[Pred -> pr] ,[c] ,[a]) `(if ,pr ,c ,a)]
      [(let ([,uv* ,[vl*]] ...) ,[vl]) `(let ([,uv* ,vl*] ...) ,vl)]
      [(quote ,[Immediate -> im]) im]
      [(,vl-prim ,[vl*] ...) (guard (value-prim? vl-prim)) (value-prim->ptr vl-prim vl*)]
      [(,[rator] ,[rand*] ...) `(,rator ,rand* ...)]
      [,uv (guard (uvar? uv)) uv]
      [,lbl (guard (label? lbl)) lbl]
      [,else (invalid who 'Value else)]
      ))

  #|
  ||
  |#
  (define (Program p)
    (match p
      [(letrec ([,label (lambda (,uvar* ...) ,[Value -> vl*])] ...) ,[Value -> vl])
       `(letrec ([,label (lambda (,uvar* ...) ,vl*)] ...) ,vl)]
      [,else (invalid who 'Program else)]
      ))

  (Program program)

)) ;; end library
