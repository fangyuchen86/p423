;; uncover-free.ss
;;
;; part of p423-sp12/srwaggon-p423
;; http://github.iu.edu/p423-sp12/srwaggon-p423
;; introduced in A12
;; 2012 / 5 / 4
;;
;; Samuel Waggoner
;; srwaggon@indiana.edu
;; revised in A12
;; 2012 / 5 / 4

#!chezscheme
(library (compiler uncover-free)
  (export uncover-free)
  (import
   ;; Load Chez Scheme primitives:
   (chezscheme)
   ;; Load compiler framework:
   (framework match)
   (framework helpers)
   (compiler helpers)
   )

#| This pass annotates each lambda expression with a list of its free variables.
|| It does so via a new form, free, wrapped around each lambda body.
|#
(define-who (uncover-free program)
  

  ;; dirty hack: find uvar --> lol hai r u in mai locals? no ok bai
  (define (uncovs-free ex uv*)
    (match ex
      [,uv (guard (uvar? uv)) (if (member uv uv*) '() `(,uv))]
      [(let ([,uv** ,ex*] ...) ,ex^)
       (filter (lambda (x)
                 (if (null? x) '()
                     (not (member x uv*))))
               (union (uncovs-free ex* uv**) 
                      (uncovs-free ex^ uv**)))]
      [(letrec ([,uv** ,ex*] ...) ,ex^)
       (filter (lambda (x)
                 (if (null? x) '()
                     (not (member x uv*))))
               (union (uncovs-free ex* uv**)
                      (uncovs-free ex^ uv**)))]
      [(lambda (,param* ...) ,bd)
       (filter (lambda (x)
                 (if (null? x) '()
                     (not (member x uv*))))
               (uncovs-free bd param*))]
      [(,x . ,y) (union (uncovs-free x uv*) (uncovs-free y uv*))]
      [,else '()]
      ))

  
  (define (Immediate immediate)
    (match immediate
      [,im (guard (immediate? im)) im]
      [,else (invalid who 'Immediate else)]
      ))

  (define (Expr expr)
    (match expr
      [,uv (guard (uvar? uv)) uv]
      [(quote ,[Immediate -> im]) `(quote ,im)]
      [(if ,[t] ,[c] ,[a]) `(if ,t ,c ,a)]
      [(begin ,[ex*] ... ,[ex]) `(begin ,ex* ... ,ex)]
      [(let ([,uv* ,[ex*]] ...) ,[ex]) `(let ([,uv* ,ex*] ...) ,ex)]
      [(letrec ([,uv* (lambda (,uv** ...) ,[ex*])] ...) ,[ex])
       (let ([free** (map uncovs-free ex* uv**)])
         `(letrec ([,uv* (lambda (,uv** ...) (free ,free** ,ex*))] ...) ,ex)
         )]
      [(,prim ,[ex*] ...) (guard (prim? prim)) `(,prim ,ex* ...)]
      [(,[rator] ,[rand*] ...) `(,rator ,rand* ...)]
      [,else (invalid who 'Expr else)]
      ))

  (Expr program)

)) ;; end library