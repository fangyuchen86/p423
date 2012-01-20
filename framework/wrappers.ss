(library (framework wrappers aux)
  (export
    handle-overflow
    set!
    rewrite-opnds
    code
    jump
    locals
    (rename (lambda-p423 lambda))
    register-conflict
    locate
    true
    false
    nop)
  (import
    (except (chezscheme) set!)
    (framework match)
    (framework helpers))

(define int64-in-range?
  (lambda (x)
    (<= (- (expt 2 63)) x (- (expt 2 63) 1))))

(define handle-overflow
  (lambda (x)
    (cond
      [(not (number? x)) x]
      [(int64-in-range? x) x]
      [(not (= x (logand 18446744073709551615 x)))
       (handle-overflow (logand 18446744073709551615 x))]
      [(< x 0) (handle-overflow (+ x (expt 2 64)))]
      [else (handle-overflow (- x (expt 2 64)))])))

(define rewrite-opnds
  (lambda (x)
    (match x
      [,r (guard (disp-opnd? r))
       `(mref ,(disp-opnd-reg r) ,(disp-opnd-offset r))]
      [,r (guard (index-opnd? r))
       `(mref ,(index-opnd-breg r) ,(index-opnd-ireg r))]
      [(set! ,r ,[expr]) (guard (disp-opnd? r))
       `(mset! ,(disp-opnd-reg r) ,(disp-opnd-offset r) ,expr)]
      [(set! ,r ,[expr]) (guard (index-opnd? r))
       `(mset! ,(index-opnd-breg r) ,(index-opnd-ireg r) ,expr)]
      [(,[expr] ...) expr]
      [,x x])))

(define-syntax set!
  (let ()
    (import (chezscheme))
    (syntax-rules ()
      [(_ x expr)
       (set! x (handle-overflow expr))])))

(define-syntax code
  (lambda (x)
    (define build
      (lambda (body)
        (syntax-case body ()
          [() #'(())]
          [(label expr ...)
           (identifier? #'label)
           (with-syntax ([((expr ...) defn ...) (build #'(expr ...))])
             #'(((bounce label))
                (define label
                  (lambda ()
                    (bounce (lambda () expr ...))))
                defn ...))]
          [(expr1 expr ...)
           (with-syntax ([((expr ...) defn ...) (build #'(expr ...))])
             #'((expr1 expr ...) defn ...))])))
    (syntax-case x ()
      [(k expr ...)
       (with-syntax ([((expr ...) defn ...) (build #'(expr ...))])
         #'((call/cc
              (lambda (bounce)
                defn ...
                expr ...))))])))

(define-syntax jump
  (syntax-rules ()
    [(_ target) (target)]))

  (define-syntax locals
    (syntax-rules ()
      [(_ (x* ...) body) (let ([x* 0] ...) body)]))

(define-syntax lambda-p423
    (let ()
      (import scheme)
      (syntax-rules ()
        [(lambda () body) (lambda arg-list body)]
        [(lambda arg-list e e* ...) (lambda arg-list e e* ...)])))

(define-syntax register-conflict
  (syntax-rules ()
    [(_ ct body) body]))

(define-syntax locate
  (let ()
    (import scheme)
    (syntax-rules ()
      [(_ ([x* loc*] ...) body)
       (let-syntax ([x* (identifier-syntax 
                          (id loc*) 
                          ((set! id e) 
                           (set! loc* (handle-overflow e))))] ...)
         body)])))

(define (true) #t)

(define (false) #f)

(define (nop) (void))

)

(library (framework wrappers)
  (export
    pass->wrapper
    source/wrapper
    verify-scheme/wrapper
    finalize-locations/wrapper
    expose-frame-var/wrapper
    expose-basic-blocks/wrapper
    flatten-program/wrapper
    generate-x86-64/wrapper)
  (import
    (chezscheme)
    (framework match)
    (framework helpers)
    (framework driver))

(define env
  (environment
    '(except (chezscheme) set!)
    '(framework helpers)
    '(framework helpers frame-variables)))

(define rewrite-opnds
  (lambda (x)
    (match x
      [,r (guard (disp-opnd? r))
       `(mref ,(disp-opnd-reg r) ,(disp-opnd-offset r))]
      [,r (guard (index-opnd? r))
       `(mref ,(index-opnd-breg r) ,(index-opnd-ireg r))]
      [(set! ,r ,[expr]) (guard (disp-opnd? r))
       `(mset! ,(disp-opnd-reg r) ,(disp-opnd-offset r) ,expr)]
      [(set! ,r ,[expr]) (guard (index-opnd? r))
       `(mset! ,(index-opnd-breg r) ,(index-opnd-ireg r) ,expr)]
      [(,[expr] ...) expr]
      [,x x])))

(define pass->wrapper
  (lambda (pass)
    (case pass
      ((source) source/wrapper)
      ((verify-scheme) verify-scheme/wrapper)
      ((finalize-locations) finalize-locations/wrapper)
      ((expose-frame-var) expose-frame-var/wrapper)
      ((expose-basic-blocks) expose-basic-blocks/wrapper)
      ((flatten-program) flatten-program/wrapper)
      ((generate-x86-64) generate-x86-64/wrapper)
      (else (errorf 'pass->wrapper
              "Wrapper for pass ~s not found" pass)))))

(define-language-wrapper (source/wrapper verify-scheme/wrapper)
  (x)
  (environment env)
  (import
    (only (framework wrappers aux)
      handle-overflow set! locate true false nop))
  (call/cc (lambda (k) (set! ,return-address-register k) ,x))
  ,return-value-register)

(define-language-wrapper finalize-locations/wrapper
  (x)
  (environment env)
  (import
    (only (framework wrappers aux)
      handle-overflow set! true false nop))
  (call/cc (lambda (k) (set! ,return-address-register k) ,x))
  ,return-value-register)

(define-language-wrapper expose-frame-var/wrapper
  (x)
  (environment env)
  (import
    (only (framework wrappers aux)
      handle-overflow set! true false nop))
  (call/cc
    (lambda (k)
      (set! ,return-address-register k)
      ,(rewrite-opnds x)))
  ,return-value-register)

(define-language-wrapper expose-basic-blocks/wrapper
  (x)
  (environment env)
  (import
    (only (framework wrappers aux)
      handle-overflow set!))
  (call/cc 
    (lambda (k)
      (set! ,return-address-register k)
      ,(rewrite-opnds x)))
  ,return-value-register)

(define-language-wrapper flatten-program/wrapper
  (x)
  (environment env)
  (import
    (only (framework wrappers aux)
      handle-overflow set! code jump))
  (call/cc 
    (lambda (k)
      (set! ,return-address-register k)
      ,(rewrite-opnds x)))
  ,return-value-register)

(define (generate-x86-64/wrapper program)
  (let-values ([(out in err pid)
                (open-process-ports
                  (format "exec '~a'" program)
                  (buffer-mode block)
                  (native-transcoder))])
    (read in)))

)
