(library (framework wrappers aux)
  (export
    handle-overflow
    set!
    rewrite-opnds
    code
    jump)
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
      [(,[expr*] ...) expr*]
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

(define-syntax lambda
    (let ()
      (import scheme)
      (syntax-rules ()
        [(lambda () body) (lambda arg-list body)]
        [(lambda arg-list e e* ...) (lambda arg-list e e* ...)])))

(define (true) #t)

(define (false) #f)

(define (nop) (void))

)

(library (framework wrappers)
  (export
    pass->wrapper
    expose-frame-var/wrapper
    flatten-program/wrapper
    generate-x86-64/wrapper
    source/wrapper
    verify-scheme/wrapper)
  (import
    (chezscheme)
    (framework match)
    (framework helpers)
    (framework driver)
    (only (framework wrappers aux) rewrite-opnds))

(define env
  (environment
    '(except (chezscheme) set! lambda)
    '(framework helpers)
    '(framework helpers frame-variables)))

(define pass->wrapper
  (lambda (pass)
    (case pass
      ((source) source/wrapper)
      ((verify-scheme) verify-scheme/wrapper)
      ((uncover-register-conflict) uncover-register-conflict/wrapper)
      ((assign-registers) assign-registers/wrapper)
      ((discard-call-live) discard-call-live/wrapper)
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
  (import (only (framework wrappers aux)
            set! handle-overflow int64-in-range? locals
            lambda true false nop))
  (call/cc (lambda (k) (set! ,return-address-register k) ,x))
  ,return-value-register)

(define-language-wrapper uncover-register-conflict/wrapper (x) 
  (environment env)
  (import
    (except (chezscheme) set! lambda)
    (p423 compiler helpers))
  (define int64-in-range?
    (let ()
      (import scheme)
      (lambda (x)
        (<= (- (expt 2 63)) x (- (expt 2 63) 1)))))
  (define handle-overflow
    (let ()
      (import scheme)
      (lambda (x)
        (cond
          [(not (number? x)) x]
          [(int64-in-range? x) x]
          [(not (= x (logand 18446744073709551615 x)))
           (handle-overflow (logand 18446744073709551615 x))]
          [(< x 0) (handle-overflow (+ x (expt 2 64)))]
          [else (handle-overflow (- x (expt 2 64)))]))))
  (define-syntax set!
    (let ()
      (import scheme)
      (syntax-rules ()
        [(_ x expr)
         (set! x (handle-overflow expr))])))
  (define-syntax locals
    (syntax-rules ()
      [(_ (x* ...) body) (let ([x* 0] ...) body)]))
  (define-syntax lambda
    (let ()
      (import scheme)
      (syntax-rules ()
        [(lambda () body) (lambda arg-list body)]
        [(lambda arg-list e e* ...) (lambda arg-list e e* ...)])))
  (define-syntax register-conflict
    (syntax-rules ()
      [(_ ct body) body]))
  (define (true) #t)
  (define (false) #f)
  (define (nop) (void))
  (call/cc 
    (lambda (k)
      (set! r15 k)
      ,x))
  rax)

(define-language-wrapper expose-frame-var/wrapper
  (x)
  (environment env)
  (import (only (framework wrappers aux) set! handle-overflow))
  (call/cc 
    (lambda (k)
      (set! ,return-address-register k)
      ,(rewrite-opnds x)))
  ,return-value-register)

(define-language-wrapper flatten-program/wrapper
  (x)
  (environment env)
  (import (only (framework wrappers aux) set! handle-overflow code jump))
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
