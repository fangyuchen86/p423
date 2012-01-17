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
    (framework driver))

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
      [(,[expr] ...) `(,expr ...)]
      [,x x])))

(define pass->wrapper
  (lambda (pass)
    (case pass
      ((source) source/wrapper)
      ((verify-scheme) verify-scheme/wrapper)
      ((expose-frame-var) expose-frame-var/wrapper)
      ((flatten-program) flatten-program/wrapper)
      ((generate-x86-64) generate-x86-64/wrapper)
      (else (errorf 'pass->wrapper
              "Wrapper for pass ~s not found" pass)))))

(define-language-wrapper (source/wrapper verify-scheme/wrapper)
  (x)
  (import
    (except (chezscheme) set!)
    (framework helpers))
  (define-frame-variables)
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
  (define-syntax set!
    (let ()
      (import (chezscheme))
      (syntax-rules ()
        [(_ x expr) (set! x (handle-overflow expr))])))
  (call/cc
    (lambda (k)
      (set! r15 k)
      ,x))
  rax)

(define-language-wrapper expose-frame-var/wrapper
  (x)
  (import
    (except (chezscheme) set!)
    (framework helpers))
  (define-frame-variables)
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
  (define-syntax set!
    (let ()
      (import (chezscheme))
      (syntax-rules ()
        [(_ x expr)
         (set! x (handle-overflow expr))])))
  (call/cc 
    (lambda (k)
      (set! ,return-address-register k)
      ,(rewrite-opnds x)))
  ,return-value-register)

(define-language-wrapper flatten-program/wrapper
  (x)
  (import
    (except (chezscheme) set!)
    (framework helpers))
  (define-frame-variables)
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
