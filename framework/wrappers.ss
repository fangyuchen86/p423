(library (framework wrappers aux)
  (export
    handle-overflow
    set!
    rewrite-opnds
    code
    jump
    locals
    locate
    ulocals
    spills
    register-conflict
    true
    false
    nop
    frame-conflict)
  (import
    (except (chezscheme) set! letrec)
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

(define-syntax ulocals
  (syntax-rules ()
    [(_ (x* ...) body) (let ([x* 0] ...) body)]))

(define-syntax spills
  (syntax-rules ()
    [(_ (x* ...) body) (let ([x* 0] ...) body)]))

(define-syntax lambda-p423
    (let ()
      (import scheme)
      (syntax-rules ()
        [(lambda () body) (lambda arg-list body)]
        [(lambda arg-list e e* ...) (lambda arg-list e e* ...)])))

(define-syntax frame-conflict
  (syntax-rules ()
    [(_ ct body) body]))

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

(define-syntax letrec
    (let ()
      (import scheme)
      (syntax-rules (lambda)
        [(_ ([lab (lambda () lambda-body)] ...) letrec-body)
         (letrec ([lab (lambda ignore (parameterize ([fp-offset 0]) lambda-body))] ...)
           (parameterize ([fp-offset 0]) letrec-body))])))

(define-syntax new-frames
    (lambda (x)
      (import scheme)
      (syntax-case x (return-point)
        [(_ ((nfv ...) ...) expr)
         (with-syntax ([((i ...) ...) (map enumerate #'((nfv ...) ...))])
           #'(let ([top (fxsll frame-size word-shift)])
               (define-syntax nfv
                 (identifier-syntax
                   [id (mref (- ,frame-pointer-register (fp-offset))
                         (fxsll (+ i frame-size) word-shift))]
                   [(set! id e) 
                    (mset! (- ,frame-pointer-register (fp-offset))
                      (fxsll (+ i frame-size) word-shift)
                      e)]))
               ...
               ...
               expr))])))

  (define-syntax return-point
    (lambda (x)
      (import scheme)
      (syntax-case x ()
        [(_ rplab expr)
         #'(let ([top (fxsll frame-size word-shift)]
                 [rplab (lambda args (void))])
             (parameterize ([fp-offset (+ (fp-offset) top)])
               (set! ,frame-pointer-register
                 (+ ,frame-pointer-register top))
               expr
               (set! ,frame-pointer-register
                 (- ,frame-pointer-register top))))])))

(define (true) #t)

(define (false) #f)

(define (nop) (void))

)

(library (framework wrappers)
  (export
    pass->wrapper
    source/wrapper
    verify-scheme/wrapper
    remove-complex-opera*/wrapper
    flatten-set!/wrapper
    impose-calling-conventions/wrapper
    uncover-frame-conflict/wrapper
    pre-assign-frame/wrapper
    assign-new-frame/wrapper
    select-instructions/wrapper
    uncover-register-conflict/wrapper
    assign-registers/wrapper
    assign-frame/wrapper
    finalize-frame-locations/wrapper
    discard-call-live/wrapper
    finalize-locations/wrapper
    expose-frame-var/wrapper
    expose-basic-blocks/wrapper
    flatten-program/wrapper
    generate-x86-64/wrapper)
  (import
    (chezscheme)
    (framework match)
    (framework helpers)
    (framework driver)
    (only (framework wrappers aux) rewrite-opnds))

(define env
  (environment
    '(except (chezscheme) set! letrec)
    '(framework helpers)
    '(framework helpers frame-variables)))

(define pass->wrapper
  (lambda (pass)
    (case pass
      ((source) source/wrapper)
      ((verify-scheme) verify-scheme/wrapper)
      ((remove-complex-opera*) remove-complex-opera*/wrapper)
      ((flatten-set!) flatten-set!/wrapper)
      ((impose-calling-conventions) impose-calling-conventions/wrapper)
      ((uncover-frame-conflict) uncover-frame-conflict/wrapper)
      ((pre-assign-frame) pre-assign-frame/wrapper)
      ((assign-new-frame) assign-new-frame/wrapper)
      ((select-instructions) select-instructions/wrapper)
      ((uncover-register-conflict) uncover-register-conflict/wrapper)
      ((assign-registers) assign-registers/wrapper)
      ((assign-frame) assign-frame/wrapper)
      ((finalize-frame-locations) finalize-frame-locations/wrapper)
      ((discard-call-live) discard-call-live/wrapper)
      ((finalize-locations) finalize-locations/wrapper)
      ((expose-frame-var) expose-frame-var/wrapper)
      ((expose-basic-blocks) expose-basic-blocks/wrapper)
      ((flatten-program) flatten-program/wrapper)
      ((generate-x86-64) generate-x86-64/wrapper)
      (else (errorf 'pass->wrapper
              "Wrapper for pass ~s not found" pass)))))

;;-----------------------------------
;; source/wrapper
;; verify-scheme/wrapper
;; remove-complex-opera*/wrapper
;; flatten-set!/wrapper
;;-----------------------------------
(define-language-wrapper
  (source/wrapper verify-scheme/wrapper
   remove-complex-opera*/wrapper flatten-set!/wrapper)
  (x)
  (environment env)
  (import
    (only (framework wrappers aux)
      set! handle-overflow locals true false nop))
  (reset-machine-state!)
  ,x)

;;-----------------------------------
;; impose-calling-conventions/wrapper
;;-----------------------------------
(define-language-wrapper impose-calling-conventions/wrapper
  (x)
  (environment env)
  (import
    (only (framework wrappers aux)
      frame-size handle-overflow letrec set! locals new-frames
      return-point true false nop))
  (call/cc (lambda (k) (set! ,return-address-register k) ,x))
  ,return-value-register)

;;-----------------------------------
;; uncover-frame-conflict/wrapper
;;-----------------------------------
(define-language-wrapper uncover-frame-conflict/wrapper
  (x)
  (environment env)
  (import
    (only (framework wrappers aux)
      frame-size handle-overflow letrec set! locals spills call-live
      frame-conflict new-frames return-point true false nop))
  (call/cc (lambda (k) (set! ,return-address-register k) ,x))
  ,return-value-register)

;;-----------------------------------
;; uncover-register-conflict/wrapper
;;-----------------------------------
(define-language-wrapper uncover-register-conflict/wrapper (x) 
  (environment env)
  (import
    (only (framework wrappers aux)
      handle-overflow letrec set! locate locals ulocals frame-conflict
      register-conflict return-point true false nop))
  (call/cc (lambda (k) (set! ,return-address-register k) ,x))
  ,return-value-register)

;;----------------------------------
;; pre-assign-frame
;;----------------------------------
(define-language-wrapper pre-assign-frame/wrapper (x)
  (environment env)
  (import
    (only (framework wrappers aux)
      frame-size handle-overflow letrec set! locals locate call-live
      frame-conflict return-point true false nop))
  (call/cc (lambda (k) (set! ,return-address-register k) ,x))
  ,return-value-register)

;;----------------------------------
;; assign-new-frame
;;----------------------------------
(define-language-wrapper assign-new-frame/wrapper (x)
  (environment env)
  (import
    (only (framework wrapper aux)
      handle-overflow letrec set! locals ulocals spills locate
      frame-conflict return-point true false nop))
  (call/cc 
    (lambda (k)
      (set! ,return-address-register k)
      ,x))
  ,return-value-register)


;;-----------------------------------
;; finalize-frame-locations/wrapper
;; select-instructions/wrapper
;; assign-frame/wrapper
;;-----------------------------------
(define-language-wrapper
  (finalize-frame-locations/wrapper
   select-instructions/wrapper
   assign-frame/wrapper)
  (x)
  (environment env)
  (import
    (only (framework wrappers aux)
      handle-overflow letrec set! locate locals ulocals frame-conflict
      return-point true false nop))
  (call/cc (lambda (k) (set! ,return-address-register k) ,x))
  ,return-value-register)

;;-----------------------------------
;; assign-registers/wrapper
;;-----------------------------------
(define-language-wrapper assign-registers/wrapper (x)
  (environment env)
  (import
    (only (framework wrappers aux)
      handle-overflow letrec set! locate locals ulocals spills
      frame-conflict return-point true false nop))
  (call/cc (lambda (k) (set! ,return-address-register k) ,x))
  ,return-value-register)

;;-----------------------------------
;; discard-call-live/wrapper
;;-----------------------------------
(define-language-wrapper discard-call-live/wrapper (x)
  (environment env)
  (import
    (only (framework wrappers aux)
      handle-overflow letrec set! locate return-point true false nop))
  (call/cc (lambda (k) (set! ,return-address-register k) ,x))
  ,return-value-register)

;;-----------------------------------
;; finalize-locations/wrapper
;;-----------------------------------
(define-language-wrapper finalize-locations/wrapper (x)
  (environment env)
  (import
    (only (framework wrappers aux)
      handle-overflow letrec set! return-point true false nop))
  (call/cc (lambda (k) (set! ,return-address-register k) ,x))
  ,return-value-register)

;;-----------------------------------
;; expose-frame-var/wrapper
;;-----------------------------------
(define-language-wrapper expose-frame-var/wrapper (x)
  (environment env)
  (import
    (only (framework wrappers aux)
      handle-overflow set! return-point true false nop)
    (only (chezschem) letrec))
  (call/cc 
    (lambda (k)
      (set! ,return-address-register k)
      ,(rewrite-opnds x)))
  ,return-value-register)

;;-----------------------------------
;; expose-basic-blocks/wrapper
;;-----------------------------------
(define-language-wrapper expose-basic-blocks/wrapper (x)
  (environment env)
  (import
    (only (framework wrappers aux)
      handle-overflow set!)
    (only (chezschem) letrec))
  (call/cc
    (lambda (k)
      (set! ,return-address-register k)
      ,(rewrite-opnds x)))
  ,return-value-register)

;;-----------------------------------
;; flatten-program/wrapper
;;-----------------------------------
(define-language-wrapper flatten-program/wrapper (x)
  (environment env)
  (import
    (only (framework wrappers aux)
      handle-overflow set! code jump)
    (only (chezschem) letrec))
  (call/cc 
    (lambda (k)
      (set! ,return-address-register k)
      ,(rewrite-opnds x)))
  ,return-value-register)

;;-----------------------------------
;; generate-x86/wrapper
;;-----------------------------------
(define (generate-x86-64/wrapper program)
  (let-values ([(out in err pid)
                (open-process-ports
                  (format "exec '~a'" program)
                  (buffer-mode block)
                  (native-transcoder))])
    (read in)))

)
