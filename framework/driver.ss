;; Copyright $\copyright$ 2011 Aaron W. Hsu $\langle\.{arcfide@sacrideo.us}\rangle$
;; \smallskip\noindent
;; Permission to use, copy, modify, and distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.
;; \smallskip\noindent
;; THE SOFTWARE IS PROVIDED ``AS IS'' AND THE AUTHOR DISCLAIMS ALL
;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
;; DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
;; PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
;; TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
;; PERFORMANCE OF THIS SOFTWARE.
;; }

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Adapted for P423 Spring 2012 by Claire Alvis
;;
;; These drivers allow you to define compilers and wrappers.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#!chezscheme
(library
  (framework driver aux)
  (export
    verify-pass-specifications
    verify-iterated-pass-specifications
    &pass-verification-violation
    wrapper-violation?
    wrapper-violation-name
    &wrapper-violation
    make-wrapper-violation
    make-pass-verification-violation
    pass-verification-violation?
    pass-verification-violation-pass
    pass-verification-violation-input
    pass-verification-violation-output
    pass-verification-violation-input-result
    pass-verification-violation-output-result)
  (import (chezscheme))

(define (verify-pass-specifications x)
  (syntax-case x (iterate break/when trace)
    [() #t]
    [((iterate . specs) . rest)
     (begin
       (verify-iterated-pass-specifications #'specs)
       (verify-pass-specifications #'rest))]
    [((break/when pred?) . rest)
     (syntax-violation 'define-compiler
       "break encountered outside of an iteration"
       x #'(break/when pred?))]
    [((pass wrapper) . rest) (identifier? #'pass)
     (verify-pass-specifications #'rest)]
    [((trace pass wrapper) . rest) (identifier? #'trace)
     (verify-pass-specifications #'rest)]
    [((pass wrapper assemble) . rest) (identifier? #'pass)
     (verify-pass-specifications #'rest)]
    [(bad . rest)
     (syntax-violation 'define-compiler
       "invalid pass specification" x #'bad)]
    [else
      (syntax-violation 'define-compiler
        "invalid pass specifications" x)]))

(define verify-iterated-pass-specifications
  (case-lambda
    [(x) (verify-iterated-pass-specifications x #f x)]
    [(orig break? x)
     (syntax-case x (iterate break/when trace)
       [()
        (unless break?
          (syntax-violation 'define-compiler "no break in iterate" orig))
        #t]
       [((break/when pred?) . rest)
        (verify-iterated-pass-specifications orig #t #'rest)]
       [((iterate spec1 spec2 ...) . rest)
        (verify-iterated-pass-specifications #'(spec1 spec2 ...))
        (verify-iterated-pass-specifications orig break? #'rest)]
       [((pass wrapper) . rest) (identifier? #'pass)
        (verify-iterated-pass-specifications orig break? #'rest)]
       [((trace pass wrapper) . rest) (identifier? #'pass)
        (verify-iterated-pass-specifications orig break? #'rest)]
       [((pass wrapper assemble) . rest) (identifier? #'pass)
        (syntax-violation 'define-compiler
          "emit pass encountered during iteration"
          orig #'(pass wrapper assemble))]
       [(wrong . rest)
        (syntax-violation 'define-compiler "invalid pass" orig #'wrong)]
       [wrong
         (syntax-violation
           'define-compiler "invalid, dotted form" orig)])]))

(define-condition-type &wrapper-violation &error
  make-wrapper-violation wrapper-violation?
  (name wrapper-violation-name))

(define-condition-type &pass-verification-violation &condition
  make-pass-verification-violation
  pass-verification-violation?
  (pass pass-verification-violation-pass)
  (input pass-verification-violation-input)
  (output pass-verification-violation-output)
  (input-result pass-verification-violation-input-result)
  (output-result pass-verification-violation-output-result))

)

#!chezscheme
(library
  (framework driver)
  (export
    trace
    iterate
    break/when
    environment
    &wrapper-violation
    make-wrapper-violation
    make-pass-verification-violation
    wrapper-violation-name
    wrapper-violation?
    pass-verification-violation?
    pass-verification-violation-pass
    &pass-verification-violation
    pass-verification-violation-input
    pass-verification-violation-input-result
    pass-verification-violation-output
    pass-verification-violation-output-result
    display-pass-verification-violation
    define-compiler
    define-language-wrapper)
  (import
    (chezscheme)
    (framework driver aux))

(define-syntax define-language-wrapper
  (syntax-rules (environment)
    [(_ (n1 n2 ...) (args ...) exps ...)
     (begin
       (define-language-wrapper n1 (args ...) exps ...)
       (define-language-wrapper n2 (args ...) exps ...)
       ...)]
    [(_ name (args ...) (environment env) exps ...)
     (define name
       (let ([the-env env])
         (lambda (args ...)
           (with-exception-handler
             (lambda (c)
               (cond
                 [(warning? c) (display-condition c)]
                 [else (raise
                         (condition (make-wrapper-violation 'name) c))]))
             (lambda ()
               (eval `(let () exps ...) the-env))))))]
    [(_ name (args ...) exps ...)
     (begin (define env (environment '(chezscheme)))
            (define-language-wrapper name (args ...)
              (environment env)
              exps ...))]))

(define-syntax compose-passes
  (syntax-rules (iterate break/when trace)
    [(_ check k (input source-wrapper)) input]
    [(_ check k (input source-wrapper) (iterate . specs) . rest)
     (compose-passes check k
       ((run-iterated-pass check input source-wrapper . specs)
        (next-wrapper source-wrapper . specs))
       . rest)]
    [(_ check k (input source-wrapper) (break/when pred?) . rest)
     (begin
       (when (not (syntax->datum #'k))
         (syntax-violation 'define-compiler
           "break encountered outside of iterate clause"
           #'(break/when pred?)))
       #t)
     (compose-passes check k
       ((let ([inv input]) (if (pred? inv) (k inv) inv))
        source-wrapper)
       . rest)]
    [(_ check k (input source-wrapper) (pass wrapper) . rest)
     (compose-passes check k
       ((run-pass check input source-wrapper wrapper pass) wrapper)
       . rest)]
    [(_ check k (input source-wrapper) (trace pass wrapper) . rest)
     (let ([pass (trace-lambda pass (i) (pass i))])
       (compose-passes check k (input source-wrapper) 
         (pass wrapper) . rest))]
    [(_ check k (input source-wrapper) (pass wrapper assemble) . rest)
     (begin
       (when (syntax->datum #'k)
         (syntax-violation 'define-compiler
           "unexpected emit pass inside iteration"
           #'(pass wrapper assemble)))
       (when (not (null? (syntax->datum #'rest)))
         (syntax-violation 'define-compiler
           "non-final assemble pass"
           #'(pass wrapper assemble)))
       #t)
     (run-emit-pass check input source-wrapper wrapper assemble pass)]))

(define-syntax next-wrapper
  (syntax-rules (iterate break/when trace)
    [(_ src specs ... (pass wrapper) (break/when . rest)) wrapper]
    [(_ src specs ... (trace pass wrapper) (break/when . rest)) wrapper]
    [(_ src (break/when . rest)) src]
    [(_ src specs ... last) (next-wrapper src specs ...)]))

(define-syntax run-pass
  (syntax-rules ()
    [(_ check input input-wrapper output-wrapper pass)
     (let ([inv input])
       (let ([output (pass inv)])
         (when (enum-set-member? 'pass check)
           (let ([input-res (input-wrapper inv)]
                 [output-res (output-wrapper output)])
             (verify-against inv input-res output output-res pass)))
         output))]))

(define-syntax run-emit-pass
  (syntax-rules ()
    [(_ check input input-wrapper output-wrapper assemble pass)
     (let ([inv input])
       (let ([output (assemble (lambda () (pass inv)))])
         (when (enum-set-member? 'pass check)
           (let ([input-res (input-wrapper inv)]
                 [output-res (output-wrapper output)])
             (verify-against inv input-res output output-res pass))))
       (void))]))

(define-syntax run-iterated-pass
  (syntax-rules (iterate break/when)
    [(_ check input input-wrapper specs ...)
     (call-with-current-continuation
       (lambda (k)
         (let loop ([x input])
           (loop
             (compose-passes check k (x input-wrapper)
               specs ...)))))]))

(define-syntax define-compiler-enumeration
  (syntax-rules (iterate % break/when)
    [(_ % name all (passes ...))
     (begin
       (define-enumeration contains (passes ...) name)
       (define all (make-enumeration '(passes ...))))]
    [(_ % name all (passes ...) (iterate spec1 spec2 ...) rest ...)
     (define-compiler-enumeration % name all (passes ...)
       spec1 spec2 ... rest ...)]
    [(_ % name all (passes ...) (break/when foo ...) rest ...)
     (define-compiler-enumeration % name all (passes ...)
       rest ...)]
    [(_ % name all (passes ...) (pass foo ...) rest ...)
     (define-compiler-enumeration % name all (passes ... pass)
       rest ...)]
    [(_ name all spec1 spec2 ...)
     (define-compiler-enumeration % name all () spec1 spec2 ...)]))

(define-syntax define-compiler-aux
  (syntax-rules ()
    ((_ (bindings ...) (name name-passes source-wrapper) spec1 spec2 ...)
     (verify-pass-specifications #'(spec1 spec2 ...))
     (begin
       (define-compiler-enumeration name-passes all spec1 spec2 ...)
       (define (name input . maybe-opts)
         (let ([passes-to-check
                 (if (null? maybe-opts) all (car maybe-opts))]
               bindings ...)
           (compose-passes passes-to-check #f (input source-wrapper) spec1 spec2 ...)))))))

(define-syntax rewrite-specs
  (syntax-rules (iterate trace break/when %)
    [(_ name name-passes wp (specs ...) (bindings ...))
     (define-compiler-aux ((sw (wp 'source)) bindings ...) (name name-passes sw) specs ...)]
    
    [(_ name name-passes wp (ispecs ... (specs ...)) (bindings ...) % rest ...)
     (rewrite-specs name name-passes wp
       ((iterate ispecs ...) specs ...)
       (bindings ...)
       rest ...)]
    
    [(_ name name-passes wp (specs ...) (bindings ...) (iterate spec1 spec2 ...) rest ...)
     (rewrite-specs name name-passes wp
       ((specs ...))
       (bindings ...)
       spec1 spec2 ... % rest ...)]
    
    [(_ name name-passes wp (specs ...) (bindings ...) (trace pass foo ...) rest ...)
     (rewrite-specs name name-passes wp
       (specs ... (trace pass w foo ...))
       (bindings ... (w (wp 'pass)))
       rest ...)]
    
    [(_ name name-passes wp (specs ...) (bindings ...) (break/when foo ...) rest ...)
     (rewrite-specs name name-passes wp
       ((break/when foo ...) specs ...)
       (bindings ...)
       rest ...)]
    
    [(_ name name-passes wp (specs ...) (bindings ...) (pass foo ...) rest ...)
     (rewrite-specs name name-passes wp
       (specs ... (pass w foo ...))
       (bindings ... (w (wp 'pass)))
       rest ...)]))

(define-syntax define-compiler
  (syntax-rules ()
    ((_ (name name-passes wrapper-proc) spec1 spec2 ...)
     (rewrite-specs name name-passes wrapper-proc () () spec1 spec2 ...))))

(define-syntax add-wrappers
  (syntax-rules (iterate % break/when)
    [(_ % name all (passes ...))
     (begin
       (define-enumeration contains (passes ...) name)
       (define all (make-enumeration '(passes ...))))]
    [(_ % name all (passes ...) (iterate spec1 spec2 ...) rest ...)
     (define-compiler-enumeration % name all (passes ...)
       spec1 spec2 ... rest ...)]
    [(_ % name all (passes ...) (break/when foo ...) rest ...)
     (define-compiler-enumeration % name all (passes ...)
       rest ...)]
    [(_ % name all (passes ...) (pass foo ...) rest ...)
     (define-compiler-enumeration % name all (passes ... pass)
       rest ...)]
    [(_ name all spec1 spec2 ...)
     (define-compiler-enumeration % name all () spec1 spec2 ...)]))

(define-syntax (iterate x)
  (syntax-violation #f "misplaced aux keyword" x))
(define-syntax (break/when x)
  (syntax-violation #f "misplaced aux keyword" x))

(define (verify-against inv input-res output output-res pass)
  (define (stringify x)
    (if (string? x)
        x
        (with-output-to-string (lambda () (write x)))))
  (unless (string=? (stringify input-res) (stringify output-res))
    (raise
      (condition
        (make-error)
        (make-format-condition)
        (make-irritants-condition (list pass))
        (make-pass-verification-violation
          pass inv output input-res output-res)
        (make-message-condition "~a failed verification")))))

(define display-pass-verification-violation
  (case-lambda
    [(condition) (%dpvv condition (current-output-port))]
    [(condition oport) (%dpvv condition oport)]))

(define (%dpvv c p)
  (assert (pass-verification-violation? c))
  (assert (output-port? p))
  (assert (textual-port? p))
  (format p
    "Verification of pass ~a failed.~n"
    (pass-verification-violation-pass c))
  (format p "~8,8tInput Pass:~n")
  (parameterize ([pretty-initial-indent 0])
    (pretty-print
      (pass-verification-violation-input c)
      p))
  (format p "~8,8tPass Output:~n")
  (parameterize ([pretty-initial-indent 0])
    (pretty-print
      (pass-verification-violation-output c)
      p)))

)
