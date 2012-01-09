;; \def\title{TESTING FRAMEWORK (VERSION 1.0)}
;; \def\topofcontents{\null\vfill
;;   \centerline{\titlefont P423 Compiler Testing Framework}
;;   \vskip 15pt
;;   \centerline{(Version 1.0)}
;;   \vfill}
;; \def\botofcontents{\vfill
;; \noindent
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

;; @* Introduction. This module defines a testing framework for P423 
;; compilers it provides a set of convenience procedures and wrappers, 
;; coupled with a SRFI-64 test runner, that enables the efficient and 
;; manageable testing of P423 created compilers.
;; The |(p423 driver)| library provides the means to define 
;; compilers based on a series of passes and language wrappers.
;; Each compiler pass is checked for correctness as it runs.
;; Optionally, a compiler pass may be disabled on demand.
;; This provides some basic testing and verification of compilers
;; defined in these terms, but it does not handle the generalization 
;; of this to larger test suites of programs.
;; That is, given a single program source, we may be reasonably 
;; certain that a compiler for that program is correct by running 
;; the compiler on the source program, but there is no general
;; mechanism defined by |(p423 driver)| for automating this across 
;; many programs and displaying nice output.
;; To help with this, |(p423 testing)| defines conveniences for 
;; doing this sort of testing. 
;; It is based on the foundation provided by |(srfi :64)|. 
;; It provides the following general facilities:

;; \medskip{\parindent = 0.5in
;; \item{$\to$} A test runner for conveniently displaying test successes
;; and failures;
;; \item{$\to$} Convenience procedures for running a collection of tests
;; in a uniform way; and,
;; \item{$\to$} Conveniencees for inspecting and displaying errors and
;; test failures.
;; \par}

;; @(testing.sls@>=@q)
(library (framework testing)
  (export
    test-runner-current
    test-compiler-suite
    test-compiler
    make-p423-test-runner
    reset-p423-test-runner
    display-test-failure
    test-failure-condition)
  (import
    (chezscheme)
    (framework driver)))

(define-record test-runner
  (passed failed previous-result test-history))
(define (fresh-test-runner)
  (make-test-runner 0 0 #f '()))

(define test-runner-current
  (make-parameter (fresh-test-runner)
    (lambda (x) (and (test-runner? x) x))))

(define (reset-test-runner)
  (test-runner-current (fresh-test-runner)))

(define (test-compiler-suite name compile suite)
  (begin
    (reset-p423-test-runner)
    (for-each (test-one compile) suite)))

(define (test-one compile)
  (lambda (input)
    (guard
      (x [else (set-test-runner-previous-result! x)])
      (compile input))))

(define (make-p423-test-runner)
  #;
  (let ([runner (test-runner-null)])
    (test-runner-aux-value! runner '())
    (test-runner-on-group-begin! runner test-group-heading)
    (test-runner-on-test-end! runner test-completion-handler)
    (test-runner-on-final! runner finalization-handler)
    runner))

(define (print-group-heading)
  (printf "Test~8,8tResult~16,8tReason~n")
  (printf "---------------------------~n"))

;;    1    Pass    
;;    2    Fail    Pass: PASS-NAME
;;    3    Fail    Runtime error
;; ...
(define (print-individual-completion)
  (printf "~4d~8,8t~:[Fail~;Pass~]~16,8t~a~n"
    (current-test-number)
    (test-passed? runner)
    (short-error runner))
  (record-test-failure! runner))

(define (short-error runner)
  (let ((pr (test-runner-previous-result runner)))
    (cond
      [(error? pr)
       (cond
         [(pass-verification-violation? pr)
          (format "Pass: ~a" (pass-verification-violation-pass pr))]
         [else "Runtime error"])]
      [else ""])))

;; Testing Summary
;; ---------------
;; Passes:         105
;; Failures:        25
;; Total:          200
(define (print-finalization)
  (let ((passed (pass-count)) (failed (fail-count)))
    (printf "~nTesting Summary~n")
    (printf "~a~n" (make-string 15 #\-))
    (printf "Passes:~16,8t~4d~n" passed)
    (printf "Failures:~16,8t~4d~n" failed)
    (printf "Total:~16,8t~4d~n" (+ passed failed))))

(define (current-test-number runner)
  (+ (test-runner-passed runner) (test-runner-failed runner)))

(define (record-test-failure! runner)
  (when (and (not (test-passed? runner))
             (was-error? runner))
    (let ((pr (test-runner-previous-result runner)))
      (set-test-runner-previous-history! runner
        (cons
          (cons (current-test-number runner) pr)
          (test-runner-previous-history runner))))))

(define (display-test-failure test-num)
  (let ([res (test-failure-condition test-num)])
    (when res
      (cond
        [(pass-verification-violation? res)
         (display-pass-verification-violation res)]
        [(wrapper-violation? res)
         (printf "Error in wrapper ~a:~n"
           (wrapper-violation-name res))
         (display-condition res)]
        [else (display-condition res)])
      (newline))))

(define (test-failure-condition num)
  (let ([res (assv num
               (test-runner-test-history
                 (test-runner-current)))])
    (and res (cdr res))))


