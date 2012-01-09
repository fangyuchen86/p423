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

;; Adapated for P423 Spring 2012 by Claire Alvis

(library (framework testing)
  (export
    ;; to run tests
    test-valid
    test-invalid
    test-one
    test-number

    ;; debugging
    display-test-failure
    test-failure-condition)
  (import
    (chezscheme)
    (framework test-suite)
    (compiler compile)
    (framework driver))

(define-record test-runner
  (passed failed previous-result test-history))
(define (fresh-test-runner)
  (make-test-runner 0 0 #f '()))

(define test-runner-current
  (make-parameter (fresh-test-runner)
    (lambda (x) (and (test-runner? x) x))))

(define (reset-test-runner)
  (test-runner-current (fresh-test-runner)))

(define (test-valid)
  (test-compiler-suite 'p423-compile p423-compile (valid-tests)))

(define (test-invalid)
  (test-compiler-suite 'p423-compile p423-compile (invalid-tests)))

(define (test-compiler-suite name compile suite)
  (begin
    (reset-test-runner)
    (print-group-heading)
    (for-each (test-one compile) suite)
    (print-finalization)))

(define (test-one compile)
  (lambda (input)
    (let ((r (guard
               (x [else (process-result x)])
               (compile input))))
      (process-result r))))

(define (test-number num)
  ((test-one p423-compile) (list-ref (valid-tests) num)))

(define (process-result x)
  (begin
    (set-test-runner-previous-result! (test-runner-current) x)
    (print-individual-completion)
    (record-test-result! (test-runner-current))))

(define (print-group-heading)
  (printf "Test~8,8tResult~16,8tReason~n")
  (printf "---------------------------~n"))

(define (test-passed? runner)
  (let ((pr (test-runner-previous-result runner)))
    (and (not (error? pr))
         (not (pass-verification-violation? pr)))))

;;    1    Pass    
;;    2    Fail    Pass: PASS-NAME
;;    3    Fail    Runtime error
;; ...
(define (print-individual-completion)
  (let ((runner (test-runner-current)))
    (printf "~4d~8,8t~:[Fail~;Pass~]~16,8t~a~n"
      (current-test-number)
      (test-passed? runner)
      (short-error runner))))

(define (short-error runner)
  (let ((pr (test-runner-previous-result runner)))
    (cond
      [(error? pr)
       (cond
         [else "Runtime error"])]
      [else ""])))

;; Testing Summary
;; ---------------
;; Passes:         105
;; Failures:        25
;; Total:          200
(define (print-finalization)
  (let ((runner (test-runner-current)))
    (let ((passed (test-runner-passed runner))
          (failed (test-runner-failed runner)))
      (printf "~nTesting Summary~n")
      (printf "~a~n" (make-string 15 #\-))
      (printf "Passes:~16,8t~4d~n" passed)
      (printf "Failures:~16,8t~4d~n" failed)
      (printf "Total:~16,8t~4d~n" (+ passed failed)))))

(define (current-test-number)
  (let ((runner (test-runner-current)))
    (+ (test-runner-passed runner) (test-runner-failed runner))))

(define (record-test-result! runner)
  (let ((pr (test-runner-previous-result runner)))
    (cond
      ((and (not (test-passed? runner))
            (or (pass-verification-violation? pr)
                (error? pr)))
       (set-test-runner-test-history! runner
         (cons
           (cons (current-test-number) pr)
           (test-runner-test-history runner)))
       (set-test-runner-failed! runner
         (+ (test-runner-failed runner) 1)))
      (else
        (set-test-runner-passed! runner
          (+ (test-runner-passed runner) 1))))))

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

)