;; P423 Testing Framework

;; Copyright (C) 2011 Aaron W. Hsu {arcfide@sacrideo.us}
;; Permission to use, copy, modify, and distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.
;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
;; DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
;; PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
;; TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
;; PERFORMANCE OF THIS SOFTWARE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Adapated for P423 Spring 2012 by Claire Alvis and Chris Frisz
;;
;; This testing framework will run the tests found in (framework
;; test-suite) over any compiler exported in (compiler compile).
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; To run a test suite:
;; (test-valid)    runs all the valid tests
;; (test-invalid)  runs all the invalid tests
;; (test-all)      runs all the tests
;; (run-tests)
;;    runs the compiler in (test-compiler) over the tests in
;;    (test-suite) with a fresh test runner. if you've customized
;;    anything, use this to run your customizations.
;;
;; Debugging:
;; After you run a test suite, you may use the following procedures
;; to recover what went wrong with the tests that failed.
;;
;; (display-test-failure <test number>)
;;    returns a description of the test's error condition
;;
;; (inspect (test-failure-condition <test number>))
;;    this enters the scheme debugger with the error condition
;;    so you can inspect it
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Customizations:
;;
;; (test-suite <new-test-suite>)
;;    will redefine the test suite. 
;; (refine-test-suite <any number of test numbers>)
;;    test suite is refined to only include the desired tests.
;;    causes original numbering to be lost.
;;
;; (test-compiler <new-test-compiler>)
;;    redefines which compiler is called. this should be a
;;    compiler exported in your (compiler compile) library.
;;
;; (reset-test-runner)
;;    reset the current test runner to a fresh test runner.
;;    doing this will discard all the information from the
;;    previous run of the test suite
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(library (framework testing)
  (export
    ;; to run tests
    test-valid
    test-invalid
    test-all
    run-tests

    ;; to change the parameters
    test-compiler
    test-suite
    refine-test-suite

    ;; debugging
    display-test-failure
    test-failure-condition)
  
  (import
    (chezscheme)
    (framework test-suite)
    (compiler compile)
    (framework driver))

;; Record that defines the test runner. This records the number of
;; passed/failed tests cases, the result of the previous test, and an
;; entire test history.
(define-record test-runner
  (pass-expected pass-unexpected
   fail-expected fail-unexpected
   history))

(define (incr-field record-set! field)
  (lambda (runner)
    (record-set! runner (+ (field runner) 1))))
(define incr-pass-expected
  (incr-field set-test-runner-pass-expected! test-runner-pass-expected))
(define incr-pass-unexpected
  (incr-field set-test-runner-pass-unexpected! test-runner-pass-unexpected))
(define incr-fail-expected
  (incr-field set-test-runner-fail-expected! test-runner-fail-expected))
(define incr-fail-unexpected
  (incr-field set-test-runner-fail-unexpected! test-runner-fail-unexpected))
(define (add-to-history runner p)
  (set-test-runner-history! runner
    (cons p (test-runner-history runner))))

(define (fresh-test-runner)
  (make-test-runner 0 0 0 0 '()))
(define (reset-test-runner)
  (current-test-runner (fresh-test-runner)))
(define current-test-runner
  (make-parameter (fresh-test-runner)
    (lambda (x)
      (unless (test-runner? x)
        (errorf 'current-test-runner
          "~s is not a valid test-runner" x))
      x)))

(define test-suite
  (make-parameter '()
    (lambda (x)
      (unless (and (list? x) (for-all test-case? x))
        (errorf 'test-suite
          "~s is not a valid test suite" x))
      x)))

(define test-compiler
  (make-parameter p423-compile
    (lambda (x)
      (unless (procedure? x)
        (errorf 'test-compiler
          "~s is not a valid compiler" x))
      x)))

(define (refine-test-suite . num)
  (let* ((suite (test-suite))
         (max-index (- (length suite) 1)))
    (let ((new-suite
            (map
              (lambda (n)
                (unless (<= 0 n max-index)
                  (errorf 'refine-test-suite
                    "Number ~s not a valid test index" n))
                (list-ref suite n))
              num)))
      (begin
        (test-suite new-suite)
        (reset-test-runner)))))

(define (refine-to-unexpected)
  (let ((runner (current-test-runner)))
    (let ((unexpected
            (map car (filter cadr (test-runner-history runner)))))
      (apply refine-test-suite unexpected))))

(define (test-valid)
  (begin
    (test-suite (valid-tests))
    (run-tests)))

(define (test-invalid)
  (begin
    (test-suite (invalid-tests))
    (run-tests)))

(define (test-all)
  (begin
    (reset-test-runner)
    (let ((compiler (test-compiler))
          (runner (current-test-runner))
          (vt (valid-tests))
          (it (invalid-tests)))
      (begin
        ;; Process the valid tests
        (test-suite vt)
        (printf "Testing (valid-tests)\n")
        (print-group-heading)
        (for-each (test-one compiler runner) (test-suite))

        ;; Process the invalid tests
        (test-suite it)
        (printf "\nTesting (invalid-tests)\n")
        (print-group-heading)
        (for-each (test-one compiler runner) (test-suite))

        ;; Finish up
        (print-finalization runner)
        (test-suite (append vt it))))))

;; Runs the compiler in (test-compiler) over the tests in (test-suite)
;; with a fresh test runner. If you've customized anything, use this
;; to run your customizations.
(define (run-tests)
  (begin
    (reset-test-runner)
    (let ((compiler (test-compiler))
          (runner (current-test-runner))
          (suite (test-suite)))
      (begin 
        (print-group-heading)
        (for-each (test-one compiler runner) suite)
        (print-finalization runner)))))

;; This prints out the information for a single test.
;; Don't use test-one to run the compiler on a single program,
;; just call the current compiler on that program.
(define (test-one compiler runner)
  (lambda (input)
    (let ((pr (guard (x [else x])
                (compiler (test-case-source input))))
          (expected-failure
            (not (test-case-valid? input))))
      (begin
        (print-individual-completion pr runner)
        (record-test-result pr expected-failure runner)))))

(define (print-group-heading)
  (printf "Test~8,8tResult\n")
  (printf "---------------------------~n"))

;;    1    Pass    
;;    2    Fail    Pass: PASS-NAME
;;    3    Fail    Compile-time error
;; ...
(define (print-individual-completion pr runner)
  (apply printf "~4d    ~8a~a~n"
    (current-test-number runner)
    (result->string pr)))

(define (result->string pr)
  (cond
    [(wrapper-violation? pr)
     (list "Fail" "Wrapper violation")]
    [(pass-verification-violation? pr)
     (list "Fail"
       (format "~a: ~s"
         "Verification error"
         (pass-verification-violation-pass pr)))]
    [(or (error? pr) (violation? pr))
     (list "Fail" "Runtime error")]
    [else (list "Pass" "")]))

;; Testing Summary
;; ----------------------------
;; Expected Passess:        100
;; Unexpected Passes:        10
;; Expected Failures:        20
;; Unexpected Failures:      25
;; Total:                   200
(define (print-finalization runner)
  (let ((pass-expected   (test-runner-pass-expected   runner))
        (pass-unexpected (test-runner-pass-unexpected runner))
        (fail-expected   (test-runner-fail-expected   runner))
        (fail-unexpected (test-runner-fail-unexpected runner)))
    (printf "~nTesting Summary~n")
    (printf "~a~n" (make-string 28 #\-))
    (printf "Expected Passes:~24,8t~4d~n"      pass-expected)
    (printf "Unexpected Passes:~24,8t~4d~n"    pass-unexpected)
    (printf "Expected Failures:~24,8t~4d~n"    fail-expected)
    (printf "Unexpected Failures: ~24,8t~4d~n" fail-unexpected)
    (printf "Total:~24,8t~4d~n"
      (+ pass-expected pass-unexpected
         fail-expected fail-unexpected))))

;; Calculates the current test number
(define (current-test-number runner)
  (+ (test-runner-pass-expected   runner)
     (test-runner-pass-unexpected runner)
     (test-runner-fail-expected   runner)
     (test-runner-fail-unexpected runner)))

;; This records the result of the previous test, whether it be an
;; expected pass (just increments test-runner-pass-expected), or
;; failed (increments test-runner-failed-[un]expected and stores the
;; error condition in the history). also store the unexpected-passes.
(define (record-test-result pr ef runner)
  (cond
    ((or (error? pr)
         (wrapper-violation? pr)
         (pass-verification-violation? pr)
         (violation? pr))
     (begin
       (add-to-history runner
         (cons (current-test-number runner)
           ;; If the failure was expected, store #f. otherwise #t
           (cons (not ef) pr)))
       (if ef
           ;; An expected failure
           (incr-fail-expected runner)
           ;; An unexpected failure
           (incr-fail-unexpected runner))))
    (else
      (if ef
          ;; An unexpected pass
          ;; Since this is an unexpected result, store #t.
          (begin
            (add-to-history runner
              (cons (current-test-number runner)
                (cons #t (void))))
            (incr-pass-unexpected runner))
          ;; An expected pass
          (incr-pass-expected runner)))))

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

(define (test-failure-condition test-num)
  (let ((runner (current-test-runner)))
    (let ([res (assv test-num (test-runner-history runner))])
      (and res (cddr res)))))

)

