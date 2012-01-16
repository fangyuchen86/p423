;; P423 Testing Framework

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
;;


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
;; (refine-test-suite <list of numbers>)
;;    test suite is refined to only include the tests from
;;    given list. causes original numbering to be lost.
;;
;; (test-compiler <new-test-compiler>)
;;    redefines which compiler is called. this should be a compiler
;;    exported in your (compiler compile) library.
;;
;; (reset-test-runner)
;;    reset the current test runner to a fresh test runner
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
  (passed failed-expected failed-unexpected test-history))
(define (fresh-test-runner)
  (make-test-runner 0 0 0 '()))
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
      (unless (list? x)
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

;; Mutable boolean flag that controls whether the framework is
;; currently running known-invalid tests.
;;
;; As Claire pointed out we need to be very careful that this is ONLY
;; true during test-all, NOT while the student is using the REPL.
(define expect-failure (make-parameter #f))

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

(define (test-valid)
  (begin
    (test-suite (valid-tests))
    (run-tests)))

(define (test-invalid)
  (parameterize ([expect-failure #t])
    (begin
      (test-suite (invalid-tests))
      (run-tests))))

(define (test-all)
  (begin
    (reset-test-runner)
    (let ((compiler (test-compiler))
          (runner (current-test-runner)))
      (begin
        (test-suite (valid-tests))
        (printf "Testing (valid-tests)\n")
        (print-group-heading)
        (for-each (test-one compiler runner) (test-suite))
        (printf "\nTesting (invalid-tests)\n")
        (print-group-heading)
        (test-suite (invalid-tests))
	(parameterize ([expect-failure #t])
	  (for-each (test-one compiler runner) (test-suite)))
        (print-finalization runner)
        (test-suite
          (append (valid-tests) (invalid-tests)))))))

(define (run-tests)
  (begin
    (reset-test-runner)
    (print-group-heading)
    (let ((compiler (test-compiler))
          (runner (current-test-runner))
          (suite (test-suite)))
      (begin
        (for-each (test-one compiler runner) suite)
        (print-finalization runner)))))

;; This prints out the information for a single test.
;; Don't use test-one to run the compiler on a single program,
;; just call the current compiler on that program.
(define (test-one compiler runner)
  (lambda (input)
    (let ((pr (guard (x [else x]) (compiler input))))
      (begin
        (print-individual-completion pr runner)
        (record-test-result pr runner)))))

(define (print-group-heading)
  (printf "Test~8,8tResult\n")
  (printf "---------------------------~n"))

(define (test-passed? pr)
  (and (not (error? pr))
       (not (pass-verification-violation? pr))))

;;    1    Pass    
;;    2    Fail    Pass: PASS-NAME
;;    3    Fail    Runtime error
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
    [(error? pr)
     (list "Fail" "Runtime error")]
    [else (list "Pass" "")]))

;; Testing Summary
;; ---------------
;; Passes:         105
;; Failures:        25
;; Total:          200
(define (print-finalization runner)
  (let ((passed (test-runner-passed runner))
        (failed-expected   (test-runner-failed-expected   runner))
	(failed-unexpected (test-runner-failed-unexpected runner)))
    (printf "~nTesting Summary~n")
    (printf "~a~n" (make-string 15 #\-))
    (printf "Passes:              ~3d~n" passed)
    (printf "Expected Failures:   ~3d~n" failed-expected)
    (printf "UNEXPECTED Failures: ~3d~n" failed-unexpected)
    (printf "Total:~16,8t~4d~n" (+ passed failed-expected failed-unexpected))))

(define (current-test-number runner)
  (+ (test-runner-passed runner)
     (test-runner-failed-unexpected runner)
     (test-runner-failed-expected   runner)))

;; This records the result of the previous test, whether it be a pass
;; (just increments test-runner-passed), or failed (increments
;; test-runner-failed-[un]expected and stores the error condition in the history).
(define (record-test-result pr runner)
  (cond
    ((test-passed? pr)
     (set-test-runner-passed! runner
       (+ (test-runner-passed runner) 1)))
    (else
      (begin
        (set-test-runner-test-history! runner
          (cons
            (cons (current-test-number runner) pr)
            (test-runner-test-history runner)))
        (if (expect-failure)
	    (set-test-runner-failed-expected!   runner (+ (test-runner-failed-expected   runner) 1))
	    (set-test-runner-failed-unexpected! runner (+ (test-runner-failed-unexpected runner) 1)))
   ))))

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
    (let ([res (assv test-num (test-runner-test-history runner))])
      (and res (cdr res)))))

)