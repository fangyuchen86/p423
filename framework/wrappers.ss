(language-wrapper
  (lambda (pass-name x)
    (case pass-name
      [(source verify-scheme)
       `(let ()
          (import (except (chezscheme) set!))
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
              (import scheme)
              (syntax-rules ()
                [(_ x expr) (set! x (handle-overflow expr))])))
          ,x
          rax)]
      [else x])))


;; (define-language-wrapper (source/wrapper verify-scheme/wrapper)
;;   (prefix
;;     (import
;;       (except (chezscheme) set!)
;;       (p423 compiler helpers))
;;     (define int64-in-range?
;;       (lambda (x)
;;         (<= (- (expt 2 63)) x (- (expt 2 63) 1))))
;;     (define handle-overflow
;;       (lambda (x)
;;         (cond
;;           [(not (number? x)) x]
;;           [(int64-in-range? x) x]
;;           [(not (= x (logand 18446744073709551615 x)))
;;            (handle-overflow (logand 18446744073709551615 x))]
;;           [(< x 0) (handle-overflow (+ x (expt 2 64)))]
;;           [else (handle-overflow (- x (expt 2 64)))])))
;;     (define-syntax set!
;;       (let ()
;;         (import scheme)
;;         (syntax-rules ()
;;           [(_ x expr) (set! x (handle-overflow expr))]))))
;;   (suffix rax))

;; (define (generate-x86-64/wrapper program)
;;   (let-values ([(out in err pid)
;;                 (open-process-ports
;;                   (format "exec '~a'" program)
;;                   (buffer-mode block)
;;                   (native-transcoder))])
;;     (read in)))


