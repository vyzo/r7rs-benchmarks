;;; DERIV -- Symbolic derivation.

;;; Returns the wrong answer for quotients.
;;; Fortunately these aren't used in the benchmark.
(def (deriv a)
  (match a
    (['+ . args]
     ['+ (map deriv args) ...])
    (['- . args]
     ['- (map deriv args) ...])
    (['* . args]
     ['* a ['+ (map (lambda (a) ['/ (deriv a) a]) args) ...]])
    (['/ a1 a2]
     ['- ['/ (deriv a1) a2]
         ['/ a1 ['* a2 a2 (deriv a2)]]])
    ('x 1)
    (else 0)))

(define (run-benchmark)
  (let* ((count (read))
         (input1 (read))
         (output (read))
         (s (number->string count))
         (name "deriv"))
    (run-r7rs-benchmark
     (string-append name ":" s)
     count
     (lambda () (deriv (hide count input1)))
     (lambda (result) (equal? result output)))))
