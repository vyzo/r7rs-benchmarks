;;; CPSTAK -- A continuation-passing version of the TAK benchmark.
;;; A good test of first class procedures and tail recursion.


(def (cpstak (x : :fixnum) (y : :fixnum) (z : :fixnum))

  (def (tak (x : :fixnum) (y : :fixnum) (z : :fixnum) (k : :procedure))
    (if (not (fx< y x))
        (k z)
        (tak (fx- x 1)
             y
             z
             (lambda ((v1  :- :fixnum))
               (tak (fx- y 1)
                    z
                    x
                    (lambda ((v2 :- :fixnum))
                      (tak (fx- z 1)
                           x
                           y
                           (lambda ((v3 :- :fixnum))
                             (tak v1 v2 v3 k)))))))))

  (tak x y z (lambda (a) a)))

(define (run-benchmark)
  (let* ((count (read))
         (input1 (read))
         (input2 (read))
         (input3 (read))
         (output (read))
         (s4 (number->string count))
         (s3 (number->string input3))
         (s2 (number->string input2))
         (s1 (number->string input1))
         (name "cpstak"))
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2 ":" s3 ":" s4)
     count
     (lambda ()
       (cpstak (hide count input1) (hide count input2) (hide count input3)))
     (lambda (result) (equal? result output)))))
