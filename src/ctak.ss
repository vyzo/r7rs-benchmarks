;;; CTAK -- A version of the TAK procedure that uses continuations.

(def (ctak (x : :fixnum) (y : :fixnum) (z : :fixnum))
  (call-with-current-continuation
   (lambda ((k :- :procedure)) (ctak-aux k x y z))))

(def (ctak-aux (k : :procedure) (x : :fixnum) (y : :fixnum) (z : :fixnum))
  (if (not (fx< y x))
      (k z)
      (call-with-current-continuation
       (lambda ((k :- :procedure))
         (ctak-aux
          k
          (call-with-current-continuation
           (lambda ((k :- :procedure)) (ctak-aux k (fx- x 1) y z)))
          (call-with-current-continuation
           (lambda ((k :- :procedure)) (ctak-aux k (fx- y 1) z x)))
          (call-with-current-continuation
           (lambda ((k :- :procedure)) (ctak-aux k (fx- z 1) x y))))))))

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
         (name "ctak"))
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2 ":" s3 ":" s4)
     count
     (lambda ()
       (ctak (hide count input1) (hide count input2) (hide count input3)))
     (lambda (result) (equal? result output)))))
