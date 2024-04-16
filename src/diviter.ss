;;; DIVITER -- Benchmark which divides by 2 using lists of n ()'s.

(def (create-n (n : :fixnum))
  (do (((n :- :fixnum) n (fx- n 1))
       (a '() (cons '() a)))
      ((fx= n 0) a)))

(def (iterative-div2 l)
  (let loop ((l l) (a []))
    (if (null? l)
      a
      (using ((l :- :pair)
              (l-cdr (cdr l) :- :pair))
        (loop (cdr l-cdr)
              (cons (car l) a))))))

(define (run-benchmark)
  (let* ((count (read))
         (input1 (read))
         (output (read))
         (s2 (number->string count))
         (s1 (number->string input1))
         (ll (create-n (hide count input1)))
         (name "diviter"))
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda ()
       (iterative-div2 ll))
     (lambda (result) (equal? (length result) output)))))
