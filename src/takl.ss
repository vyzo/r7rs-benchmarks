;;; TAKL -- The TAKeuchi function using lists as counters.

(def (mas x y z)
  (if (shorter? y x)
    (mas (mas (cdr x) y z)
         (mas (cdr y) z x)
         (mas (cdr z) x y))
    z))

(def (shorter? x y)
  (cond
   ((null? y) #f)
   ((null? x) #t)
   (else
    (using ((x :- :pair) (y :- :pair))
      (shorter? (cdr x)
                (cdr y))))))

(define (run-benchmark)
  (let* ((count (read))
         (input1 (read))
         (input2 (read))
         (input3 (read))
         (output (read))
         (s4 (number->string count))
         (s3 (number->string (length input3)))
         (s2 (number->string (length input2)))
         (s1 (number->string (length input1)))
         (name "takl"))
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2 ":" s3 ":" s4)
     count
     (lambda ()
       (mas (hide count input1) (hide count input2) (hide count input3)))
     (lambda (result) (equal? (length result) output)))))
