;;; NQUEENS -- Compute number of solutions to 8-queens problem.
(def (nqueens (n : :fixnum))
  (def (append x y)
    (if (null? x)
      y
      (using (x :- :pair)
        (cons (car x)
              (append (cdr x) y)))))

  (def (my-try x y z)
    => :fixnum
    (if (null? x)
      (if (null? y)
        1
        0)
      (using (x :- :pair)
        (fx+ (if (ok? (car x) 1 z)
               (my-try (append (cdr x) y) [] (cons (car x) z))
               0)
             (my-try (cdr x) (cons (car x) y) z)))))

  (def (ok? (row :- :fixnum) (dist :- :fixnum) placed)
    (if (null? placed)
      #t
      (using ((placed :- :pair)
              (pos (car placed) :- :fixnum))
        (and (not (fx= pos (fx+ row dist)))
             (not (fx= pos (fx- row dist)))
             (ok? row (fx+ dist 1) (cdr placed))))))

  (my-try (iota n) '() '()))

(define (run-benchmark)
  (let* ((count (read))
         (input1 (read))
         (output (read))
         (s2 (number->string count))
         (s1 (number->string input1))
         (name "nqueens"))
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda () (nqueens (hide count input1)))
     (lambda (result) (= result output)))))
