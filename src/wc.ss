;;; WC -- One of the Kernighan and Van Wyk benchmarks.
;;; Rewritten by Will Clinger into more idiomatic (and correct!) Scheme.


(def (wcport (port : :port))
  (let loop (((nl :- :fixnum) 0)
             ((nw :- :fixnum) 0)
             ((nc :- :fixnum) 0)
             (inword? #f))
    (let (x (##read-char port))
      (if (eof-object? x)
        [nl nw nc]
        (using (x :- :char)
          (cond
           ((char=? x #\space)
            (loop nl nw (fx+ nc 1) #f))
           ((char=? x #\newline)
            (loop (fx+ nl 1) nw (fx+ nc 1) #f))
           (else
            (loop nl (if inword? nw (fx+ nw 1)) (fx+ nc 1) #t))))))))

(def (go x)
  (call-with-input-file x wcport))

(define (run-benchmark)
  (let* ((count (read))
         (input (read))
         (output (read))
         (s2 (number->string count))
         (s1 input)
         (name "wc"))
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda () (go (hide count input)))
     (lambda (result) (equal? result output)))))
