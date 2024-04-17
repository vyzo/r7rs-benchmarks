;;; FFT - Fast Fourier Transform, translated from "Numerical Recipes in C"

;;; We need R6RS div for this benchmark.

(def (div (x : :fixnum) (y : :fixnum))
  => :fixnum
  (cond
   ((fxquotient x y))
   ((fx< y 0)
    ;; x < 0, y < 0
    (let* ((q (fxquotient x y))
           (r (fx- x (fx* q y))))
      (if (fx= r 0)
        q
        (fx+ q 1))))
   (else
    ;; x < 0, y > 0
    (let* ((q (fxquotient x y))
           (r (fx- x (fx* q y))))
      (if (fx= r 0)
        q
        (fx- q 1))))))

;;(def sin sin)

(def (four1 (data : :f64vector))
  (let ((n (f64vector-length data))
        (pi*2 6.28318530717959)) ; to compute the inverse, negate this value

    ;; bit-reversal section

    (let loop1 (((i :- :fixnum) 0) ((j :- :fixnum) 0))
      (when (fx< i n)
        (when (fx< i j)
          (let ()
            (declare (not safe))        ; perfectly safe
            (let (temp (f64vector-ref data i))
              (f64vector-set! data i (f64vector-ref data j))
              (f64vector-set! data j temp))
            (let (temp (f64vector-ref data (fx+ i 1)))
              (f64vector-set! data (fx+ i 1) (f64vector-ref data (fx+ j 1)))
              (f64vector-set! data (fx+ j 1) temp))))
        (let loop2 (((m :- :fixnum) (div n 2)) ((j :- :fixnum) j))
          (if (and (fx>= m 2) (fx>= j m))
            (loop2 (div m 2) (fx- j m))
            (loop1 (fx+ i 2) (fx+ j m))))))

    ;; Danielson-Lanczos section

    (let loop3 (((mmax :- :fixnum) 2))
      (when (fx< mmax n)
        (let* ((theta
                (fl/ pi*2 (inexact mmax)))
               (wpr
                (let ((x (sin (fl* 0.5 theta))))
                  (fl* -2.0 (fl* x x))))
               (wpi
                (sin theta)))
          (let loop4 (((wr :- :flonum) 1.0) ((wi :- :flonum) 0.0) ((m :- :fixnum) 0))
            (when (fx< m mmax)
              (let loop5 (((i :- :fixnum) m))
                (if (fx< i n)
                  (let* ((j (fx+ i mmax))
                         (tempr
                          (fl- (fl* wr (f64vector-ref data j))
                               (fl* wi (f64vector-ref data (fx+ j 1)))))
                         (tempi
                          (fl+ (fl* wr (f64vector-ref data (fx+ j 1)))
                               (fl* wi (f64vector-ref data j)))))
                    (f64vector-set! data j
                      (fl- (f64vector-ref data i) tempr))
                    (f64vector-set! data (fx+ j 1)
                      (fl- (f64vector-ref data (fx+ i 1)) tempi))
                    (f64vector-set! data i
                      (fl+ (f64vector-ref data i) tempr))
                    (f64vector-set! data (fx+ i 1)
                      (fl+ (f64vector-ref data (fx+ i 1)) tempi))
                    (loop5 (fx+ j mmax)))
              (loop4 (fl+ (fl- (fl* wr wpr) (fl* wi wpi)) wr)
                     (fl+ (fl+ (fl* wi wpr) (fl* wr wpi)) wi)
                     (fx+ m 2))))))
          (loop3 (fx* mmax 2)))))))

(def data
  (make-f64vector 1024 0.0))

(def (run data)
  (four1 data)
  (f64vector-ref data 0))

(define (run-benchmark)
  (let* ((count (read))
         (input1 (read))
         (input2 (read))
         (output (read))
         (s2 (number->string count))
         (s1 (number->string input1))
         (name "fft"))
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda ()
       (run (hide count (make-f64vector input1 input2))))
     (lambda (result) (equal? result output)))))
