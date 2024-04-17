;;; MBROT -- Generation of Mandelbrot set fractal.

(def max-count 64) ; u8vector is sufficient

(defstruct Matrix ((D :- :fixnum) (data :- :u8vector))
  constructor: :init!
  final: #t)

(defmethod {:init! Matrix}
  (lambda (self (D : :fixnum))
    (set! self.D D)
    (set! self.data (make-u8vector (fx* D D) 0))))

(def (Matrix-ref (m : Matrix) (i : :fixnum) (j : :fixnum))
  => :fixnum
  (let (off (fx+ (fx* m.D i) j))
    (:- (##u8vector-ref m.data off) :fixnum)))

(def (Matrix-set! (m : Matrix) (i : :fixnum) (j : :fixnum) (v : :fixnum))
  (let (off (fx+ (fx* m.D i) j))
    (##u8vector-set! m.data off v)))

(def (count (r : :flonum) (i : :flonum) (step : :flonum)
            (x : :fixnum) (y : :fixnum))
  => :fixnum
  (let ((radius^2  16.0)
        (cr (fl+ r (fl* (inexact x) step)))
        (ci (fl+ i (fl* (inexact y) step))))

      (let loop (((zr :- :flonum) cr)
                 ((zi :- :flonum) ci)
                 ((c  :- :fixnum) 0))
        => :fixnum
        (if (fx= c max-count)
          c
          (let ((zr^2 (fl* zr zr))
                (zi^2 (fl* zi zi)))
            (if (fl> (fl+ zr^2 zi^2) radius^2)
              c
              (let ((new-zr (fl+ (fl- zr^2 zi^2) cr))
                    (new-zi (fl+ (fl* 2.0 (fl* zr zi)) ci)))
                (loop new-zr new-zi (fx+ c 1)))))))))

(def (mbrot (matrix : Matrix)
            (r : :flonum) (i : :flonum) (step : :flonum)
            (n : :fixnum))
  (let loop1 (((y :- :fixnum) (fx- n 1)))
    (when (fx>= y 0)
      (let loop2 (((x :- :fixnum) (fx- n 1)))
        (if (fx>= x 0)
          (begin
            (Matrix-set! matrix x y (count r i step x y))
            (loop2 (- x 1)))
          (loop1 (- y 1)))))))

(def (test (n : :fixnum))
  (let (matrix (Matrix n))
    (mbrot matrix -1.0 -0.5 0.005 n)
    (Matrix-ref matrix 0 0)))

(define (run-benchmark)
  (let* ((count (read))
         (input1 (read))
         (output (read))
         (s2 (number->string count))
         (s1 (number->string input1))
         (name "mbrot"))
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda () (test (hide count input1)))
     (lambda (result) (= result output)))))
