;;; MBROT -- Generation of Mandelbrot set fractal
;;; using Scheme's complex numbers.

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

(defrule (cpx a b)
  (:- (##cpxnum-make a b)
      :cpxnum))

(defrule (cpx+ a b)
  (:- (##cpxnum.+ a b)
      :cpxnum))

(defrule (cpx* a b)
  (:- (##cpxnum.* a b)
      :cpxnum))

(defrule (cpx-real c)
  (:- (##cpxnum-real c)
      :flonum))

(defrule (cpx-imag c)
  (:- (##cpxnum-imag c)
      :flonum))

(def (count (z0 : :cpxnum) (step : :flonum) (z : :cpxnum))
  => :fixnum
  (let* ((radius    4.0)
         (radius^2  (fl* radius radius)))
    (let ((z0 (cpx+ z0 (: (* z step) :cpxnum))))
      (let loop (((z :- :cpxnum) z0) ((c :- :fixnum) 0))
        => :fixnum
        (if (fx= c max-count)
            c
            (let* ((zr (cpx-real z))
                   (zi (cpx-imag z))
                   (zr^2 (fl* zr zr))
                   (zi^2 (fl* zi zi)))
              (if (fl> (fl+ zr^2 zi^2) radius^2)
                c
                (loop (cpx+ (cpx* z z) z0) (fx+ c 1)))))))))

(def (mbrot (matrix : Matrix) (z0 : :cpxnum) (step : :flonum) (n : :fixnum))
  (let loop1 (((y :- :fixnum) (fx- n 1)))
    (when (fx>= y 0)
      (let loop2 (((x :- :fixnum) (fx- n 1)))
        (if (fx>= x 0)
          (begin
            (Matrix-set! matrix x y
                         (count z0 step (cpx (inexact x) (inexact y))))
            (loop2 (fx- x 1)))
          (loop1 (fx- y 1)))))))

(def (test (n : :fixnum))
  (let (matrix (Matrix n))
    (mbrot matrix -1.0-0.5i 0.005 n)
    (Matrix-ref matrix 0 0)))

(define (run-benchmark)
  (let* ((count (read))
         (input1 (read))
         (output (read))
         (s2 (number->string count))
         (s1 (number->string input1))
         (name "mbrotZ"))
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s2)
     count
     (lambda () (test (hide count input1)))
     (lambda (result) (= result output)))))
