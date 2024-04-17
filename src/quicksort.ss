;; This is probably from Lars Hansen's MS thesis.
;; The quick-1 benchmark.  (Figure 35, page 132.)

(def (quick-1 (v : :vector) (less? : :procedure))

  (def (helper (left :- :fixnum) (right :- :fixnum))
    (if (fx< left right)
        (let ((median (partition v left right less?)))
          (if (fx< (fx- median left) (fx- right median))
              (begin (helper left (fx- median 1))
                     (helper (fx+ median 1) right))
              (begin (helper (fx+ median 1) right)
                     (helper left (fx- median 1)))))
        v))

  (helper 0 (fx- (vector-length v) 1)))


(def (partition (v : :vector) (left : :fixnum) (right : :fixnum) (less? : :procedure))
  => :fixnum
  ;; check the bounds and then we can just (declare (not safe))
  (using ((left  :~ (in-range? 0 (vector-length v)))
          (right :~ (in-range? left (vector-length v))))
    (declare (not safe))
    (using (mid (vector-ref v right) :- :fixnum)

      (def (uploop (i :- :fixnum))
        => :fixnum
        (let (i (fx+ i 1))
          (if (and (fx< i right) (less? (vector-ref v i) mid))
            (uploop i)
            i)))

      (def (downloop (j :- :fixnum))
        => :fixnum
        (let (j (fx- j 1))
          (if (and (fx> j left) (less? mid (vector-ref v j)))
            (downloop j)
            j)))

      (def (ploop (i :- :fixnum) (j :- :fixnum))
        => :fixnum
        (let* ((i (uploop i))
               (j (downloop j)))
          (using (tmp (vector-ref v i) :- :fixnum)
            (vector-set! v i (vector-ref v j))
            (vector-set! v j tmp)
            (if (fx< i j)
              (ploop i j)
              (begin (vector-set! v j (vector-ref v i))
                     (vector-set! v i (vector-ref v right))
                     (vector-set! v right tmp)
                     i)))))

      (ploop (fx- left 1) right))))

;;; Hansen's original code for this benchmark used Larceny's
;;; predefd random procedure.  When Marc Feeley modified
;;; Hansen's benchmark for the Gambit benchmark suite, however,
;;; he added a specific random number generator taken from an
;;; article in CACM.  Feeley's generator used bignums, and was
;;; extremely slow, causing the Gambit version of this benchmark
;;; to spend nearly all of its time generating the random numbers.
;;; For a benchmark called quicksort to become a bignum benchmark
;;; was very misleading, so Clinger left Feeley's version of this
;;; benchmark out of the Larceny benchmark suite.
;;;
;;; The following random number generator is much better and
;;; faster than the one used in the Gambit benchmark.  See
;;;
;;; http://srfi.schemers.org/srfi-27/mail-archive/msg00000.html
;;; http://www.math.purdue.edu/~lucier/random/random.scm

;;; A uniform [0,1] random number generator; is
;;; Pierre L'Ecuyer's generator from his paper
;;; "Good parameters and implementations for combined multiple
;;; recursive random number generators"
;;; available at his web site http://www.iro.umontreal.ca/~lecuyer

(def norm 2.328306549295728e-10)
(def m1 4294967087.0)
(def m2 4294944443.0)
(def a12 1403580.0)
(def a13n 810728.0)
(def a21 527612.0)
(def a23n 1370589.0)
(def seed (f64vector 1.0 0.0 0.0 1.0 0.0 0.0)) ;; will be mutated

(def (random-flonum)
  => :flonum
  (let ((seed seed)) ;; make it local
    (let ((p1 (fl- (fl* a12 (f64vector-ref seed 1))
                   (fl* a13n (f64vector-ref seed 0))))
          (p2 (fl- (fl* a21 (f64vector-ref seed 5))
                   (fl* a23n (f64vector-ref seed 3)))))
      (let ((k1 (truncate (fl/ p1 m1)))
            (k2 (truncate (fl/ p2 m2)))
            (ignore1 (f64vector-set! seed 0 (f64vector-ref seed 1)))
            (ignore3 (f64vector-set! seed 3 (f64vector-ref seed 4))))
        (let ((p1 (fl- p1 (fl* k1 m1)))
              (p2 (fl- p2 (fl* k2 m2)))
              (ignore2 (f64vector-set! seed 1 (f64vector-ref seed 2)))
              (ignore4 (f64vector-set! seed 4 (f64vector-ref seed 5))))
          (let ((p1 (if (fl< p1 0.0) (fl+ p1 m1) p1))
                (p2 (if (fl< p2 0.0) (fl+ p2 m2) p2)))
            (f64vector-set! seed 2 p1)
            (f64vector-set! seed 5 p2)
            (if (fl<= p1 p2)
              (fl* norm (fl+ (fl- p1 p2) m1))
              (fl* norm (fl- p1 p2)))))))))


(def (random (n : :fixnum))
  => :fixnum
  (:- (exact (truncate (fl* (inexact n) (random-flonum))))
      :fixnum))

;;; Even with the improved random number generator,
;;; this benchmark still spends almost all of its time
;;; generating the random vector.  To make this a true
;;; quicksort benchmark, we generate a relatively small
;;; random vector and then sort many copies of it.

(define (run-benchmark)
  (let* ((count (read))
         (input1 (read))
         (input2 (read))
         (output (read))
         (s3 (number->string count))
         (s2 (number->string input2))
         (s1 (number->string input1))
         (name "quicksort")
         (n (hide count input1))
         (r (hide count input2))
         (less? (hide count (lambda ((x :- :fixnum) (y :- :fixnum)) (fx< x y))))
         (v (make-vector n)))
    (let ()
      (declare (fixnum) (not safe))
      (do ((i 0 (+ i 1)))
          ((= i n))
        (vector-set! v i (random r))))
    (run-r7rs-benchmark
     (string-append name ":" s1 ":" s3)
     count
     (lambda () (quick-1 (vector-map identity v) less?))
     (lambda (v)
       (call-with-current-continuation
        (lambda (return)
          (do ((i 1 (+ i 1)))
              ((= i (vector-length v))
               #t)
            (unless (<= (vector-ref v (- i 1))
                        (vector-ref v i))
              (return #f)))))))))
